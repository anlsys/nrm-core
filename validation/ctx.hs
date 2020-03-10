{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Control.Monad.Primitive
import Data.Generics.Product
import Data.Sequence
import H.Prelude as R
import HBandit.Class
import HBandit.Exp4R
import HBandit.Types
import Protolude
import Refined.Unsafe
import System.Random

type ZO = ZeroOne Double

rrequire :: (MonadR m, Literal lib a) => lib -> m ()
rrequire p = void [r| suppressMessages(require(p_hs,character.only=TRUE)) |]

rpackages :: [Text]
rpackages = ["ggplot2", "plyr", "plotly", "latex2exp"]

data OutputType = Plotly | Pdf | Png
  deriving (Read, Show, Enum, Bounded)

saveTsPlot ::
  (MonadR m, Literal ggplot a) =>
  ggplot ->
  Text ->
  OutputType ->
  m (SomeSEXP (PrimState m))
saveTsPlot ggplot path = \case
  Plotly -> rggplotly ggplot >>= ras_widget >>= rsaveWidget (path <> ".html")
  Pdf -> ggsave (path <> ".pdf") ggplot
  Png -> ggsave (path <> ".png") ggplot
  where
    ggsave pa pl = [r| ggsave(pa_hs,pl_hs, width = 6, height = 3) |]
    rggplotly x = [r| ggplotly(x_hs) |]
    ras_widget x = [r| as_widget(x_hs) |]
    rsaveWidget p x = [r| htmlwidgets::saveWidget(x_hs,p_hs) |]

data GameState
  = GameState
      { historyActions :: Seq Int,
        historyCosts :: Seq Double,
        historyConstraints :: Seq Double,
        bandit :: Exp4R () Int
      }
  deriving (Generic)

onePass ::
  ( MonadState GameState m,
    MonadIO m,
    (Functor (Zoomed m' (Int, StdGen))),
    (Zoom m' m (Exp4R () Int) GameState)
  ) =>
  [(ZO, ZO, ZO, ZO)] ->
  m ()
onePass dataset =
  for_ dataset $ \(c1, c2, r1, r2) -> do
    oldA <- use (field @"bandit" . field @"lastAction")
    let (c, r) = if oldA == 1 then (c1, r1) else (c2, r2)
    g <- liftIO getStdGen
    (a, g') <- zoom (field @"bandit") $ stepCtx g (Feedback c r) ()
    liftIO $ setStdGen g'
    field @"historyActions" %= (Data.Sequence.|> a)
    field @"historyCosts" %= (Data.Sequence.|> unrefine c)
    field @"historyConstraints" %= (Data.Sequence.|> unrefine r)

plot1pass ::
  (MonadR m, MonadIO m) =>
  Double ->
  [Double] ->
  [Double] ->
  [Double] ->
  [Double] ->
  m (SomeSEXP (PrimState m))
plot1pass comparator one_cost two_cost one_risk two_risk = do
  g <- liftIO getStdGen
  let (b, a, g') = initCtx g Exp4RCfg
        { expertsCfg = expertsC,
          constraintCfg = unsafeRefine 0.4,
          horizonCfg = unsafeRefine 100,
          as = 1 :| [2]
        }
  ( GameState
      ((fmap fromIntegral) . toList -> hActions :: [Double])
      (toList -> hCosts :: [Double])
      (toList -> hConstraints :: [Double])
      _
    ) <- execStateT (onePass dataset) $
    GameState
      { historyActions = Data.Sequence.empty,
        historyCosts = Data.Sequence.empty,
        historyConstraints = Data.Sequence.empty,
        bandit = b
      }
  [r|
    d <- data.frame(seq(1, length(historyActions_hs), 1),
                    hActions_hs,
                    hCosts_hs,
                    hConstraints_hs,
                    one_cost_hs,
                    two_cost_hs,
                    one_risk_hs,
                    two_risk_hs)
    names(d)=c("t","action","c1", "c2", "r1","r2")
    print(summary(d))
    d$best = min(mean(d$c1),mean(d$c2))
    print(head(d))
    ggplot(d,aes(x = t, y = (cumsum((history-label)^2))/t)) +
      geom_point() +
      ylim(c(0,max(d$c/d$t))) +
      geom_hline(yintercept = comparator_hs, linetype="dashed",
                color = "red") +
      xlab(TeX("t")) +
      ylab(TeX("\\frac{1}{t} \\sum_{i=1}^{i=t} c^i (x^i)"))
  |]
  where
    expertsC :: NonEmpty (() -> NonEmpty (ZeroOne Double, Int))
    expertsC = expert1 :| [expert2]
    expert1 () = [(HBandit.Types.one, 1 :: Int), (HBandit.Types.zero, 2 :: Int)]
    expert2 () = [(HBandit.Types.one, 2 :: Int), (HBandit.Types.zero, 1 :: Int)]
    p = ZipList . fmap unsafeRefine
    (ZipList dataset) =
      (\a b c d -> (a, b, c, d) :: (ZO, ZO, ZO, ZO))
        <$> p one_cost
        <*> p two_cost
        <*> p one_risk
        <*> p two_risk

experiment ::
  (MonadR m) =>
  Double ->
  m (SomeSEXP (PrimState m))
experiment n = do
  one_cost <- gen01TS <&> fromSomeSEXP
  two_cost <- gen01TS <&> fromSomeSEXP
  one_risk <- gen01TS <&> fromSomeSEXP
  two_risk <- gen01TS <&> fromSomeSEXP
  plot1pass 0.4 one_cost two_cost one_risk two_risk
  where
    gen01TS =
      [r| mu = pmax(0,pmin(1,rnorm(1, mean=0.5, sd=1)))
          pmax(0,pmin(1,rnorm(n_hs, mean=mu, sd=0.1))) |]

main :: IO ()
main =
  R.withEmbeddedR defaultConfig
    $ R.runRegion
    $ do
      for_ rpackages rrequire
      [r| theme_set(theme_bw()) |]
      (experiment 1000 >>= \x -> void $ saveTsPlot x "plotBandit" Png)
