{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad.Primitive
import Data.Generics.Product
import Data.Sequence
import H.Prelude as R
import HBandit.Class
import HBandit.Exp4R
import Protolude
import System.Random

rrequire :: (MonadR m, Literal lib a) => lib -> m ()
rrequire p = void [r| suppressMessages(require(p_hs,character.only=TRUE)) |]

rpackages :: [Text]
rpackages = ["ggplot2", "plotly", "latex2exp"]

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
      { history :: Seq Double,
        bandit :: Exp4R () Int
      }
  deriving (Generic)

onePass ::
  (MonadState GameState m, MonadIO m) =>
  [(Double, Double, Double, Double)] ->
  m ()
onePass dataset =
  for_ dataset $ \(x, y) -> do
    g <- liftIO getStdGen
    (a, g') <- zoom (field @"bandit") $ do
      stepCtx g undefined ()
    liftIO $ setStdGen g'
    field @"history" %= (Data.Sequence.|> undefined)

plot1pass ::
  (MonadR m, MonadIO m) =>
  Double ->
  [(Double, Double, Double, Double)] ->
  m (SomeSEXP (PrimState m))
plot1pass comparator fit dataset = do
  g <- liftIO getStdGen
  (b, a, g') <- initCtx g undefined
  let (GameState (toList -> history) _) =
        execState (onePass fit dataset) $ GameState
          { history = Data.Sequence.Empty,
            bandit = b
          }
  [r|
    d <- data.frame(seq(1, length(ys_hs), 1), ys_hs, history_hs)
    names(d)=c("t","label","history")
    d$c = cumsum((d$history - d$label)^2)
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
    ys = snd <$> dataset

preamble :: (MonadR m) => m (SomeSEXP (PrimState m))
preamble =
  [r|
    set.seed(1)
    k = 3
    n = 100
    mus = pmax(0,pmin(1,rnorm(k, mean=0.5, sd=1)))
    make_arm <- function(mu) pmax(0,pmin(1,rnorm(n, mean=mu, sd=0.1)))
    d = as.data.frame(t(ldply(mus,make_arm)))
    names(d) = as.character(1:k)
    summary(d)
|]

experiment ::
  (MonadR m) =>
  m (SomeSEXP (PrimState m))
experiment = do
  one_cost <- [r| d$"1"|]
  two_cost <- [r| d$"2" |]
  one_risk <- [r| d$"1"|]
  two_risk <- [r| d$"2" |]
  let (ZipList dataset) =
        (\a b c d -> (a, b, c, d) :: (Double, Double, Double, Double))
          <$> p one_cost
          <*> p two_cost
          <*> p one_risk
          <*> p two_risk
  plot1pass 0.4 dataset
  where
    p = ZipList . fromSomeSEXP

main :: IO ()
main =
  R.withEmbeddedR defaultConfig
    $ R.runRegion
    $ do
      for_ rpackages rrequire
      [r| theme_set(theme_bw()) |]
      _ <- preamble
      (experiment >>= \x -> void $ saveTsPlot x "plotBandit" Png)
