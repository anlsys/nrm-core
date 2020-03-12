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
import HBandit.Types as HBT
import Protolude
import Refined hiding (NonEmpty)
import Refined.Unsafe
import System.Random

type ZO = ZeroOne Double

rrequire :: (MonadR m, Literal lib a) => lib -> m ()
rrequire p = void [r| suppressMessages(require(p_hs,character.only=TRUE)) |]

rpackages :: [Text]
rpackages = ["ggplot2", "plyr", "plotly"]

data OutputType = Plotly | Pdf | Png
  deriving (Read, Show, Enum, Bounded)

data GameState
  = GameState
      { historyActions :: Seq Int,
        historyCosts :: Seq Double,
        historyConstraints :: Seq Double,
        bandit :: Exp4R () Int (ObliviousRep Int)
      }
  deriving (Generic)

onePass ::
  ( MonadState GameState m,
    MonadIO m,
    (Functor (Zoomed m' (Int, StdGen))),
    (Zoom m' m (Exp4R () Int (ObliviousRep Int)) GameState)
  ) =>
  [(ZO, ZO, ZO, ZO, ZO, ZO)] ->
  m ()
onePass dataset =
  for_ dataset $ \(c1, c2, c3, r1, r2, r3) ->
    use (field @"bandit" . field @"lastAction") >>= \case
      Just (LastAction a _ _) -> do
        let (c, r) = case a of
              1 -> (c1, r1)
              2 -> (c2, r2)
              3 -> (c3, r3)
        g <- liftIO getStdGen
        (a', g') <- zoom (field @"bandit") $ stepCtx g (Just $ Feedback c r) ()
        liftIO $ setStdGen g'
        field @"historyActions" %= (Data.Sequence.|> a')
        field @"historyCosts" %= (Data.Sequence.|> unrefine c)
        field @"historyConstraints" %= (Data.Sequence.|> unrefine r)
      Nothing -> do
        g <- liftIO getStdGen
        (a', g') <- zoom (field @"bandit") $ stepCtx g Nothing ()
        liftIO $ setStdGen g'

plot1pass ::
  (MonadR m, MonadIO m) =>
  [Double] ->
  [Double] ->
  [Double] ->
  [Double] ->
  [Double] ->
  [Double] ->
  m (SomeSEXP (PrimState m))
plot1pass one_cost two_cost three_cost one_risk two_risk three_risk = do
  let b = initCtx $ Exp4RCfg
        { expertsCfg = expertsC,
          constraintCfg = unsafeRefine 0.5,
          horizonCfg = unsafeRefine 500000,
          as = [1, 2, 3]
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
    print(length(hActions_hs))
    print(length(hCosts_hs))
    print(length(one_cost_hs))
    d <- data.frame(seq(1, length(hActions_hs), 1),
                    hActions_hs,
                    hCosts_hs,
                    hConstraints_hs,
                    one_cost_hs[-1],
                    two_cost_hs[-1],
                    three_cost_hs[-1],
                    one_risk_hs[-1],
                    two_risk_hs[-1],
                    three_risk_hs[-1])
    names(d)=c("t","action","cost","risk","c1", "c2","c3", "r1","r2","r3")
    print(summary(d))

    best = min(mean(d$c1),mean(d$c2),mean(d$c3))

    costPlot <- ggplot(d,aes(x = t, y = cumsum(cost)/t)) +
                  ylab("average cost") +
                  geom_line() +
                  geom_hline(yintercept = 0.3, linetype="dashed", color = "black") +
                  geom_hline(yintercept = 0.7, linetype="dashed", color = "black") +
                  geom_hline(yintercept = 0.5, linetype="dashed", color = "green")
    riskPlot <- ggplot(d,aes(x = t, y = cumsum(risk)/t)) +
                  ylab("average risk") +
                  geom_line() +
                  geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +
                  geom_hline(yintercept = 0.9, linetype="dashed", color = "black") +
                  geom_hline(yintercept = 0.3, linetype="dashed", color = "black") +
                  geom_hline(yintercept = 0.4, linetype="dashed", color = "green")
    ggsave("cost.pdf", costPlot)
    ggsave("risk.pdf", riskPlot)
  |]
  where
    expertsC :: NonEmpty (ObliviousRep Int)
    expertsC =
      [ ObliviousRep [(HBT.one, 1 :: Int), (HBT.zero, 2 :: Int), (HBT.zero, 3 :: Int)],
        ObliviousRep [(HBT.zero, 1 :: Int), (HBT.one, 2 :: Int), (HBT.zero, 3 :: Int)],
        ObliviousRep [(HBT.zero, 1 :: Int), (HBT.zero, 2 :: Int), (HBT.one, 3 :: Int)],
        ObliviousRep [(HBT.zero, 1 :: Int), (unsafeRefine 0.5, 2 :: Int), (unsafeRefine 0.5, 3 :: Int)],
        ObliviousRep [(unsafeRefine 0.5, 1 :: Int), (HBT.zero, 2 :: Int), (unsafeRefine 0.5, 3 :: Int)],
        ObliviousRep [(unsafeRefine 0.5, 1 :: Int), (unsafeRefine 0.5, 2 :: Int), (HBT.zero, 3 :: Int)]
      ]
    p = ZipList . fmap unsafeRefine
    (ZipList dataset) =
      (\a b c d e f -> (a, b, c, d, e, f) :: (ZO, ZO, ZO, ZO, ZO, ZO))
        <$> p one_cost
        <*> p two_cost
        <*> p three_cost
        <*> p one_risk
        <*> p two_risk
        <*> p three_risk

experiment ::
  (MonadR m) =>
  Double ->
  m (SomeSEXP (PrimState m))
experiment n = do
  one_cost <- gen01TS 0.3 <&> fromSomeSEXP
  two_cost <- gen01TS 0.7 <&> fromSomeSEXP
  three_cost <- gen01TS 0.5 <&> fromSomeSEXP
  one_risk <- gen01TS 0.9 <&> fromSomeSEXP
  two_risk <- gen01TS 0.3 <&> fromSomeSEXP
  three_risk <- gen01TS 0.4 <&> fromSomeSEXP
  plot1pass one_cost two_cost three_cost one_risk two_risk three_risk
  where
    gen01TS :: (MonadR m) => Double -> m (SomeSEXP (PrimState m))
    gen01TS mu =
      [r| pmax(0,pmin(1,rnorm(n_hs, mean=mu_hs, sd=0.1))) |]

main :: IO ()
main =
  R.withEmbeddedR defaultConfig
    $ R.runRegion
    $ do
      for_ rpackages rrequire
      [r| theme_set(theme_bw()) |]
      void $ experiment 500000
