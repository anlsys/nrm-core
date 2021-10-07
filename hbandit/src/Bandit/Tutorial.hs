
{-| This module serves as an introduction to the `hbandit` Multi-Armed Bandit library.
-}

module Bandit.Tutorial (
-- *** Setup

-- | The code snippets displayed in this tutorial require the following list of extensions and modules.

-- |
-- >  {-# LANGUAGE LambdaCase #-}
-- >  {-# LANGUAGE FlexibleContexts #-}
-- >  {-# LANGUAGE DeriveGeneric #-}
-- >  {-# LANGUAGE OverloadedStrings #-}
-- >  {-# LANGUAGE ViewPatterns #-}
-- >  {-# LANGUAGE OverloadedLists #-}
-- >  {-# LANGUAGE OverloadedLabels #-}
-- >  {-# LANGUAGE DataKinds #-}
-- >  {-# LANGUAGE FlexibleInstances  #-}
-- >  {-# LANGUAGE ScopedTypeVariables #-}
-- >  {-# LANGUAGE StandaloneDeriving #-}
-- >  {-# LANGUAGE NoImplicitPrelude #-}
-- >  {-# LANGUAGE TemplateHaskell #-}
-- >  {-# LANGUAGE DeriveAnyClass #-}
-- >  {-# LANGUAGE RecordWildCards #-}
-- >  {-# LANGUAGE QuasiQuotes #-}
-- >  import Protolude
-- >  import Text.Pretty.Simple
-- >  import System.Random
-- >  import Data.List.NonEmpty as NonEmpty hiding (init)
-- >  import Refined hiding (NonEmpty)
-- >  import Refined.Unsafe
-- >  import Data.Sequence as Sequence
-- >  import Data.Generics.Product
-- >  import Prelude ((!!))
-- >  import Data.Coerce
-- >  import Data.Generics.Labels
-- >  import Data.Functor.Compose
-- >  import H.Prelude.Interactive
-- >  import System.IO hiding (print)
-- >  import Control.Monad.Primitive
-- >  import qualified Language.R.Instance as R
-- >  import Control.Lens
-- >  import Bandit
-- >  import Bandit.EpsGreedy
-- >  import Bandit.Exp3
-- >  import qualified Data.Text.Lazy.Encoding as T
-- >  import qualified Data.Text.Lazy as T
-- >  import Data.Aeson hiding ((.=))

-- 
-- print' :: (Show a) => a -> IO ()
-- print' a = Protolude.putText $ "@" <> toS (pShowNoColor a) <> "@"
-- 
-- putText' :: Text -> IO ()
-- putText' t = Protolude.putText $ "@" <> t <> "@"
-- 
-- rrequire :: (MonadR m, Literal lib a) => lib -> m ()
-- rrequire p = void [r| suppressMessages(require(p_hs,character.only=TRUE)) |]
-- 
-- rpackages :: [Text]
-- rpackages = [ "svglite", "dplyr", "tidyr", "purrr", "ggplot2" , "jsonlite"]
-- 
-- main :: IO ()
-- main = do
--   R.initialize R.defaultConfig
--   for_ rpackages rrequire
--   [r| theme_set(
--       theme_bw() +
--       theme(
--         panel.background = element_rect(fill = "transparent"), 
--         plot.background = element_rect(fill = "transparent", color = NA), 
--         legend.background = element_rect(fill = "transparent"), 
--         legend.box.background = element_rect(fill = "transparent") 
--       )) |]

-- | This tutorial only covers non-contextual bandit algorithms.

-- ** Classes
--
-- *** Non-contextual
--
-- | The algorithm class for non-contextual bandits is 'Bandit'. This class gives
-- types for a basic bandit game between a learner and an environment, where the
-- learner has access to a random generator and is defined via a stateful 'step'
-- function.
Bandit.Class.Bandit(..)

-- **** example instance: Epsilon-Greedy
--
-- | Let's take a look at the instance for the classic fixed-rate \(\epsilon\)-Greedy
-- algorithm. The necessary hyperparameters are the number of arms and the rate value,
-- as the 'EpsGreedyHyper' datatype shows.
,Bandit.EpsGreedy.EpsGreedyHyper(..)

-- | Let's use that instance on some toy data with a few rounds.
--
-- First, we define the @onePass@ function that takes a deterministic oblivious adversary
-- (represented as a list of  of @ a->l @), an initial random generator for the bandit,
-- a hyperparameter, and runs the bandit game on all the iterations of the adversary
-- to produces a history of lossses and actions:

-- | 
-- >  data GameState b a l
-- >    = GameState
-- >        { historyActions :: NonEmpty a,
-- >          historyLosses :: Seq l,
-- >          bandit :: b,
-- >          stdGen :: StdGen
-- >        }
-- >    deriving (Generic, Show)
-- >  
-- >  onePass :: (Bandit b hyper a l) =>
-- >    hyper ->       -- ^ hyperparameter
-- >    StdGen ->      -- ^ random generator initial value
-- >    [(a -> l)] ->  -- ^ oblivious deterministic adversary
-- >    GameState b a l
-- >  onePass hyper g adversary = runGame initialGame
-- >   where
-- >    (initialBanditState, initialAction, g') = Bandit.init g hyper
-- >    initialGame =  GameState
-- >      { historyActions = [initialAction],
-- >        historyLosses = [],
-- >        bandit = initialBanditState,
-- >        stdGen = g'
-- >      }
-- >    runGame = execState game
-- >    game = for_ adversary iteration
-- >    iteration actionToLoss = do
-- >      (actionToLoss . NonEmpty.head -> loss) <- use #historyActions
-- >      oldGen <- use #stdGen
-- >      (action, newGen) <- zoom #bandit $ step oldGen loss
-- >      #stdGen .= newGen
-- >      #historyActions %= (action NonEmpty.<|)
-- >      #historyLosses %= (loss Sequence.<|)

-- |  Specializing this to the 'EpsGreedy' datatype on a small toy dataset, using a fixed rate:

-- | 
-- >  runOnePassEG :: StdGen -> GameState (EpsGreedy Bool FixedRate) Bool Double
-- >  runOnePassEG g = onePass hyper g (getZipList $ f <$> ZipList [40, 2, 10] <*> ZipList [4, 44 ,3] )
-- >   where
-- >    f a b = \case True -> a; False -> b
-- >    hyper = EpsGreedyHyper {rateRep = (FixedRate 0.5), arms = Bandit.Arms [True, False]}
-- >  
-- >  printOnePassEG :: IO ()
-- >  printOnePassEG = putText $
-- >    "Action series:" <> 
-- >    show  (historyActions gs ^.. traversed) <>
-- >    "\nLoss series:" <> 
-- >    show ( historyLosses gs ^.. traversed)
-- >   where gs = runOnePassEG (mkStdGen 1)

-- |
-- >>>  printOnePassEG
-- $eg

-- *** Contextual 
--
-- | The algorithm class for contextual bandits is 'ContextualBandit'. This class gives
-- types for a bandit game between a learner and an environment with context, where the
-- learner has access to a random generator and is defined via a stateful 'step'
-- function.

, Bandit.Class.ContextualBandit(..)

-- | The 'ExpertRepresentation' class is used to encode experts.

, Bandit.Class.ExpertRepresentation(..)

-- ** Non-contextual algorithm comparison
-- | This subsection runs bandit experiments on an example dataset with some of the @Bandit@ instances.
-- The data for this tutorial is generated in R using the [inline-r](https://hackagehaskell.org/package/inline-r) package.
-- Let's define a simple problem with three gaussian arms. We will threshold all cost values to \(\left[0,1\right]\).

-- | 
-- >  generateGaussianData ::
-- >    Int ->                -- ^ number of rounds
-- >    [ZeroOne Double] ->   -- ^ arm averages
-- >    IO [[Double]]         -- ^ dataset
-- >  generateGaussianData (fromInteger . toInteger -> n :: Double) avgs = 
-- >    (mapM generate (unrefine <$> avgs ))
-- >    where
-- >      generate :: (MonadR m, Functor m) => Double -> m [Double]
-- >      generate mu = gen01TS mu <&> fromSomeSEXP
-- >      gen01TS :: (MonadR m) => Double -> m (SomeSEXP (PrimState m))
-- >      gen01TS mu = [r| pmax(0,pmin(1,rnorm(n_hs, mean=mu_hs, sd=0.1))) |]
-- >  
-- >  refineDataset ::  [[Double]] -> [[ZeroOne Double]]
-- >  refineDataset = (fmap.fmap) unsafeRefine

-- Let's generate data for a 3 arm problem and observe the distribution of costs.

-- |
-- >>>  dataset <- generateGaussianData 400 (unsafeRefine <$> [0.1, 0.5, 0.6])
-- >>>  let d :: Text
-- >>>      d = show $ Protolude.transpose dataset
-- >>>  [r| print(summary(jsonlite::fromJSON(d_hs))) |]
-- $summaryProblem

-- |
-- >>>  [r| 
-- >>>    data <- as.data.frame(jsonlite::fromJSON(d_hs))
-- >>>    data_mutated = data %>% gather("arm", "cost", 1:ncol(data))
-- >>>    ggplot(data_mutated, aes(arm, cost, group=factor(arm)))+ geom_boxplot()
-- >>>  |]
-- $summaryPlot

-- | Here is helper that convert to the @[action->loss]@ adversary format:

-- | 
-- >  toAdversary :: [[a]] -> [Int -> a]
-- >  toAdversary xss = Protolude.transpose xss <&> listToAdversary
-- >   where
-- >    listToAdversary :: [a] -> Int -> a
-- >    listToAdversary l i = l Prelude.!! i

-- | Let's define some experiments:

-- | 
-- >  exp3 :: [[Double]] -> StdGen -> GameState (Exp3 Int) Int (ZeroOne Double)
-- >  exp3 dataset g = 
-- >    onePass
-- >      (Bandit.Arms [0..2])
-- >      g
-- >      (toAdversary $ refineDataset dataset)
-- >                   
-- >  greedy :: (Rate r) => [[Double]] -> StdGen -> r -> GameState (EpsGreedy Int r) Int (Double)
-- >  greedy dataset g r =  
-- >    onePass
-- >      (EpsGreedyHyper {rateRep = r, arms = Bandit.Arms [0..2]})
-- >      g
-- >      (toAdversary dataset)
-- >  
-- >  data SimResult t = SimResult {
-- >    t :: t Int,
-- >    seed :: t Int,
-- >    greedy05 :: t Double,
-- >    greedy03 :: t Double,
-- >    greedysqrt05 :: t Double,
-- >    exp3pf :: t Double
-- >  } deriving (Generic)
-- >  
-- >  simulation :: Int -> Int -> IO (SimResult [])
-- >  simulation tmax seed@(mkStdGen -> g) = do
-- >    dataset <- generateGaussianData tmax (unsafeRefine <$> [0.1, 0.5, 0.6])
-- >    return $ SimResult {
-- >               t = [1 .. tmax],
-- >               seed = Protolude.replicate tmax seed,
-- >               greedy05 = extract $ greedy dataset g (FixedRate 0.5),
-- >               greedy03 = extract $ greedy dataset g (FixedRate 0.3),
-- >               greedysqrt05 = extract $ greedy dataset g (InverseSqrtRate 0.5),
-- >               exp3pf = fmap unrefine . extract $ exp3 dataset g
-- >             }
-- >   where 
-- >     extract = Protolude.toList . Sequence.reverse . historyLosses
-- >  
-- >  instance Semigroup (SimResult []) where
-- >    x <> y = SimResult {
-- >               t = f Main.t,
-- >               seed = f seed, 
-- >               greedy05 = f greedy05, 
-- >               greedy03 = f greedy03, 
-- >               greedysqrt05 = f greedysqrt05,
-- >               exp3pf = f exp3pf
-- >             }
-- >             where f accessor = accessor x <> accessor y
-- >  
-- >  instance Monoid (SimResult []) where
-- >    mempty = SimResult mempty mempty mempty mempty mempty mempty
-- >  
-- >  instance ToJSON (SimResult []) where
-- >    toJSON SimResult{..} = 
-- >      toJSON (t, seed, greedy05, greedy03, greedysqrt05, exp3pf)

-- |
-- >>>  results <- forM ([2..10] ::[Int]) (simulation 400)
-- >>>  let exported = T.unpack $ T.decodeUtf8 $ encode $ mconcat results
-- >>>  [r| data.frame(t(jsonlite::fromJSON(exported_hs))) %>%
-- >>>        rename(t = X1, iteration = X2, greedy05= X3, greedy03=X4, greedysqrt05=X5,exp3=X6 ) %>%
-- >>>        gather("strategy", "loss", -t, -iteration) %>%
-- >>>        mutate(strategy=factor(strategy)) %>% 
-- >>>        group_by(strategy,iteration) %>%
-- >>>        mutate(regret = cumsum(loss-0.1)) %>% 
-- >>>        ungroup() %>%
-- >>>        ggplot(., aes(t, regret, color=strategy, group=interaction(strategy, iteration))) +
-- >>>          geom_line(alpha=0.5) + ylab("External Regret")
-- >>>  |]
-- $regretPlot

  ) where
import Bandit.Class
import Bandit.EpsGreedy

--   pass

-- > Resolving dependencies...
-- > Build profile: -w ghc-8.6.5 -O1
-- > In order, the following will be built (use -v for more details):
-- >  - hbandit-1.0.0 (lib) (configuration changed)
-- >  - fake-package-0 (exe:script) (configuration changed)
-- > Configuring library for hbandit-1.0.0..
-- > Preprocessing library for hbandit-1.0.0..
-- > Building library for hbandit-1.0.0..
-- > [8 of 8] Compiling Bandit.Tutorial  ( src/Bandit/Tutorial.hs, /home/fre/workspace/hbandit/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hbandit-1.0.0/build/Bandit/Tutorial.o )
-- > Configuring executable 'script' for fake-package-0..
-- > Preprocessing executable 'script' for fake-package-0..
-- > Building executable 'script' for fake-package-0..
-- > [1 of 1] Compiling Main             ( Main.hs, /home/fre/workspace/hbandit/dist-newstyle/build/x86_64-linux/ghc-8.6.5/fake-package-0/x/script/build/script/script-tmp/Main.o )
-- > Linking /home/fre/workspace/hbandit/dist-newstyle/build/x86_64-linux/ghc-8.6.5/fake-package-0/x/script/build/script/script ...
-- $eg
-- > Action series:[True,True,False,True]
-- > Loss series:[10.0,44.0,40.0]
-- $summaryProblem
-- >        V1                V2               V3        
-- >  Min.   :0.00000   Min.   :0.2499   Min.   :0.2949  
-- >  1st Qu.:0.02184   1st Qu.:0.4302   1st Qu.:0.5201  
-- >  Median :0.10437   Median :0.5018   Median :0.5891  
-- >  Mean   :0.10661   Mean   :0.5029   Mean   :0.5888  
-- >  3rd Qu.:0.16927   3rd Qu.:0.5718   3rd Qu.:0.6598  
-- >  Max.   :0.33667   Max.   :0.7510   Max.   :0.8877  
-- $summaryPlot
-- <<literate/summaryPlot.png>>
-- $regretPlot
-- <<literate/regretPlot.png>>

