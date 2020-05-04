
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
-- >  {-# LANGUAGE ScopedTypeVariables #-}
-- >  {-# LANGUAGE NoImplicitPrelude #-}
-- >  {-# LANGUAGE TemplateHaskell #-}
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

-- * Non-contextual
-- | We'll first cover the case of simple MABs that do not use context information.

-- ** Classes
--
-- | The main algorithm class for non-contextual bandits is 'Bandit'. This class gives
-- types for a basic bandit game between a learner and an environment, where the
-- learner has access to a random generator and is defined via a stateful 'step'
-- function. All non-contetual bandit algorithms in this library are instances of this.
Bandit.Class.Bandit(..)

-- *** Example instance: Epsilon-Greedy
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

-- |  Specializing this to the 'EpsGreedy' datatype on a small toy dataset:

-- | 
-- >  runOnePassEG :: StdGen -> GameState (EpsGreedy Bool) Bool Double
-- >  runOnePassEG g = onePass hyper g (getZipList $ f <$> ZipList [40, 2, 10] <*> ZipList [4, 44 ,3] )
-- >   where
-- >    f a b = \case True -> a; False -> b
-- >    hyper = EpsGreedyHyper {epsilon = 0.5, arms = Bandit.Arms [True, False]}
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

-- *** Other classes
-- | Some other, more restrictive classes are available in [Bandit.Class](Bandit-Class.html) for convenience. See for
-- example 'Bandit.Class.ParameterFreeMAB', which exposes a hyperparameter-free interface for
-- algorithms that don't need any information besides the arm count. Those instances are not necessary
-- per se, and the 'Bandit' class is always sufficient. Note that some instances make agressive use
-- of type refinement (See e.g. Bandit.Exp3.Exp3) through the 'Refined' package.
-- In particular, we are about to make use of the \(\left[0,1\right]\) interval through the 'ZeroOne'
-- type alias.
,Bandit.Types.ZeroOne

-- ** Algorithm comparison
-- | This subsection runs bandit experiments on an example dataset with some of the instances for 'Bandit.Bandit'.
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
-- >  greedy :: [[Double]] -> StdGen -> Double -> GameState (EpsGreedy Int) Int (Double)
-- >  greedy dataset g eps =  
-- >    onePass
-- >      (EpsGreedyHyper {epsilon = eps, arms = Bandit.Arms [0..2]})
-- >      g
-- >      (toAdversary dataset)
-- >  
-- >  simulation :: Int -> IO ([Int],[Int],[Double],[Double],[Double])
-- >  simulation seed = do
-- >    dataset <- generateGaussianData tmax (unsafeRefine <$> [0.1, 0.5, 0.6])
-- >    return ([1 .. tmax], Protolude.replicate tmax seed, greedy05 dataset, greedy03 dataset, exp3pf dataset)
-- >   where tmax = 400 
-- >         g = mkStdGen seed
-- >         greedy05 :: [[Double]] -> [Double]
-- >         greedy05 dataset = extract $ greedy dataset g 0.5
-- >         greedy03 :: [[Double]] -> [Double]
-- >         greedy03 dataset = extract $ greedy dataset g 0.3
-- >         exp3pf :: [[Double]] -> [Double]
-- >         exp3pf dataset = fmap unrefine . extract $ exp3 dataset g
-- >         extract = Protolude.toList . Sequence.reverse . historyLosses
-- >  
-- >  newtype Reducer = Reducer {getReducer :: ([Int],[Int],[Double],[Double],[Double])}
-- >  
-- >  instance Semigroup Reducer where
-- >    (getReducer -> (a,b,c,d,e)) <> (getReducer -> (a',b',c',d',e')) = Reducer (a<>a',b<>b',c<>c',d<>d',e<>e')
-- >  
-- >  instance Monoid Reducer where
-- >    mempty = Reducer ([],[],[],[],[])

-- |
-- >>>  results <- forM ([2..10] ::[Int]) simulation
-- >>>  let exported = T.unpack $ T.decodeUtf8 $ encode $ getReducer $ mconcat (Reducer <$> results)
-- >>>  [r|
-- >>>    data.frame(t(jsonlite::fromJSON(exported_hs))) %>%
-- >>>      summary %>%
-- >>>      print
-- >>>  |]
-- $expe

-- |
-- >>>  [r| data.frame(t(jsonlite::fromJSON(exported_hs))) %>%
-- >>>        rename(t = X1, iteration = X2, greedy05= X3, greedy03=X4, exp3=X5 ) %>%
-- >>>        gather("strategy", "loss", -t, -iteration) %>%
-- >>>        mutate(strategy=factor(strategy)) %>% 
-- >>>        group_by(strategy,iteration) %>%
-- >>>        mutate(regret = cumsum(loss-0.1)) %>% 
-- >>>        ungroup() %>%
-- >>>        ggplot(., aes(t, regret, color=strategy, group=interaction(strategy, iteration))) +
-- >>>          geom_line() + ylab("External Regret")
-- >>>  |]
-- $regretPlot

  ) where
import Bandit.Class
import Bandit.Types
import Bandit.EpsGreedy

--   pass

-- > Resolving dependencies...
-- > Build profile: -w ghc-8.6.5 -O1
-- > In order, the following will be built (use -v for more details):
-- >  - hbandit-1.0.0 (lib) (file src/Bandit.hs changed)
-- >  - fake-package-0 (exe:script) (configuration changed)
-- > Preprocessing library for hbandit-1.0.0..
-- > Building library for hbandit-1.0.0..
-- > [8 of 9] Compiling Bandit.Tutorial  ( src/Bandit/Tutorial.hs, /home/fre/workspace/hbandit/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hbandit-1.0.0/build/Bandit/Tutorial.o )
-- > [9 of 9] Compiling Bandit           ( src/Bandit.hs, /home/fre/workspace/hbandit/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hbandit-1.0.0/build/Bandit.o )
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
-- >  Min.   :0.00000   Min.   :0.1141   Min.   :0.3602  
-- >  1st Qu.:0.02809   1st Qu.:0.4279   1st Qu.:0.5433  
-- >  Median :0.10136   Median :0.4959   Median :0.6043  
-- >  Mean   :0.11087   Mean   :0.4985   Mean   :0.6072  
-- >  3rd Qu.:0.17558   3rd Qu.:0.5647   3rd Qu.:0.6685  
-- >  Max.   :0.38586   Max.   :0.7899   Max.   :0.8843  
-- $summaryPlot
-- <<literate/summaryPlot.png>>
-- $expe
-- >        X1              X2           X3                X4        
-- >  Min.   :  1.0   Min.   : 2   Min.   :0.00000   Min.   :0.0000  
-- >  1st Qu.:100.8   1st Qu.: 4   1st Qu.:0.06609   1st Qu.:0.0498  
-- >  Median :200.5   Median : 6   Median :0.16996   Median :0.1322  
-- >  Mean   :200.5   Mean   : 6   Mean   :0.25850   Mean   :0.1935  
-- >  3rd Qu.:300.2   3rd Qu.: 8   3rd Qu.:0.47440   3rd Qu.:0.2517  
-- >  Max.   :400.0   Max.   :10   Max.   :0.97830   Max.   :0.8723  
-- >        X5         
-- >  Min.   :0.00000  
-- >  1st Qu.:0.04174  
-- >  Median :0.11186  
-- >  Mean   :0.14680  
-- >  3rd Qu.:0.19227  
-- >  Max.   :0.80434  
-- $regretPlot
-- <<literate/regretPlot.png>>

