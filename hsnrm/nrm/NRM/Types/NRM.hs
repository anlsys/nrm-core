{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.NRM
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.NRM
  ( NRM,
    App,
    execNRM,
    log,
    logInfo,
    logError,
    pub,
    rep,
    behave,
  )
where

import Control.Monad.Trans.RWS.Lazy (RWST, execRWST, tell)
import NRM.Types.Behavior
import NRM.Types.Configuration
import NRM.Types.Messaging.UpstreamPub
import NRM.Types.Messaging.UpstreamRep
import NRM.Types.State
import NRM.Types.UpstreamClient
import Protolude hiding (Rep, log)

-- |
type App st a = RWST Cfg [Behavior] st IO a

-- | The NRM monad is just a RWS.
type NRM a = App NRMState a

execNRM :: NRM a -> Cfg -> NRMState -> IO (NRMState, [Behavior])
execNRM = execRWST

-- | Perform a behavior
behave :: Behavior -> NRM ()
behave b = tell [b]

-- | NRM reply
rep :: UpstreamClientID -> Rep -> NRM ()
rep clientID rp = behave $ Rep clientID rp

-- | NRM publish
pub :: Pub -> NRM ()
pub msg = behave $ Pub msg

-- | NRM debug level log
log :: Text -> RWST Cfg [Behavior] a IO ()
log = logVerbosity Debug

-- | NRM info level log
logInfo :: Text -> RWST Cfg [Behavior] a IO ()
logInfo = logVerbosity Info

-- | NRM error level log
logError :: Text -> RWST Cfg [Behavior] a IO ()
logError = logVerbosity Error

-- | NRM verbose
logVerbosity :: DaemonVerbosity -> Text -> RWST Cfg [Behavior] a IO ()
logVerbosity vb l = verbose <$> ask
  >>= \vc -> when (vb >= vc) $ tell [Log vb l]
