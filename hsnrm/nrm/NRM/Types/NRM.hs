{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.NRM
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.NRM
  ( NRM,
    execNRM,
    log,
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

-- | The NRM monad is just a RWS.
type NRM = RWST Cfg [Behavior] NRMState IO ()

execNRM :: NRM -> Cfg -> NRMState -> IO (NRMState, [Behavior])
execNRM = execRWST

-- | Perform a behavior
behave :: Behavior -> NRM
behave b = tell [b]

-- | NRM reply
rep :: UpstreamClientID -> Rep -> NRM
rep clientID rp = behave $ Rep clientID rp

-- | NRM publish
pub :: Pub -> NRM
pub msg = behave $ Pub msg

-- | NRM log
log :: Text -> RWST Cfg [Behavior] a IO ()
log l = tell [Log l]
