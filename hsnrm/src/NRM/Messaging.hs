-- |
-- Module      : NRM.Messaging
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Messaging
  ( libnrmVars,
    ratelimitEnvVar,
    cmdIDEnvVar,
  )
where

import Data.Default
import NRM.Types.Configuration
import Protolude

ratelimitEnvVar :: Text
ratelimitEnvVar = "NRM_RATELIMIT"

cmdIDEnvVar :: Text
cmdIDEnvVar = "NRM_CMDID"

libnrmVars :: Text
libnrmVars =
  mconcat . intersperse "\n" $
    toHeader
      <$> [ ("NRM_DEFAULT_URI", show (downstreamBindAddress $ downstreamCfg (def :: Cfg))),
            ("NRM_ENV_URI", show ("NRM_DOWNSTREAM_EVENT_URI" :: Text)),
            ("NRM_ENV_CMDID", show cmdIDEnvVar),
            ("NRM_ENV_RATELIMIT", show ratelimitEnvVar),
            ("NRM_ENV_TRANSMIT", show ("NRM_TRANSMIT" :: Text)),
            ("NRM_DEFAULT_RATELIMIT_THRESHOLD", "(10000000LL)")
          ]

toHeader :: (Text, Text) -> Text
toHeader (t, x) = "#define " <> t <> " " <> x
