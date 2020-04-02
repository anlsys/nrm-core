-- |
-- Module      : NRM.Messaging
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Messaging
  ( libnrmVars,
    ratelimitEnvVar,
    cmdIDEnvVar,
  )
where

import Data.Default
import NRM.Types.Configuration
import NRM.Types.Manifest
import NRM.Types.Units
import Protolude
import Refined
import Refined.Unsafe

data V = Str Text | LLPos (Refined Positive Int)

ratelimitEnvVar :: Text
ratelimitEnvVar = "NRM_RATELIMIT"

cmdIDEnvVar :: Text
cmdIDEnvVar = "NRM_CMDID"

libnrmVars :: Text
libnrmVars =
  mconcat . intersperse "\n" $
    toHeader
      <$> [ ("NRM_DEFAULT_URI", Str (downstreamBindAddress (def :: DownstreamCfg))),
            ("NRM_ENV_URI", Str "NRM_DOWNSTREAM_EVENT_URI"),
            ("NRM_ENV_CMDID", Str cmdIDEnvVar),
            ("NRM_ENV_RATELIMIT", Str ratelimitEnvVar),
            ("NRM_ENV_TRANSMIT", Str "NRM_TRANSMIT"),
            ( "NRM_DEFAULT_RATELIMIT_THRESHOLD",
              LLPos (unsafeRefine . floor . fromHz $ ratelimit (def :: Instrumentation))
            )
          ]

toHeader :: (Text, V) -> Text
toHeader (t, Str x) = "#define " <> t <> " " <> show x
toHeader (t, LLPos x) = "#define " <> t <> " (" <> show (unrefine x) <> "LL)"
