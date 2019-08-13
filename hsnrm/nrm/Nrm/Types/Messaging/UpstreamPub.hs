{-|
Module      : Nrm.Types.Messaging.UpstreamPub
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamPub
  ( Pub (..)
  )
where

{-import qualified Data.Aeson as A-}
{-import qualified Data.JSON.Schema as S-}
{-import Generics.Generic.Aeson-}
import Nrm.Classes.Messaging
import qualified Nrm.Types.Messaging.UpstreamPub.JSON as J
import Protolude

data Pub
  = Power
      { total :: Double
      , limit :: Double
      }
  | ContainerStart
      { container_uuid :: Text
      , errno :: Int
      , power :: Text
      }
  | ContainerExit
      { container_uuid :: Text
      , profile_data :: Map Text Text
      }
  | Performance
      { container_uuid :: Text
      , payload :: Int
      }
  | Progress
      { application_uuid :: Text
      , payload :: Int
      }
  | Control
      { powercap :: Int
      , energy :: Double
      , performance :: Double
      , control_time :: Double
      , feedback_time :: Double
      }
  deriving (Generic)

{-instance NrmMessage Pub where-}

  {-decode = fmap fromJ . A.decode . toS-}

  {-encode = toS . A.encode . toJ-}

  {-schema _ = S.schema (Proxy :: Proxy J.Pub)-}

{-fromJ :: J.Pub -> Pub-}
{-fromJ = undefined-}

{-toJ :: Pub -> J.Pub-}
{-toJ = undefined-}
{-instance JSONLayer UpstreamPub-}
