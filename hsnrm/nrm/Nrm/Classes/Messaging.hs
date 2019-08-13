{-|
Module      : Nrm.Classes.Messaging
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Classes.Messaging
  ( NrmMessage(..)
  )
where

import Protolude

class NrmMessage a where

  encode :: a -> ByteString

  decode :: ByteString -> Maybe a
