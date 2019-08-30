{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Actuator
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Actuator
  ( Actuator (..)
  )
where

{-import Data.Aeson-}
{-import Data.JSON.Schema-}
{-import Data.MessagePack-}
{-import NRM.Classes.Messaging-}
{-import Protolude-}
data Actuator a = Actuator a
