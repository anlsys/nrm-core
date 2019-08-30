{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : FFI.TypeUncurry
Copyright   : Copyright (c) 2018  Niklas Hambüchen.
License     : MIT License.

MessagePack FFI code adapted from call-haskell-from-anything
-}
module FFI.TypeUncurry
  ( -- | You see this because your compiler supports DataKinds.
    module FFI.TypeUncurry.DataKinds
  )
where

import FFI.TypeUncurry.DataKinds
