{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : FFI.TypeUncurry.Msgpack
-- Copyright   : Copyright (c) 2018  Niklas Hambüchen.
-- License     : MIT License.
--
-- MessagePack FFI code adapted from call-haskell-from-anything
module FFI.TypeUncurry.Msgpack
  ( MessagePackRec (..),
    getTypeListFromMsgpackArray,
    byteStringToCStringFunIO,
    exportIO,
  )
where

import qualified Control.Exception.Enclosed as EE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.MessagePack as MSG
import Data.Proxy
import Data.Storable.Endian (peekBE, pokeBE)
import FFI.TypeUncurry
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Protolude
import Prelude (String, error, fail)

-- | Helper to allow writing a 'MSG.MessagePack' instance for 'TypeList's.
--
-- We need this because we have to call 'parseArray' at the top-level
-- 'MSG.MessagePack' instance, but not at each function argument step.
class MessagePackRec l where
  fromObjectRec :: (Monad m) => [MSG.Object] -> m (TypeList l)

-- | When no more types need to be unpacked, we are done.
instance MessagePackRec '[] where
  fromObjectRec v | null v = pure Nil
  fromObjectRec _ = fail "fromObjectRec: passed object is not expected []"

-- | Unpack one type by just parsing the next element.
instance
  (MSG.MessagePack a, MessagePackRec l) =>
  MessagePackRec (a ': l)
  where
  fromObjectRec (x : xs) = (:::) <$> MSG.fromObject x <*> fromObjectRec xs
  fromObjectRec _ = fail "fromObjectRec: passed object is not expected (x:xs)"

-- | Parses a tuple of arbitrary size ('TypeList's) from a MessagePack array.
getTypeListFromMsgpackArray ::
  forall m l.
  (MessagePackRec l, ParamLength l, Monad m) =>
  MSG.Object ->
  m (TypeList l)
getTypeListFromMsgpackArray obj = case obj of
  MSG.ObjectArray v | length v == len -> fromObjectRec v
  _ -> fail "getTypeListFromMsgpackArray: wrong object length"
  where
    len = paramLength (Proxy :: Proxy l)

instance
  (MessagePackRec l, ParamLength l) =>
  MSG.MessagePack (TypeList l)
  where

  fromObject = getTypeListFromMsgpackArray

  toObject = error "call-haskell-from-anything: Serialising a TypeList is not supported (and not needed)!"

-- | Standard error message when unpacking failed.
errorMsg :: String -> String
errorMsg locationStr =
  "call-haskell-from-anything: "
    ++ locationStr
    ++ ": got wrong number of function arguments or non-array"

-- This function throws an 'error' if the de-serialization of the arguments fails!
-- It is recommended to use 'tryUncurryMsgpackIO' instead.
uncurryMsgpackIO ::
  (MSG.MessagePack (TypeList l), ToTypeList f l (IO r), MSG.MessagePack r) =>
  f ->
  (ByteString -> IO ByteString)
uncurryMsgpackIO f bs =
  BSL.toStrict . MSG.pack
    <$> exceptionLess
      ( translate
          f
          ( fromMaybe (error (errorMsg "uncurryMsgpackIO"))
              $ MSG.unpack
              $ BSL.fromStrict bs
          )
      )

exceptionLess :: IO a -> IO (Either Text a)
exceptionLess io = EE.catchAny (Right <$> io) $ return . Left . show

-- | O(n). Makes a copy of the ByteString's contents into a malloc()ed area.
-- You need to free() the returned string when you're done with it.
byteStringToMallocedCStringWith64bitLength :: ByteString -> IO CString
byteStringToMallocedCStringWith64bitLength bs =
  unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    targetPtr <- mallocBytes (8 + len)
    pokeBE (castPtr targetPtr) (fromIntegral len :: Int64)
    copyBytes (targetPtr `plusPtr` 8) ptr len
    return targetPtr

-- * Exporting

-- | Transforms a 'ByteString'-mapping 'IO' function to 'CString'-mapping function
-- for use in the FFI.
byteStringToCStringFunIO :: (ByteString -> IO ByteString) -> CString -> IO CString
byteStringToCStringFunIO f cs = do
  msgLength :: Int64 <- peekBE (castPtr cs)
  cs_bs <- BS.packCStringLen (cs `plusPtr` 8, fromIntegral msgLength)
  res_bs <- f cs_bs
  byteStringToMallocedCStringWith64bitLength res_bs

-- Use 'tryExportIO' if you want to handle this case.
exportIO ::
  (MSG.MessagePack (TypeList l), ToTypeList f l (IO r), MSG.MessagePack r) =>
  f ->
  CString ->
  IO CString
exportIO = byteStringToCStringFunIO . uncurryMsgpackIO
