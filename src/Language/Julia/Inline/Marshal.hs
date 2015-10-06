{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Julia.Inline.Marshal where

import qualified Data.Vector.Storable.Mutable as VM
import Data.Int
import Data.Word
import Foreign.Marshal
import Foreign.Storable
import Foreign.StablePtr
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.LibFFI
import Foreign.C.Types

import System.Posix.DynamicLinker
import System.IO.Unsafe

import Language.Julia.Inline.InternalDynamic

-- toJulia = [ (''Word64, jlBoxWord64)
--           , (''Word32, jlBoxWord32)
--           , (''Word16, jlBoxWord16)
--           , (''Word8, jlBoxWord8)
--           , (''Int64, jlBoxInt64)
--           , (''Int32, jlBoxInt32)
--           , (''Int16, jlBoxInt16)
--           , (''Int8, jlBoxInt8)
--           , (''Float, jlBoxFloat)
--           , (''Double, jlBoxDouble)
--           ]
--
-- fromJulia = [ ("Int64", jlUnboxInt64)
--             , ("Int32", jlUnboxInt32)
--             , ("Int16", jlUnboxInt16)
--             , ("Int8", jlUnboxInt8)
--             , ("UInt64", jlUnboxWord64)
--             , ("UInt32", jlUnboxWord32)
--             , ("UInt16", jlUnboxWord16)
--             , ("UInt8", jlUnboxWord8)
--             , ("Float", jlUnboxFloat)
--             , ("Double", jlUnboxDouble)
--             ]

{-# NOINLINE jl_box_int32 #-}
jl_box_int32 = unsafePerformIO $ dlsym libjulia "jl_box_int32"

{-# NOINLINE jl_unbox_int32 #-}
jl_unbox_int32 = unsafePerformIO $ dlsym libjulia "jl_unbox_int32"

{-# NOINLINE jl_box_int16 #-}
jl_box_int16 = unsafePerformIO $ dlsym libjulia "jl_box_int16"

{-# NOINLINE jl_unbox_int16 #-}
jl_unbox_int16 = unsafePerformIO $ dlsym libjulia "jl_unbox_int16"

{-# NOINLINE jl_box_int8 #-}
jl_box_int8 = unsafePerformIO $ dlsym libjulia "jl_box_int8"

{-# NOINLINE jl_unbox_int8 #-}
jl_unbox_int8 = unsafePerformIO $ dlsym libjulia "jl_unbox_int8"

{-# NOINLINE jl_box_uint64 #-}
jl_box_uint64 = unsafePerformIO $ dlsym libjulia "jl_box_uint64"

{-# NOINLINE jl_unbox_uint64 #-}
jl_unbox_uint64 = unsafePerformIO $ dlsym libjulia "jl_unbox_uint64"

{-# NOINLINE jl_box_uint32 #-}
jl_box_uint32 = unsafePerformIO $ dlsym libjulia "jl_box_uint32"

{-# NOINLINE jl_unbox_uint32 #-}
jl_unbox_uint32 = unsafePerformIO $ dlsym libjulia "jl_unbox_uint32"

{-# NOINLINE jl_box_uint16 #-}
jl_box_uint16 = unsafePerformIO $ dlsym libjulia "jl_box_uint16"

{-# NOINLINE jl_unbox_uint16 #-}
jl_unbox_uint16 = unsafePerformIO $ dlsym libjulia "jl_unbox_uint16"

{-# NOINLINE jl_box_uint8 #-}
jl_box_uint8 = unsafePerformIO $ dlsym libjulia "jl_box_uint8"

{-# NOINLINE jl_unbox_uint8 #-}
jl_unbox_uint8 = unsafePerformIO $ dlsym libjulia "jl_unbox_uint8"

{-# NOINLINE jl_box_float32 #-}
jl_box_float32 = unsafePerformIO $ dlsym libjulia "jl_box_float32"

{-# NOINLINE jl_unbox_float32 #-}
jl_unbox_float32 = unsafePerformIO $ dlsym libjulia "jl_unbox_float32"

{-# NOINLINE jl_box_float64 #-}
jl_box_float64 = unsafePerformIO $ dlsym libjulia "jl_box_float64"

{-# NOINLINE jl_unbox_float64 #-}
jl_unbox_float64 = unsafePerformIO $ dlsym libjulia "jl_unbox_float64"

{-# NOINLINE jl_ptr_to_array_1d #-}
jl_ptr_to_array_1d = unsafePerformIO $ dlsym libjulia "jl_ptr_to_array_1d"

jlBoxInt64 :: Int64 -> IO JLVal
jlBoxInt64 i = callJulia jl_box_int64 [argInt64 i]

jlUnboxInt64 :: JLVal -> IO Int64
jlUnboxInt64 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int64 retInt64 [argPtr p]

jlBoxInt32 :: Int32 -> IO JLVal
jlBoxInt32 i = callJulia jl_box_int32 [argInt32 i]

jlUnboxInt32 :: JLVal -> IO Int32
jlUnboxInt32 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int32 retInt32 [argPtr p]

jlBoxInt16 :: Int16 -> IO JLVal
jlBoxInt16 i = callJulia jl_box_int16 [argInt16 i]

jlUnboxInt16 :: JLVal -> IO Int16
jlUnboxInt16 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int16 retInt16 [argPtr p]

jlBoxInt8 :: Int8 -> IO JLVal
jlBoxInt8 i = callJulia jl_box_int8 [argInt8 i]

jlUnboxInt8 :: JLVal -> IO Int8
jlUnboxInt8 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int8 retInt8 [argPtr p]

jlBoxWord64 :: Word64 -> IO JLVal
jlBoxWord64 i = callJulia jl_box_uint64 [argWord64 i]

jlUnboxWord64 :: JLVal -> IO Word64
jlUnboxWord64 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint64 retWord64 [argPtr p]

jlBoxWord32 :: Word32 -> IO JLVal
jlBoxWord32 i = callJulia jl_box_uint32 [argWord32 i]

jlUnboxWord32 :: JLVal -> IO Word32
jlUnboxWord32 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint32 retWord32 [argPtr p]

jlBoxWord16 :: Word16 -> IO JLVal
jlBoxWord16 i = callJulia jl_box_uint16 [argWord16 i]

jlUnboxWord16 :: JLVal -> IO Word16
jlUnboxWord16 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint16 retWord16 [argPtr p]

jlBoxWord8 :: Word8 -> IO JLVal
jlBoxWord8 i = callJulia jl_box_uint8 [argWord8 i]

jlUnboxWord8 :: JLVal -> IO Word8
jlUnboxWord8 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint8 retWord8 [argPtr p]

jlBoxFloat :: Float -> IO JLVal
jlBoxFloat i = callJulia jl_box_float32 [argCFloat $ CFloat i]

jlUnboxFloat :: JLVal -> IO Float
jlUnboxFloat (JLVal i) = do
  CFloat f <- withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_float32 retCFloat [argPtr p]
  return f

jlBoxDouble :: Double -> IO JLVal
jlBoxDouble i = callJulia jl_box_float64 [argCDouble $ CDouble i]

jlUnboxDouble :: JLVal -> IO Double
jlUnboxDouble (JLVal i) = do
  CDouble d <- withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_float64 retCDouble [argPtr p]
  return d

jlBoxVoidPtr :: Ptr () -> IO JLVal
jlBoxVoidPtr i = callJulia jl_box_voidpointer [argPtr i]

-- The vector cannot be frozen before using this function
-- The vector may be mutated by julia
jlBoxMVector :: Storable a => VM.IOVector a -> String -> IO JLVal
jlBoxMVector v typ = do
  sbPtr <- newStablePtr v
  let (fp, l) = VM.unsafeToForeignPtr0 v
  withForeignPtr fp $ \p -> do
    -- box the array and turn it into a julia array
    jv <- jlBoxVoidPtr $ castPtr p
    jl <- jlBoxInt64 $ fromIntegral l
    ja <- jlCallFunction ("(x,l) -> pointer_to_array(convert(Ptr{" ++ typ ++ "}, x), (l,))") [jv, jl]
    -- box the free pointer
    jp <- jlBoxVoidPtr $ castStablePtrToPtr sbPtr
    -- add the finalizer
    jlCallFunction "(a,p) -> finalizer(a, x -> HaskellGC.finalize_hs(p))" [ja, jp]
    return ja

-- Do not freeze this vector until after julia is done using it
-- The type of elements of the IOVector must match julia type
-- The a passed in is to determine sizeof elements
jlUnboxMVector :: forall a. Storable a => JLVal -> IO (VM.IOVector a)
jlUnboxMVector jv@(JLVal v) = withForeignPtr v $ \p -> do
  l <- jlCallFunction "length" [jv] >>= jlUnboxInt64
  let dp :: Ptr a = jl_array_data1 p
  -- we calculate offset using bytes
  let offset = (castPtr dp :: Ptr Int8) `minusPtr` (castPtr p)
  -- we dont need to retain the vector because MVector does it for us
  let vec :: VM.IOVector Int8 = VM.unsafeFromForeignPtr (castForeignPtr v) offset (fromIntegral l * sizeOf (undefined :: a))
  return $ VM.unsafeCast vec

-- | Make haskell data avaliable to Julia as a Ptr.
-- The data is copied.
-- TODO: allow references to haskell values to be passed
-- TODO: there are currently two copies being done
box :: Storable a => a -> String -> IO JLVal
box x t = with x $ \p -> do
  jv <- jlBoxVoidPtr $ castPtr p
  jlCallFunction ("x -> unsafe_load(convert(Ptr{" ++ t ++ "}, x), 1)") [jv]

-- jl_array_data is a macro
foreign import ccall unsafe "jl_array_data1" jl_array_data1 :: Ptr JLVal -> Ptr a
