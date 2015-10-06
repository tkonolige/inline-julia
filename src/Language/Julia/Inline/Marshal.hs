{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Foreign.C.String

import System.Posix.DynamicLinker
import System.IO.Unsafe

import Language.Julia.Inline.InternalDynamic
import Language.Julia.Inline.Quote

-- Make sure a JLVal lives for a scope
withJLVal :: JLVal -> IO a -> IO a
withJLVal (JLVal v) a = withForeignPtr v $ const a

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

hsInt64 :: Int64 -> IO JLVal
hsInt64 i = callJulia jl_box_int64 [argInt64 i]

jlInt64 :: JLVal -> IO Int64
jlInt64 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int64 retInt64 [argPtr p]

hsInt32 :: Int32 -> IO JLVal
hsInt32 i = callJulia jl_box_int32 [argInt32 i]

jlInt32 :: JLVal -> IO Int32
jlInt32 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int32 retInt32 [argPtr p]

hsInt16 :: Int16 -> IO JLVal
hsInt16 i = callJulia jl_box_int16 [argInt16 i]

jlInt16 :: JLVal -> IO Int16
jlInt16 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int16 retInt16 [argPtr p]

hsInt8 :: Int8 -> IO JLVal
hsInt8 i = callJulia jl_box_int8 [argInt8 i]

jlInt8 :: JLVal -> IO Int8
jlInt8 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int8 retInt8 [argPtr p]

hsWord64 :: Word64 -> IO JLVal
hsWord64 i = callJulia jl_box_uint64 [argWord64 i]

jlWord64 :: JLVal -> IO Word64
jlWord64 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint64 retWord64 [argPtr p]

hsWord32 :: Word32 -> IO JLVal
hsWord32 i = callJulia jl_box_uint32 [argWord32 i]

jlWord32 :: JLVal -> IO Word32
jlWord32 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint32 retWord32 [argPtr p]

hsWord16 :: Word16 -> IO JLVal
hsWord16 i = callJulia jl_box_uint16 [argWord16 i]

jlWord16 :: JLVal -> IO Word16
jlWord16 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint16 retWord16 [argPtr p]

hsWord8 :: Word8 -> IO JLVal
hsWord8 i = callJulia jl_box_uint8 [argWord8 i]

jlWord8 :: JLVal -> IO Word8
jlWord8 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint8 retWord8 [argPtr p]

hsFloat :: Float -> IO JLVal
hsFloat i = callJulia jl_box_float32 [argCFloat $ CFloat i]

jlFloat :: JLVal -> IO Float
jlFloat (JLVal i) = do
  CFloat f <- withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_float32 retCFloat [argPtr p]
  return f

hsDouble :: Double -> IO JLVal
hsDouble i = callJulia jl_box_float64 [argCDouble $ CDouble i]

jlDouble :: JLVal -> IO Double
jlDouble (JLVal i) = do
  CDouble d <- withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_float64 retCDouble [argPtr p]
  return d

-- The vector cannot be frozen before using this function
-- The vector may be mutated by julia
hsMVector :: Storable a => VM.IOVector a -> String -> IO JLVal
hsMVector v typ = do
  sbPtr <- newStablePtr v
  let (fp, l) = VM.unsafeToForeignPtr0 v
  withForeignPtr fp $ \p -> do
    -- box the array and turn it into a julia array
    ja <- [julia| pointer_to_array(convert(Ptr{$(hsType typ)}, $(hsVoidPtr $ castPtr p)), ($(hsInt64 $ fromIntegral l),))|]
    -- box the free pointer
    jp <- hsVoidPtr $ castStablePtrToPtr sbPtr
    -- add the finalizer
    [julia| finalizer($(jl ja), x -> HaskellGC.finalize_hs($(jl jp))) |]
    return ja

-- Do not freeze this vector until after julia is done using it
-- The type of elements of the IOVector must match julia type
-- The a passed in is to determine sizeof elements
-- TODO: check types
jlMVector :: forall a. Storable a => JLVal -> IO (VM.IOVector a)
jlMVector jv@(JLVal v) = withForeignPtr v $ \p -> do
  l <- [julia| length($(jl jv)) |] >>= jlInt64
  let dp :: Ptr a = jl_array_data1 p
  -- we calculate offset using bytes
  let offset = (castPtr dp :: Ptr Int8) `minusPtr` (castPtr p)
  -- we dont need to retain the vector because MVector does it for us
  let vec :: VM.IOVector Int8 = VM.unsafeFromForeignPtr (castForeignPtr v) offset (fromIntegral l * sizeOf (undefined :: a))
  return $ VM.unsafeCast vec

jlVoid :: JLVal -> IO ()
jlVoid v = do
  [julia| @assert (typeof($(jl v)) == Void) "Return type is not Void" |]
  return ()

jlString :: JLVal -> IO String
jlString v = withJLVal v $ do
  p <- [julia| Base.unsafe_convert(Cstring, $(jl v)) |] >>= jlVoidPtr
  peekCString $ castPtr p

jl :: JLVal -> IO JLVal
jl = return

hsType :: String -> IO JLVal
hsType s = do
  jlCallFunction s []

-- jl_array_data is a macro
foreign import ccall unsafe "jl_array_data1" jl_array_data1 :: Ptr JLVal -> Ptr a
