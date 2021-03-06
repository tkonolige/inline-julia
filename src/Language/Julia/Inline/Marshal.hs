{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Language.Julia.Inline.Marshal (
    JLConvertable(..)
  , withJLVal
  -- * Converting Haskell values into Julia values
  , hsInt64
  , hsInt32
  , hsInt16
  , hsInt8
  , hsInt
  , hsWord64
  , hsWord32
  , hsWord16
  , hsWord8
  , hsWord
  , hsCSize
  , hsFloat
  , hsDouble
  , hsMVector
  , hsVector
  , hsList
  , hsType
  , hsTypeLit
  , hsByteString
  , hsLazyByteString
  , jl
  -- * Converting Julia values into Haskell values
  , jlInt64
  , jlInt32
  , jlInt16
  , jlInt8
  , jlInt
  , jlWord64
  , jlWord32
  , jlWord16
  , jlWord8
  , jlWord
  , jlCSize
  , jlFloat
  , jlDouble
  , jlMVector
  , jlVector
  , jlList
  , jlVoid
  , jlString
  , jlByteString
  , jlLazyByteString
  ) where

import Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.ByteString.Unsafe
import           Data.Int
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.LibFFI
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable

import           System.IO.Unsafe
import           System.Posix.DynamicLinker

import           Language.Julia.Inline.InternalDynamic
import           Language.Julia.Inline.Quote

instance JLConvertable JLVal where
  jlType _ = "Any"
  toJL = jl

instance JLConvertable Int where
  jlType _ = "Int"
  toJL = hsInt

instance JLConvertable Word where
  jlType _ = "Word"
  toJL = hsWord

instance JLConvertable CSize where
  jlType _ = "CSize"
  toJL = hsCSize

instance JLConvertable Int8 where
  jlType _ = "Int8"
  toJL = hsInt8

instance JLConvertable Int16 where
  jlType _ = "Int16"
  toJL = hsInt16

instance JLConvertable Int32 where
  jlType _ = "Int32"
  toJL = hsInt32

instance JLConvertable Int64 where
  jlType _ = "Int64"
  toJL = hsInt64

instance JLConvertable Word8 where
  jlType _ = "Word8"
  toJL = hsWord8

instance JLConvertable Word16 where
  jlType _ = "Word16"
  toJL = hsWord16

instance JLConvertable Word32 where
  jlType _ = "Word32"
  toJL = hsWord32

instance JLConvertable Word64 where
  jlType _ = "Word64"
  toJL = hsWord64

instance JLConvertable CString where
  jlType _ = "Cstring"
  toJL = hsCString

instance JLConvertable B.ByteString where
  jlType _ = "Vector{UInt8}"
  toJL = hsByteString

instance JLConvertable BL.ByteString where
  jlType _ = "Vector{UInt8}"
  toJL = hsLazyByteString

instance {-# OVERLAPPING #-} JLConvertable String where
  jlType _ = "ASCIIString"
  toJL = hsString

instance (Storable a, JLConvertable a) => JLConvertable (VM.IOVector a) where
  jlType _ = "Vector{" ++ jlType (undefined :: a) ++ "}"
  toJL = hsMVector

instance (Storable a, JLConvertable a) => JLConvertable (V.Vector a) where
  jlType _ = "Vector{" ++ jlType (undefined :: a) ++ "}"
  toJL = hsVector

instance (Storable a, JLConvertable a) => JLConvertable [a] where
  jlType _ = "Vector{" ++ jlType (undefined :: a) ++ "}"
  toJL = hsList

-- | Make sure a JLVal lives for a scope
withJLVal :: JLVal -> IO a -> IO a
withJLVal (JLVal v) a = withForeignPtr v $ const a

-- | Manage a Haskell value with the Julia GC
juliaGC :: a -> JLVal -> IO ()
juliaGC x jx = do
  sbPtr <- newStablePtr x
  -- box the stable ptr
  jp <- hsVoidPtr $ castStablePtrToPtr sbPtr
  -- add the finalizer
  [julia| finalizer($(jl jx), x -> HaskellGC.finalize_hs($(jl jp))) |] >>= jlVoid
  return ()

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

-- * Converting Haskell values into Julia values

hsInt :: Int -> IO JLVal
hsInt i = case finiteBitSize i of
            64 -> hsInt64 $ fromIntegral i
            32 -> hsInt32 $ fromIntegral i

hsWord :: Word -> IO JLVal
hsWord i = case finiteBitSize i of
            64 -> hsWord64 $ fromIntegral i
            32 -> hsWord32 $ fromIntegral i

hsCSize :: CSize -> IO JLVal
hsCSize i = case finiteBitSize i of
            64 -> hsWord64 $ fromIntegral i
            32 -> hsWord32 $ fromIntegral i

hsInt64 :: Int64 -> IO JLVal
hsInt64 i = callJulia jl_box_int64 [argInt64 i]

hsInt32 :: Int32 -> IO JLVal
hsInt32 i = callJulia jl_box_int32 [argInt32 i]

hsInt16 :: Int16 -> IO JLVal
hsInt16 i = callJulia jl_box_int16 [argInt16 i]

hsInt8 :: Int8 -> IO JLVal
hsInt8 i = callJulia jl_box_int8 [argInt8 i]

hsWord64 :: Word64 -> IO JLVal
hsWord64 i = callJulia jl_box_uint64 [argWord64 i]

hsWord32 :: Word32 -> IO JLVal
hsWord32 i = callJulia jl_box_uint32 [argWord32 i]

hsWord16 :: Word16 -> IO JLVal
hsWord16 i = callJulia jl_box_uint16 [argWord16 i]

hsWord8 :: Word8 -> IO JLVal
hsWord8 i = callJulia jl_box_uint8 [argWord8 i]

-- | Marshals 'Float' to @Float32@
hsFloat :: Float -> IO JLVal
hsFloat i = callJulia jl_box_float32 [argCFloat $ CFloat i]

-- | Marshals 'Double' to @Float64@
hsDouble :: Double -> IO JLVal
hsDouble i = callJulia jl_box_float64 [argCDouble $ CDouble i]

-- | Pass a mutable 'VM.IOVector' to Julia.
-- The vector cannot be frozen until after Julia is finished using it. The
-- vector may be mutated by julia.
hsMVector :: forall a. (Storable a, JLConvertable a) => VM.IOVector a -> IO JLVal
hsMVector v = do
  let (fp, l) = VM.unsafeToForeignPtr0 v
  withForeignPtr fp $ \p -> do
    -- box the array and turn it into a julia array
    ja <- [julia| pointer_to_array(convert(Ptr{$(hsType (undefined :: a))}, $(hsVoidPtr $ castPtr p)), ($(hsInt64 $ fromIntegral l),))|]
    juliaGC v ja
    return ja

-- | Clone a 'V.Vector' and pass it to Julia.
hsVector :: (Storable a, JLConvertable a) => V.Vector a -> IO JLVal
hsVector v = V.thaw v >>= \v' -> hsMVector v'

hsList :: (Storable a, JLConvertable a) => [a] -> IO JLVal
hsList v = hsVector (V.fromList v)

-- TODO: use size_t
-- TODO: call c from haskell
-- | Clone a 'B.ByteString' and pass it to Julia.
hsByteString :: B.ByteString -> IO JLVal
hsByteString s = case B.length s of
  0 -> [julia| ""::ASCIIString |]
  _ -> unsafeUseAsCStringLen s $ \(cs, l) -> do
    [julia| ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{Void}, UInt64), $(hsVoidPtr $ castPtr cs), $(hsWord64 $ fromIntegral l)) |]

-- | Clone a lazy 'BL.ByteString' and pass it to Julia.
hsLazyByteString :: BL.ByteString -> IO JLVal
hsLazyByteString s = hsByteString $ BL.toStrict s

-- | Get the type representation of a Haskell object in Julia.
hsType :: JLConvertable a => a -> IO JLVal
hsType x = jlEvalString $ jlType x

-- | Convert a 'String' into a type in Julia.
hsTypeLit :: String -> IO JLVal
hsTypeLit s = jlEvalString $ s ++ " :: Type"

-- | Pass a Julia value to a Julia function. Useful for when you don't want to
-- unbox and rebox values between calls to Julia.
jl :: JLVal -> IO JLVal
jl = return


-- * Converting Julia values into Haskell values

jlInt :: JLVal -> IO Int
jlInt v = case finiteBitSize (undefined :: Int) of
            64 -> fromIntegral <$> jlInt64 v
            32 -> fromIntegral <$> jlInt32 v

jlWord :: JLVal -> IO Word
jlWord v = case finiteBitSize (undefined :: Word) of
            64 -> fromIntegral <$> jlWord64 v
            32 -> fromIntegral <$> jlWord32 v

jlCSize :: JLVal -> IO CSize
jlCSize v = case finiteBitSize (undefined :: CSize) of
            64 -> fromIntegral <$> jlWord64 v
            32 -> fromIntegral <$> jlWord32 v

jlInt64 :: JLVal -> IO Int64
jlInt64 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int64 retInt64 [argPtr p]

jlInt32 :: JLVal -> IO Int32
jlInt32 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int32 retInt32 [argPtr p]

jlInt16 :: JLVal -> IO Int16
jlInt16 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int16 retInt16 [argPtr p]

jlInt8 :: JLVal -> IO Int8
jlInt8 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_int8 retInt8 [argPtr p]

jlWord64 :: JLVal -> IO Word64
jlWord64 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint64 retWord64 [argPtr p]

jlWord32 :: JLVal -> IO Word32
jlWord32 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint32 retWord32 [argPtr p]

jlWord16 :: JLVal -> IO Word16
jlWord16 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint16 retWord16 [argPtr p]

jlWord8 :: JLVal -> IO Word8
jlWord8 (JLVal i) = withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_uint8 retWord8 [argPtr p]

jlFloat :: JLVal -> IO Float
jlFloat (JLVal i) = do
  CFloat f <- withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_float32 retCFloat [argPtr p]
  return f

jlDouble :: JLVal -> IO Double
jlDouble (JLVal i) = do
  CDouble d <- withForeignPtr i $ \p -> callJuliaUnsafe jl_unbox_float64 retCDouble [argPtr p]
  return d

-- | Make a mutable Julia array avaliable in Haskell.
-- Do not freeze this vector until after julia is done using it.
-- The resulting 'VM.IOVector' may be modified at any time by Julia.
jlMVector :: forall a. (Storable a, JLConvertable a) => JLVal -> IO (VM.IOVector a)
jlMVector jv = do
  -- convert return type to right format
  ja@(JLVal v) <- [julia| convert($(hsType (undefined :: VM.IOVector a)), $(jl jv)) |]
  withForeignPtr v $ \p -> do
    l <- [julia| length($(jl ja)) |] >>= jlInt64
    let dp :: Ptr a = jl_array_data1 p
    -- we calculate offset using bytes
    let offset = (castPtr dp :: Ptr Int8) `minusPtr` (castPtr p)
    -- we dont need to retain the vector because MVector does it for us
    let vec :: VM.IOVector Int8 = VM.unsafeFromForeignPtr (castForeignPtr v) offset (fromIntegral l * sizeOf (undefined :: a))
    return $ VM.unsafeCast vec

-- | Copy a Julia array into a 'V.Vector'
jlVector :: (Storable a, JLConvertable a) => JLVal -> IO (V.Vector a)
jlVector v = jlMVector v >>= V.freeze

jlList :: (Storable a, JLConvertable a) => JLVal -> IO [a]
jlList v = V.toList <$> jlVector v

-- | Annotation for julia functions that return nothing.
jlVoid :: JLVal -> IO ()
jlVoid v = do
  [julia| @assert (typeof($(jl v)) == Void) "Return type is not Void" |]
  return ()

jlString :: JLVal -> IO String
jlString v = withJLVal v $ do
  p <- [julia| Base.unsafe_convert(Cstring, $(jl v)) |] >>= jlVoidPtr
  peekCString $ castPtr p

jlByteString :: JLVal -> IO B.ByteString
jlByteString v = withJLVal v $ do
  l <- [julia| length($(jl v)) |] >>= jlInt64
  jp@(JLVal jp') <- [julia| Base.unsafe_convert(Ptr{UInt8}, $(jl v)) |]
  p <- jlVoidPtr jp
  unsafePackCStringFinalizer (castPtr p) (fromIntegral l) (touchForeignPtr jp')

jlLazyByteString :: JLVal -> IO BL.ByteString
jlLazyByteString v = BL.fromStrict <$> jlByteString v

-- jl_array_data is a macro
foreign import ccall unsafe "jl_array_data1" jl_array_data1 :: Ptr JLVal -> Ptr a
