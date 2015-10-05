{-# LANGUAGE ScopedTypeVariables #-}
module Language.Julia.Inline.InternalDynamic where

import Foreign.LibFFI
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.StablePtr
import Foreign.C.String
import Foreign.C.Types
import GHC.ForeignPtr
import System.Posix.DynamicLinker
import System.IO.Unsafe
import Control.Exception
import Control.Monad
import Data.Int
import Data.Word
import Control.Concurrent.MVar

import Data.Vector.Storable.Mutable hiding (length)

import Debug.Trace

-- TODO: precompile julia functions
-- TODO: memory management
-- | memory management when briding the gap between julia and haskell is a
-- little interesting. All julia functions which return a pointer require that
-- said pointer be retained in some way before the next call to julia. We
-- create a global julia array to hold references to all data that haskell has.
-- On the haskell side, julia pointers a wrapped with a ForeignPtr that that
-- removes the object from the array when it is gc'd in haskell.
--
-- Passing Haskell values to julia is a little more complicated. If the data we
-- are passing is not Storable, then we call a marshaller to generate data
-- which julia then copies and manages. If the data is mutable, then we pass
-- julia the data directy from a pointer and julia copies it.
-- TODO: Storable types should be able to be managed with julia's gc using stableptr

data JuliaException = JuliaException JLVal deriving (Show)
data RawJuliaException = RawJuliaException (Ptr ()) deriving (Show)
instance Exception JuliaException
instance Exception RawJuliaException

newtype JLVal = JLVal (ForeignPtr JLVal)
newtype JLFunc = JLFunc (ForeignPtr JLFunc)
newtype JLModule = JLModule (ForeignPtr JLModule)

-- | The julia runtime is not threadsafe. This global lock makes sure we don't
-- access concurrently
{-# NOINLINE juliaLock #-}
juliaLock :: MVar ()
juliaLock = unsafePerformIO $ do
  jlInit "/Applications/Julia-0.4.0-rc3.app/Contents/Resources/julia/libexec"
  newMVar ()

-- TODO: this lock could deadlock...
-- this function is unsafe because it does not retain julia values
callJuliaUnsafe :: FunPtr c -> RetType a -> [Arg] -> IO a
callJuliaUnsafe f ret args = withMVar juliaLock $ const $ do
  x <- callFFI f ret args
  -- check for exception
  e <- callFFI jl_exception_occurred (retPtr retVoid) []
  when (e /= nullPtr) $ do
    -- backtrace
    -- tmp <- callFFI jl_eval_string (retPtr retVoid) [argString "print"]
    -- callFFI jl_call1 (retPtr retVoid) [argPtr tmp, argPtr e]
    -- callFFI jl_eval_string (retPtr retVoid) [argString "println(sprint(io->Base.show_backtrace(io, backtrace())))"]
    throw $ RawJuliaException e
  return x

-- call a julia c function and retain a reference to its result
callJulia :: FunPtr c -> [Arg] -> IO JLVal
callJulia a b = do
  x <- handle f $ callJuliaUnsafe a (retPtr retVoid) b
  -- TODO: check if result is null and dont push gc
  jlGCPush x
  where
    f :: RawJuliaException -> IO a
    f (RawJuliaException e) = jlGCPush e >>= throw . JuliaException

instance Show JLVal where
  show v = "julia: " ++ showJL v

instance Show JLFunc where
  show (JLFunc p) = "julia: " ++ (showJL $ JLVal $ castForeignPtr p)

instance Show JLModule where
  show (JLModule p) = "julia: " ++ (showJL $ JLVal $ castForeignPtr p)

-- TODO: noinline?
showJL :: JLVal -> String
showJL v = unsafePerformIO $ do
  rep <- jlCallFunction "HaskellGC.show" [v]
  sPtr <- jlUnboxVoidPtr rep
  peekCString $ castPtr sPtr

-- julia library and function ptrs. We cache them for performance

{-# NOINLINE libjulia #-}
libjulia = unsafePerformIO $ dlopen "libjulia.dylib" [RTLD_NOW, RTLD_GLOBAL]

{-# NOINLINE jl_eval_string #-}
jl_eval_string = unsafePerformIO $ dlsym libjulia "jl_eval_string"

{-# NOINLINE jl_call0 #-}
jl_call0 = unsafePerformIO $ dlsym libjulia "jl_call0"

{-# NOINLINE jl_call #-}
jl_call = unsafePerformIO $ dlsym libjulia "jl_call"

{-# NOINLINE jl_call1 #-}
jl_call1 = unsafePerformIO $ dlsym libjulia "jl_call1"

{-# NOINLINE jl_call2 #-}
jl_call2 = unsafePerformIO $ dlsym libjulia "jl_call2"

{-# NOINLINE jl_call3 #-}
jl_call3 = unsafePerformIO $ dlsym libjulia "jl_call3"

{-# NOINLINE jl_box_voidpointer #-}
jl_box_voidpointer = unsafePerformIO $ dlsym libjulia "jl_box_voidpointer"

{-# NOINLINE jl_unbox_voidpointer #-}
jl_unbox_voidpointer = unsafePerformIO $ dlsym libjulia "jl_unbox_voidpointer"

{-# NOINLINE jl_box_int64 #-}
jl_box_int64 = unsafePerformIO $ dlsym libjulia "jl_box_int64"

{-# NOINLINE jl_unbox_int64 #-}
jl_unbox_int64 = unsafePerformIO $ dlsym libjulia "jl_unbox_int64"

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

{-# NOINLINE jl_exception_occurred #-}
jl_exception_occurred = unsafePerformIO $ dlsym libjulia "jl_exception_occurred"

{-# NOINLINE jl_gc_add_finalizer #-}
jl_gc_add_finalizer = unsafePerformIO $ dlsym libjulia "jl_gc_add_finalizer"

{-# NOINLINE jl_ptr_to_array_1d #-}
jl_ptr_to_array_1d = unsafePerformIO $ dlsym libjulia "jl_ptr_to_array_1d"

{-# NOINLINE jl_array_data #-}
jl_array_data = unsafePerformIO $ dlsym Default "jl_array_data1"

foreign import ccall "atexit" atexit :: FunPtr () -> IO ()

-- | Initializes julia runtime and sets up garbage collector
jlInit :: FilePath -> IO ()
jlInit s = do
  -- load dynamic lib
  jl_init <- dlsym libjulia "jl_init"
  callFFI jl_init retVoid [argString s] -- TODO: check for exception?
  -- set up global array to hold haskell references to julia
  callFFI jl_eval_string (retPtr retVoid) [argString "require(\"julia/gc.jl\")"]
  -- Install julia's atexit hook
  -- jl_atexit_hook <- dlsym libjulia "jl_atexit_hook"
  -- atexit jl_atexit_hook
  return ()

-- TODO: fix int64, should be platform dependant
jlGCPop :: Int64 -> IO ()
jlGCPop i = do
  release <- callJuliaUnsafe jl_eval_string (retPtr retVoid) [argString "HaskellGC.release"]
  i' <- callJuliaUnsafe jl_box_int64 (retPtr retVoid) [argInt64 i]
  callJuliaUnsafe jl_call1 (retPtr retVoid) [argPtr release, argPtr i']
  return ()

-- TODO: do we need to call GCPush before hand?
-- | The pointer passed in should not be used after this
jlGCPush :: Ptr () -> IO JLVal
jlGCPush p = do
  retain <- callJuliaUnsafe jl_eval_string (retPtr retVoid) [argString "HaskellGC.retain"]
  i <- callJuliaUnsafe jl_call1 (retPtr retVoid) [argPtr retain, argPtr p]
  i' <- callJuliaUnsafe jl_unbox_int64 retInt64 [argPtr i]
  fp <- newForeignPtr_ (castPtr p)
  addForeignPtrConcFinalizer fp (jlGCPop i')
  return $ JLVal fp

wrapGC :: IO (Ptr ()) -> IO JLVal
wrapGC a = a >>= jlGCPush

-- jlGCPush :: JL a => a -> IO ()
-- jlGCPush = do

jlEvalString :: String -> IO JLVal
jlEvalString s = callJulia jl_eval_string [argString s]

-- XXX: brittle
-- jlMainModule = jlEvalString "Main"

-- TODO: check result is function
jlGetFunction :: String -> IO JLFunc
jlGetFunction s = do
  JLVal v <- jlEvalString s
  return $ JLFunc $ castForeignPtr v

jlCallFunction :: String -> [JLVal] -> IO JLVal
jlCallFunction fn args = do
  f <- jlGetFunction fn
  jlCall f args

-- | call a JLFunc with the given arguements
jlCall :: JLFunc -> [JLVal] -> IO JLVal
jlCall (JLFunc f) args = do
  withForeignPtr f $ \func ->
    -- if the number of arguements is small, we use jl_call0-3
    case args of
      [] -> callJulia jl_call0 [argPtr func]
      xs | length xs == 1 -> callPtrs jl_call1 func args
      xs | length xs == 2 -> callPtrs jl_call2 func args
      xs | length xs == 3 -> callPtrs jl_call3 func args
      _ -> withMany withForeignPtr (map unwrap args) $ \ptrs ->
             withArray ptrs $ \a ->
               callJulia jl_call [argPtr func, argPtr a, argInt32 (fromIntegral $ length args)]
  where
    unwrap (JLVal v) = v
    callPtrs call fn xs = withMany withForeignPtr (map unwrap xs) $ \ptrs ->
                         callJulia call $ (argPtr fn):(map argPtr ptrs)

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
jlBoxMVector :: Storable a => IOVector a -> String -> IO JLVal
jlBoxMVector v typ = do
  sbPtr <- newStablePtr v
  let (fp, l) = unsafeToForeignPtr0 v
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
jlUnboxMVector :: Storable a => JLVal -> IO (IOVector a)
jlUnboxMVector jv@(JLVal v) = withForeignPtr v $ \p -> do
  l <- jlCallFunction "length" [jv] >>= jlUnboxInt64
  let dp = jl_array_data1 $ castPtr p
  -- dp <- callJuliaUnsafe jl_array_data (retPtr retVoid) [argPtr p]
  print dp
  print p
  print $ dp `minusPtr` (castPtr p)
  -- we dont need to retain the vector because MVector does it for us
  -- TODO: compute actual offset
  let offset = (castPtr p) `minusPtr` dp
  return $ unsafeFromForeignPtr (castForeignPtr v) offset (fromIntegral l)

-- TODO: who owns this memory?
jlUnboxVoidPtr' :: Ptr () -> IO (Ptr ())
jlUnboxVoidPtr' v = callJuliaUnsafe jl_unbox_voidpointer (retPtr retVoid) [argPtr v]

jlUnboxVoidPtr :: JLVal -> IO (Ptr ())
jlUnboxVoidPtr (JLVal v) = withForeignPtr v (jlUnboxVoidPtr' . castPtr)

-- | Make haskell data avaliable to Julia as a Ptr.
-- The data is copied.
-- TODO: allow references to haskell values to be passed
-- TODO: there are currently two copies being done
box :: Storable a => a -> String -> IO JLVal
box x t = with x $ \p -> do
  jv <- jlBoxVoidPtr $ castPtr p
  jlCallFunction ("x -> unsafe_load(convert(Ptr{" ++ t ++ "}, x), 1)") [jv]

foreign import ccall unsafe "jl_array_data1" jl_array_data1 :: Ptr () -> Ptr ()
