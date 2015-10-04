module Language.Inline.Julia.InternalDynamic where

import Foreign.LibFFI
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import GHC.ForeignPtr
import Foreign.StablePtr
import Foreign.C.String
import System.Posix.DynamicLinker
import System.IO.Unsafe
import Control.Exception
import Control.Monad
import Data.Int
import Control.Concurrent.MVar

import Debug.Trace

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
  jlInit "/Users/tristan/Applications/Julia-0.3.10.app/Contents/Resources/julia/libexec"
  newMVar ()

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

{-# NOINLINE jl_box_voidpointer #-}
jl_box_voidpointer = unsafePerformIO $ dlsym libjulia "jl_box_voidpointer"

{-# NOINLINE jl_unbox_voidpointer #-}
jl_unbox_voidpointer = unsafePerformIO $ dlsym libjulia "jl_unbox_voidpointer"

{-# NOINLINE jl_box_int64 #-}
jl_box_int64 = unsafePerformIO $ dlsym libjulia "jl_box_int64"

{-# NOINLINE jl_unbox_int64 #-}
jl_unbox_int64 = unsafePerformIO $ dlsym libjulia "jl_unbox_int64"

{-# NOINLINE jl_exception_occurred #-}
jl_exception_occurred = unsafePerformIO $ dlsym libjulia "jl_exception_occurred"

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
  (JLFunc f) <- jlGetFunction fn
  withForeignPtr f $ \func ->
    case args of -- TODO: ass call1 and call2 [] -> callJulia jl_call0 [argPtr func]
      x:[] -> withForeignPtr (unwrap x) $ \p -> callJulia jl_call1 [argPtr func, argPtr p]
      _ -> withMany withForeignPtr (map unwrap args) $ \ptrs ->
             withArray ptrs $ \a ->
               callJulia jl_call [argPtr func, argPtr a, argInt32 (fromIntegral $ length args)]
  where
    unwrap (JLVal v) = v

jlCall0' :: Ptr () -> IO (Ptr ())
jlCall0' f = callJuliaUnsafe jl_call0 (retPtr retVoid) [argPtr f]

jlCall0 :: JLFunc -> IO JLVal
jlCall0 (JLFunc f) = withForeignPtr f (wrapGC . jlCall0' . castPtr)

jlCall1' :: Ptr () -> Ptr () -> IO (Ptr ())
jlCall1' f p = callJuliaUnsafe jl_call1 (retPtr retVoid) [argPtr f, argPtr p]

jlCall1 :: JLFunc -> JLVal -> IO JLVal
jlCall1 (JLFunc f) (JLVal v) = withForeignPtr f
  (\p -> withForeignPtr v (\v' -> wrapGC $ jlCall1' (castPtr p) (castPtr v')))

jlBoxInt64' :: Int64 -> IO (Ptr ())
jlBoxInt64' i = callJuliaUnsafe jl_box_int64 (retPtr retVoid) [argInt64 i]

jlUnboxInt64' :: Ptr () -> IO Int64
jlUnboxInt64' i = callJuliaUnsafe jl_unbox_int64 retInt64 [argPtr i]

jlBoxVoidPtr' :: Ptr () -> IO (Ptr ())
jlBoxVoidPtr' p = callJuliaUnsafe jl_box_voidpointer (retPtr retVoid) [argPtr p]

jlBoxVoidPtr :: Ptr () -> IO JLVal
jlBoxVoidPtr p = callJulia jl_box_voidpointer [argPtr p]

-- TODO: who owns this memory?
jlUnboxVoidPtr' :: Ptr () -> IO (Ptr ())
jlUnboxVoidPtr' v = callJuliaUnsafe jl_unbox_voidpointer (retPtr retVoid) [argPtr v]

jlUnboxVoidPtr :: JLVal -> IO (Ptr ())
jlUnboxVoidPtr (JLVal v) = withForeignPtr v (jlUnboxVoidPtr' . castPtr)
