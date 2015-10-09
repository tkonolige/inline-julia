{-# LANGUAGE ScopedTypeVariables #-}

{-| A set of low-level bindings to the Julia runtime.

-}
module Language.Julia.Inline.InternalDynamic (
  -- * Memory management
  -- $mem

  -- * Julia Datatypes
    JuliaException(..)
  , JLVal(..)
  , JLFunc(..)
  -- * Julia's C-interface
  , libjulia
  , jl_box_voidpointer
  , jl_box_int64
  , jl_unbox_int64
  -- * Prmitive functions
  , callJulia
  , callJuliaUnsafe
  , jlVoidPtr
  , jlCallFunction
  , jlEvalString
  , showJL
  ) where

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

{- $mem
   Memory management when briding the gap between julia and haskell is a
   little interesting. All julia functions which return a pointer require that
   said pointer be retained in some way before the next call to julia. We
   create a global julia array to hold references to all data that haskell has.
   On the haskell side, julia pointers a wrapped with a ForeignPtr that that
   removes the object from the array when it is gc'd in haskell.

   Passing Haskell values to julia is a little more complicated. If the data we
   are passing is not Storable, then we call a marshaller to generate data
   which julia then copies and manages. If the data is mutable, then we pass
   julia the data directy from a pointer and julia copies it.
-}

-- As of GHC 7.10.2 linking against libjulia at runtime does not work. See https://ghc.haskell.org/trac/ghc/ticket/10458.
-- Instead we use dlopen and friends
-- TODO: precompile julia functions

-- | A exception thrown by Julia; wraps a Julia object.
data JuliaException = JuliaException JLVal deriving (Show)
instance Exception JuliaException

data RawJuliaException = RawJuliaException (Ptr JLVal) deriving (Show)
instance Exception RawJuliaException

-- | A Julia value
newtype JLVal = JLVal (ForeignPtr JLVal)

-- | A Julia function
newtype JLFunc = JLFunc (ForeignPtr JLFunc)

instance Show JLVal where
  show v = "julia: " ++ showJL v

instance Show JLFunc where
  show (JLFunc p) = "julia: " ++ (showJL $ JLVal $ castForeignPtr p)

-- TODO: noinline?
-- | Show for Julia values
showJL :: JLVal -> String
showJL v = unsafePerformIO $ do
  js <- jlCallFunction "HaskellGC.show" [v]
  vp <- jlVoidPtr js
  peekCString $ castPtr vp

-- | Get a raw pointer to a Julia object. Use with caution.
jlVoidPtr :: JLVal -> IO (Ptr ())
jlVoidPtr (JLVal v) = withForeignPtr v (jlUnboxVoidPtr' . castPtr)
  where
    jlUnboxVoidPtr' v = callJuliaUnsafe jl_unbox_voidpointer (retPtr retVoid) [argPtr v]

-- | The Julia runtime is not threadsafe. This global lock makes sure we don't
-- access concurrently
{-# NOINLINE juliaLock #-}
juliaLock :: MVar ()
juliaLock = unsafePerformIO $ do
  jlInit $ Just "/usr/local/lib/" -- TODO: automatically find
  newMVar ()

-- TODO: this lock could deadlock...
-- | Call a Julia c-interface function. Checks exceptions thrown by Julia.
-- Values returned by this function are not managed by the garbage collector
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
    throw $ RawJuliaException $ castPtr e
  return x

-- | Call a Julia c-interface function and make the result be managed by the
-- garbage collector
callJulia :: FunPtr c -> [Arg] -> IO JLVal
callJulia a b = do
  x <- handle f $ callJuliaUnsafe a (retPtr retVoid) b
  -- TODO: check if result is null and dont push gc
  jlGCPush $ castPtr x
  where
    f :: RawJuliaException -> IO a
    f (RawJuliaException e) = jlGCPush e >>= throw . JuliaException

-- julia library and function ptrs. We cache them for performance

-- | The Julia library. Dynamically loaded at runtime because it requires RTLD_GLOBAL. This library is not thread safe.
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

-- | Julia C funtion to box a 'Ptr ()'. Used a primitive for boxing datatypes.
{-# NOINLINE jl_box_voidpointer #-}
jl_box_voidpointer = unsafePerformIO $ dlsym libjulia "jl_box_voidpointer"

{-# NOINLINE jl_unbox_voidpointer #-}
jl_unbox_voidpointer = unsafePerformIO $ dlsym libjulia "jl_unbox_voidpointer"

{-# NOINLINE jl_exception_occurred #-}
jl_exception_occurred = unsafePerformIO $ dlsym libjulia "jl_exception_occurred"

{-# NOINLINE jl_gc_add_finalizer #-}
jl_gc_add_finalizer = unsafePerformIO $ dlsym libjulia "jl_gc_add_finalizer"

{-# NOINLINE jl_array_data #-}
jl_array_data = unsafePerformIO $ dlsym Default "jl_array_data1"

-- | Julia C function to box an 'Int64'
{-# NOINLINE jl_box_int64 #-}
jl_box_int64 = unsafePerformIO $ dlsym libjulia "jl_box_int64"

-- | Julia C function to unbox an 'Int64'
{-# NOINLINE jl_unbox_int64 #-}
jl_unbox_int64 = unsafePerformIO $ dlsym libjulia "jl_unbox_int64"

foreign import ccall "atexit" atexit :: FunPtr () -> IO ()

-- | Initializes julia runtime and sets up garbage collector
jlInit :: Maybe FilePath -> IO ()
jlInit s = do
  -- load dynamic lib
  jl_init <- dlsym libjulia "jl_init"
  case s of
    Just s' -> callFFI jl_init retVoid [argString s'] -- TODO: check for exception?
    Nothing -> callFFI jl_init retVoid [argPtr nullPtr]
  -- TODO: inline this file here? jl_load_file_string?
  -- set up global array to hold haskell references to julia
  callFFI jl_eval_string (retPtr retVoid) [argString "push!(LOAD_PATH, \"./julia\"); import HaskellGC"]
  -- Install julia's atexit hook
  jl_atexit_hook <- dlsym libjulia "jl_atexit_hook"
  atexit jl_atexit_hook
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
jlGCPush :: Ptr JLVal -> IO JLVal
jlGCPush p = do
  retain <- callJuliaUnsafe jl_eval_string (retPtr retVoid) [argString "HaskellGC.retain"]
  i <- callJuliaUnsafe jl_call1 (retPtr retVoid) [argPtr retain, argPtr p]
  i' <- callJuliaUnsafe jl_unbox_int64 retInt64 [argPtr i]
  fp <- newForeignPtr_ (castPtr p)
  addForeignPtrConcFinalizer fp (jlGCPop i')
  return $ JLVal fp

-- jlGCPush :: JL a => a -> IO ()
-- jlGCPush = do

-- | Evaluate a 'String' in Julia.
jlEvalString :: String -> IO JLVal
jlEvalString s = callJulia jl_eval_string [argString s]

-- XXX: brittle
-- jlMainModule = jlEvalString "Main"

-- TODO: check result is function
jlGetFunction :: String -> IO JLFunc
jlGetFunction s = do
  JLVal v <- jlEvalString s
  return $ JLFunc $ castForeignPtr v

-- | Call a Julia function with the given arguements. Arguement lengths must
-- match.
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
