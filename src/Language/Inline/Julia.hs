{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.Inline.Julia where

import Language.Inline.Julia.InternalDynamic
import System.IO.Unsafe
import Foreign.C.String
import Foreign.Ptr

import Control.Exception

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH

parseJulia :: String -> IO JLVal
parseJulia s = withCString s $ \x -> do
  boxed <- jlBoxVoidPtr $ castPtr x
  jlCallFunction "x -> parse(bytestring(convert(Ptr{Uint8}, x)))" [boxed]

println :: JLVal -> IO ()
println v = do
  f <- jlGetFunction "println"
  jlCall1 f v
  return ()

-- | Parse a string for antiquotes ($()) and return them and a new string which
-- is a julia function that takes the antiquotes as arguements
parseAntiQuote :: String -> ([a], String)
parseAntiQuote input = undefined

julia :: QuasiQuoter
julia = QuasiQuoter { quoteExp = parseJulia' }
  where
    parseJulia' s = do
      p <- runIO $ try $ parseJulia s
      case p of
        Left (JuliaException e) -> fail $ showJL e
        Right parsed -> do
          -- return type, usually Any
          -- retType <- runIO $ jlGetFunction "x -> x.typ" >>= (\f -> jlCall1 f parsed)
          appE [| jlEvalString |] [| s |]
