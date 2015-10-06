{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.Julia.Inline.Quote where

import Language.Julia.Inline.InternalDynamic

import System.IO.Unsafe
import Foreign.C.String
import Foreign.Ptr
import Foreign.LibFFI
import Control.Exception as E

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta.Parse

import Text.Megaparsec as P
import Text.Megaparsec.String

import Data.List
import Data.Maybe

parseJulia :: String -> IO JLVal
parseJulia s = do
  boxed <- hsString s
  jlCallFunction "parse" [boxed]

println :: JLVal -> IO ()
println v = jlCallFunction "println" [v] >> return ()

-- | Parse a string for antiquotes ($()) and return them and a new string which
-- is a julia function that takes the antiquotes as arguements
parseAntiQuote :: String -> ([a], String)
parseAntiQuote input = undefined

-- TODO: better error messages
data Token = TString String
           | TPat String
           -- | TMarshal String String
           deriving (Eq, Show)

parser :: Parsec String [Token]
parser = do
  r <- many $ antiquote <|>
    TString <$> someTill anyChar (eof <|> ((lookAhead $ string "$(") >> return ()))
  eof
  return r
antiquote = between (string "$(") (char ')') $ TPat <$> many (satisfy (/=')'))

mkCall :: [Token] -> ExpQ
mkCall ts = [| sequence $(listE vars) >>= $(callFunc) |]
  where
    callFunc = appE [|jlCallFunction|] (stringE $ mkJLFunc ts)
    failLeft (Right a) = return a
    failLeft (Left a) = fail a
    vars = map (failLeft . parseExp) $ catMaybes $ map isPat ts
    isPat (TString _) = Nothing
    isPat (TPat s) = Just s

mkJLFunc :: [Token] -> String
mkJLFunc ts = "(" ++ intercalate "," args ++ ") -> " ++ body
  where
    (args, body) = go ts 0
    go [] i = ([], [])
    go ((TString s):xs) i = let (a, x) = go xs i
                             in (a, s ++ x)
    go ((TPat _):xs) i = let (a, x) = go xs (i+1)
                             arg = "__hs_" ++ show i
                          in (arg : a, arg ++ x)

julia :: QuasiQuoter
julia = QuasiQuoter { quoteExp = parseJulia' }
  where
    parseJulia' s = do
      let Right tks = parse parser "" s
      p <- runIO $ E.try $ parseJulia $ mkJLFunc tks
      case p of
        Left (JuliaException e) -> fail $ showJL e
        Right parsed -> mkCall tks

hsVoidPtr :: Ptr () -> IO JLVal
hsVoidPtr i = callJulia jl_box_voidpointer [argPtr i]

hsString :: String -> IO JLVal
hsString s = withCString s $ \cs -> do
  js <- hsVoidPtr $ castPtr cs
  jlCallFunction "x -> bytestring(convert(Ptr{UInt8}, x))" [js]
