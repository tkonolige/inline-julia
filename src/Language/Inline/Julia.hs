{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.Inline.Julia where

import Language.Inline.Julia.InternalDynamic
import System.IO.Unsafe
import Foreign.C.String
import Foreign.Ptr

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
parseJulia s = withCString s $ \x -> do
  boxed <- jlBoxVoidPtr $ castPtr x
  jlCallFunction "x -> parse(bytestring(convert(Ptr{Uint8}, x)))" [boxed]

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
mkCall ts = [|jlCallFunction|] `appE` (stringE $ mkJLFunc ts) `appE` (listE vars)
  where
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

--   do
--   space
--   r <- P.try marshaler <|> TVar <$> var
--   space
--   return r
-- var = do
--   start <- lowerChar
--   rest <- many $ alphaNumChar <|> char '\'' <|> char '_'
--   return $ start : rest
-- marshaler = do
--   m <- var
--   skipSome spaceChar
--   v <- var
--   return $ TMarshal m v

julia :: QuasiQuoter
julia = QuasiQuoter { quoteExp = parseJulia' }
  where
    parseJulia' s = do
      let Right tks = parse parser "" s
      p <- runIO $ E.try $ parseJulia $ mkJLFunc tks
      case p of
        Left (JuliaException e) -> fail $ showJL e
        Right parsed -> do
          mkCall tks
          -- return type, usually Any
          -- retType <- runIO $ jlGetFunction "x -> x.typ" >>= (\f -> jlCall1 f parsed)
          -- appE [| jlEvalString |] [| s |]

-- [julia| println($("hi")) |]
-- [julia| x :: ASCIIString -> println(x) |] "hi"
