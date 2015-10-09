{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-| A QuasiQuoter for generating callable Julia functions in Haskell.
-}
module Language.Julia.Inline.Quote (
   julia
 , hsVoidPtr
 , hsString
 ) where

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

-- TODO: better error messages
data Token = TString Char
           | TPat String
           deriving (Eq, Show)

-- Parser for quasiquote string
antiquoter :: Parsec String [Token]
antiquoter = many (P.try antiquote <|> (TString <$> anyChar)) <* eof
  where
    antiquote = string "$(" *> (TPat <$> (concat <$> many parens)) <* char ')'
    parens =  (P.try $ do
      p1 <- char '('
      m <- optional $ many parens
      p2 <- char ')'
      case m of
        Just m' -> return $ p1:(concat m') ++ [p2]
        Nothing -> return $ p1:[p2]
      ) <|> some (noneOf "()")

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
                             in (a, s:x)
    go ((TPat _):xs) i = let (a, x) = go xs (i+1)
                             arg = "__hs_" ++ show i
                          in (arg : a, arg ++ x)

-- | A 'QuasiQuoter' for Julia functions, arguments are passed in with @$()@.
-- Arguments provided must be of type 'IO'
-- 'Language.Julia.Inline.InternalDynamic.JLVal'. Look in
-- "Language.Julia.Inline.Marshal" for marshaling data to and from Julia.
--
-- A couple examples:
--
-- >> [julia| println($(hsString "Hello World")) |]
-- > Hello World
-- > julia: nothing
--
-- >> [julia| 1 + $(hsInt64 2) |]
-- > julia: 3
julia :: QuasiQuoter
julia = QuasiQuoter { quoteExp = parseJulia'
                    , quotePat = fail "not implemented"
                    , quoteType = fail "not implemented"
                    , quoteDec = fail "not implemented"
                    }
  where
    parseJulia' s = do
      let Right tks = parse antiquoter "" s
      p <- runIO $ E.try $ parseJulia $ mkJLFunc tks
      case p of
        Left (JuliaException e) -> fail $ showJL e
        Right parsed -> mkCall tks

-- TODO: these should really not be here
-- | Box a 'Ptr' @()@ and pass it to Julia
hsVoidPtr :: Ptr () -> IO JLVal
hsVoidPtr i = callJulia jl_box_voidpointer [argPtr i]

-- TODO: use CStringLen
-- | Box a 'String' and pass it to Julia
hsString :: String -> IO JLVal
hsString s = withCString s $ \cs -> do
  js <- hsVoidPtr $ castPtr cs
  jlCallFunction "x -> bytestring(convert(Ptr{UInt8}, x))" [js]
