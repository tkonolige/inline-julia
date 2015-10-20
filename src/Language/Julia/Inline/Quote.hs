{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-| A QuasiQuoter for generating callable Julia functions in Haskell.
-}
module Language.Julia.Inline.Quote (
   julia
 , JLConvertable(..)
 , hsVoidPtr
 , hsString
 , hsCString
 , JLVal
 ) where

import           Language.Julia.Inline.InternalDynamic

import           Control.Exception                     as E
import           Foreign.C.String
import           Foreign.LibFFI
import           Foreign.Ptr
import           System.IO.Unsafe

import           Language.Haskell.Meta.Parse
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Text.Megaparsec                       as P
import           Text.Megaparsec.String

import           Data.List
import           Data.Maybe

-- | Type class to determine the name of a Haskell type in Julia
class JLConvertable a where
  jlType :: a      -- ^ @a@ is not used
         -> String -- ^ 'String' represemtation of Julia type
  toJL :: a -> IO JLVal -- | convert @a@ to a Julia value


parseJulia :: String -> IO JLVal
parseJulia s = do
  boxed <- hsString s
  jlCallFunction "parse" [boxed]

-- TODO: better error messages
data Token = TString Char
           | TPat String
           | TSingle String
           deriving (Eq, Show)

-- Parser for quasiquote string
antiquoter :: Parsec String [Token]
antiquoter = many (P.try antiquote <|> P.try singlequote <|> (TString <$> anyChar)) <* eof
  where
    singlequote = char '$' *> (TSingle <$>
      ((:) <$> lowerChar <*> many (alphaNumChar <|> char '\'')))
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
    vars = catMaybes $ map f ts
      where
        f (TString _) = Nothing
        f (TPat s) = Just $ failLeft $ parseExp s
        f (TSingle s) = Just $ appE [| toJL |] (failLeft $ parseExp s)

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
    go ((TSingle _):xs) i = let (a, x) = go xs (i+1)
                                arg = "__hs_" ++ show i
                            in (arg : a, arg ++ x)

-- | A 'QuasiQuoter' for Julia functions, arguments are passed in with @$()@.
-- Arguments provided must be of type 'IO'
-- 'Language.Julia.Inline.InternalDynamic.JLVal'. Look in
-- "Language.Julia.Inline.Marshal" for marshaling data to and from Julia.
-- Arguments can be used with the default marshaler by using @$varname@.
-- Explicit marshalers use the @$(marshaler varname)@ syntax.
--
-- A couple examples:
--
-- >> let x = [1,2,3] :: [Int]
-- >> [julia| map(x -> x+1, $x) |]
-- > julia: [2,3,4]
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
-- The passed string cannot contain NULL or invalid ASCII characters
hsString :: String -> IO JLVal
hsString s = withCString s hsCString

-- | Box a 'CString' as a Julia Value
-- The passed string cannot contain NULL or invalid ASCII characters
hsCString :: CString -> IO JLVal
hsCString cs = do
  js <- hsVoidPtr $ castPtr cs
  jlCallFunction "x -> bytestring(convert(Ptr{UInt8}, x))" [js]
