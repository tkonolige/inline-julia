{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC

import           Data.Int
import           Data.MonoTraversable
import qualified Data.Vector.Storable      as V

import           Language.Julia.Inline

main = defaultMain qcProps

instance (V.Storable a, Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

qcProps = testGroup "round trip"
  [ testRoundtrip hsInt8           jlInt8           "Int8"           Nothing
  , testRoundtrip hsInt16          jlInt16          "Int16"          Nothing
  , testRoundtrip hsInt32          jlInt32          "Int32"          Nothing
  , testRoundtrip hsInt64          jlInt64          "Int64"          Nothing
  , testRoundtrip hsWord8          jlWord8          "Word8"          Nothing
  , testRoundtrip hsWord16         jlWord16         "Word16"         Nothing
  , testRoundtrip hsWord32         jlWord32         "Word32"         Nothing
  , testRoundtrip hsWord64         jlWord64         "Word64"         Nothing
  , testRoundtrip hsFloat          jlFloat          "Float"          Nothing
  , testRoundtrip hsDouble         jlDouble         "Double"         Nothing
  , testRoundtrip hsString         jlString         "String"         (Just noNul)
  , testRoundtrip hsByteString     jlByteString     "ByteString"     Nothing
  , testRoundtrip hsLazyByteString jlLazyByteString "LazyByteString" Nothing
  -- , testRoundtrip hsMVector jlMVector "MVector" Nothing
  , testRoundtrip (hsVector :: V.Vector Int64 -> IO JLVal) jlVector "Vector" Nothing
  , testRoundtrip (hsList :: [Int64] -> IO JLVal) jlList "List" Nothing
  ]
    where
      noNul :: (Enum (Element a), MonoFoldableEq a) => a -> Bool
      noNul = oall (/= toEnum 0) -- julia does not allow nul values in c strings (fixed in julia v0.5)
      testRoundtrip :: (Arbitrary a, Show a, Eq a) => (a -> IO JLVal) -> (JLVal -> IO a)
                    -> String -> Maybe (a -> Bool) -> TestTree
      testRoundtrip to from name prec = QC.testProperty name $ \x -> monadicIO $ do
        case prec of
          Just p -> pre $ p x
          Nothing -> return ()
        x' <- run $ to x >>= from
        assert $ x' == x
