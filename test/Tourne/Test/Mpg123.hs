-- | Tests for the Mpg123Encoding <-> CInt mapping.
module Tourne.Test.Mpg123 (tests) where

import Relude
import Foreign.C.Types (CInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Tourne.Audio.Types

tests :: [TestTree]
tests =
  [ testGroup "Mpg123Encoding"
  [ testGroup "mpg123EncToCInt"
    [ testCase "Mpg123EncSigned16" $
        mpg123EncToCInt Mpg123EncSigned16  @?= 0x8000
    , testCase "Mpg123EncUnsigned8" $
        mpg123EncToCInt Mpg123EncUnsigned8 @?= 0x0400
    , testCase "Mpg123EncSigned8" $
        mpg123EncToCInt Mpg123EncSigned8   @?= 0x0080
    , testCase "Mpg123EncFloat32" $
        mpg123EncToCInt Mpg123EncFloat32   @?= 0xe000
    ]
  , testGroup "cIntToMpg123Enc"
    [ testCase "0x8000 -> Mpg123EncSigned16" $
        cIntToMpg123Enc 0x8000 @?= Just Mpg123EncSigned16
    , testCase "0x0400 -> Mpg123EncUnsigned8" $
        cIntToMpg123Enc 0x0400 @?= Just Mpg123EncUnsigned8
    , testCase "0x0080 -> Mpg123EncSigned8" $
        cIntToMpg123Enc 0x0080 @?= Just Mpg123EncSigned8
    , testCase "0xe000 -> Mpg123EncFloat32" $
        cIntToMpg123Enc 0xe000 @?= Just Mpg123EncFloat32
    , testCase "unknown CInt -> Nothing" $
        cIntToMpg123Enc 0x1234 @?= (Nothing :: Maybe Mpg123Encoding)
    ]
  , testGroup "round-trip"
    [ testCase "cIntToMpg123Enc . mpg123EncToCInt is identity on the known set" $ do
        forM_ [minBound..maxBound :: Mpg123Encoding] $ \enc ->
          cIntToMpg123Enc (mpg123EncToCInt enc) @?= Just enc
    ]
  , testGroup "backwards-compat aliases"
    [ testCase "mpg123EncSigned16 == mpg123EncToCInt Mpg123EncSigned16" $
        mpg123EncSigned16 @?= mpg123EncToCInt Mpg123EncSigned16
    , testCase "mpg123EncFloat32 == mpg123EncToCInt Mpg123EncFloat32" $
        mpg123EncFloat32 @?= mpg123EncToCInt Mpg123EncFloat32
    ]
  ]
  ]
