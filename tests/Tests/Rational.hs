{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.Rational (tests) where

import Prelude

import Data.Ratio (Ratio, (%))
import Data.String.Interpolate (__i)
import GHC.TypeError (Assert)
import GHC.TypeLits (Nat, type (*), type (^), type Mod, type Div)
import Test.Tasty (TestTree, askOption, testGroup)
import Test.Tasty.HUnit (testCase, testCaseInfo, (@?=))
import Test.Tasty.ShouldError (DebugGhc (..), assertCompileError)

-- Fixed-point types for testing
data SFixed (i :: Nat) (f :: Nat) = SFixed Rational
  deriving (Eq, Show)

data UFixed (i :: Nat) (f :: Nat) = UFixed Rational
  deriving (Eq, Show)

-- Helper to create SFixed from rational
mkSFixed :: Rational -> SFixed i f
mkSFixed = SFixed

-- Helper to create UFixed from rational
mkUFixed :: Rational -> UFixed i f
mkUFixed = UFixed

-- SFixed instances for positive rational literals
instance
  ( Assert
      (RationalFitsInIntegerBits num den i)
      (FixedRangeError num den i (SFixed i f))
  , Assert
      (CanRepresentExactly num den f)
      (FixedPrecisionError num den f (SFixed i f))
  ) =>
  SafePositiveRationalLiteral num den (SFixed i f)

-- SFixed instances for negative rational literals
instance
  ( Assert
      (RationalFitsInIntegerBits num den i)
      (FixedRangeError num den i (SFixed i f))
  , Assert
      (CanRepresentExactly num den f)
      (FixedPrecisionError num den f (SFixed i f))
  ) =>
  SafeNegativeRationalLiteral num den (SFixed i f)

-- UFixed instances for positive rational literals
instance
  ( Assert
      (RationalFitsInIntegerBits num den i)
      (FixedRangeError num den i (UFixed i f))
  , Assert
      (CanRepresentExactly num den f)
      (FixedPrecisionError num den f (UFixed i f))
  ) =>
  SafePositiveRationalLiteral num den (UFixed i f)

-- UFixed instances for negative rational literals (always fails for unsigned)
instance
  (NegativeRationalError num den (UFixed i f)) =>
  SafeNegativeRationalLiteral num den (UFixed i f)

-- Test values for Float
testFloat1 :: Float
testFloat1 = 3.14

testFloat2 :: Float
testFloat2 = -2.5

-- Test values for Double
testDouble1 :: Double
testDouble1 = 3.14159

testDouble2 :: Double
testDouble2 = -1.5

-- Test values for Ratio Integer
testRatio1 :: Ratio Integer
testRatio1 = 3.5

testRatio2 :: Ratio Integer
testRatio2 = -7.25

-- Test values for SFixed
testSFixed1 :: SFixed 8 8  -- 0.5 should work with 8 fractional bits
testSFixed1 = 0.5

testSFixed2 :: SFixed 8 8  -- 0.25 should work
testSFixed2 = 0.25

testSFixed3 :: SFixed 8 2  -- 0.5 should work with 2 fractional bits
testSFixed3 = 0.5

-- Test values for UFixed
testUFixed1 :: UFixed 8 8  -- 0.5 should work
testUFixed1 = 0.5

testUFixed2 :: UFixed 8 8  -- 3.75 should work
testUFixed2 = 3.75

tests :: TestTree
tests =
  testGroup
    "Rational"
    [ testGroup
        "should-work"
        [ testCase "Float ~ 3.14" (testFloat1 @?= 3.14)
        , testCase "Float ~ -2.5" (testFloat2 @?= -2.5)
        , testCase "Double ~ 3.14159" (testDouble1 @?= 3.14159)
        , testCase "Double ~ -1.5" (testDouble2 @?= -1.5)
        , testCase "Ratio Integer ~ 3.5" (testRatio1 @?= (7 % 2))
        , testCase "Ratio Integer ~ -7.25" (testRatio2 @?= (-29 % 4))
        , testCase "SFixed 8 8 ~ 0.5" (testSFixed1 @?= mkSFixed (1 % 2))
        , testCase "SFixed 8 8 ~ 0.25" (testSFixed2 @?= mkSFixed (1 % 4))
        , testCase "SFixed 8 2 ~ 0.5" (testSFixed3 @?= mkSFixed (1 % 2))
        , testCase "UFixed 8 8 ~ 0.5" (testUFixed1 @?= mkUFixed (1 % 2))
        , testCase "UFixed 8 8 ~ 3.75" (testUFixed2 @?= mkUFixed (15 % 4))
        ]
    , testGroup
        "should-error"
        [ askOption $ \(DebugGhc debug) ->
            testCaseInfo "SFixed precision error (1/3 not representable)" $
              assertCompileError
                debug
                [__i|
                  import Prelude
                  import GHC.TypeLits
                  import GHC.TypeError
                  import SafeLiterals
                  data SFixed (i :: Nat) (f :: Nat) = SFixed Rational

                  type family CanRepresentExactly (num :: Nat) (den :: Nat) (f :: Nat) :: Bool where
                    CanRepresentExactly num den f = ((num * (2 ^ f)) `Mod` den) <=? 0

                  type FixedPrecisionError num den f typ =
                    TypeError
                      ( 'Text "Rational literal "
                          ':<>: 'ShowType num
                          ':<>: 'Text " / "
                          ':<>: 'ShowType den
                          ':<>: 'Text " cannot be represented exactly in "
                          ':<>: 'ShowType typ
                          ':<>: 'Text "."
                          ':$$: 'Text "The denominator "
                            ':<>: 'ShowType den
                            ':<>: 'Text " is not a divisor of 2^"
                            ':<>: 'ShowType f
                            ':<>: 'Text "."
                          ':$$: 'Text "Possible fix: increase the fractional bits or use 'uncheckedLiteral'."
                      )

                  type family RationalFitsInIntegerBits (num :: Nat) (den :: Nat) (i :: Nat) :: Bool where
                    RationalFitsInIntegerBits num den i = num <=? (den * (2 ^ i))

                  type FixedRangeError num den i typ =
                    TypeError
                      ( 'Text "Rational literal "
                          ':<>: 'ShowType num
                          ':<>: 'Text " / "
                          ':<>: 'ShowType den
                          ':<>: 'Text " is too large for "
                          ':<>: 'ShowType typ
                          ':<>: 'Text "."
                          ':$$: 'Text "The integer part requires more than "
                            ':<>: 'ShowType i
                            ':<>: 'Text " bits."
                          ':$$: 'Text "Possible fix: increase the integer bits or use 'uncheckedLiteral'."
                      )

                  instance
                    ( Assert
                        (RationalFitsInIntegerBits num den i)
                        (FixedRangeError num den i (SFixed i f))
                    , Assert
                        (CanRepresentExactly num den f)
                        (FixedPrecisionError num den f (SFixed i f))
                    ) =>
                    SafePositiveRationalLiteral num den (SFixed i f)

                  test :: SFixed 8 8
                  test = 0.333333333333
                |]
                [ "cannot be represented exactly"
                , "not a divisor of 2^8"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "SFixed range error (value too large)" $
              assertCompileError
                debug
                [__i|
                  import Prelude
                  import GHC.TypeLits
                  import GHC.TypeError
                  import SafeLiterals
                  data SFixed (i :: Nat) (f :: Nat) = SFixed Rational

                  type family CanRepresentExactly (num :: Nat) (den :: Nat) (f :: Nat) :: Bool where
                    CanRepresentExactly num den f = ((num * (2 ^ f)) `Mod` den) <=? 0

                  type FixedPrecisionError num den f typ =
                    TypeError
                      ( 'Text "Rational literal "
                          ':<>: 'ShowType num
                          ':<>: 'Text " / "
                          ':<>: 'ShowType den
                          ':<>: 'Text " cannot be represented exactly in "
                          ':<>: 'ShowType typ
                          ':<>: 'Text "."
                          ':$$: 'Text "The denominator "
                            ':<>: 'ShowType den
                            ':<>: 'Text " is not a divisor of 2^"
                            ':<>: 'ShowType f
                            ':<>: 'Text "."
                          ':$$: 'Text "Possible fix: increase the fractional bits or use 'uncheckedLiteral'."
                      )

                  type family RationalFitsInIntegerBits (num :: Nat) (den :: Nat) (i :: Nat) :: Bool where
                    RationalFitsInIntegerBits num den i = num <=? (den * (2 ^ i))

                  type FixedRangeError num den i typ =
                    TypeError
                      ( 'Text "Rational literal "
                          ':<>: 'ShowType num
                          ':<>: 'Text " / "
                          ':<>: 'ShowType den
                          ':<>: 'Text " is too large for "
                          ':<>: 'ShowType typ
                          ':<>: 'Text "."
                          ':$$: 'Text "The integer part requires more than "
                            ':<>: 'ShowType i
                            ':<>: 'Text " bits."
                          ':$$: 'Text "Possible fix: increase the integer bits or use 'uncheckedLiteral'."
                      )

                  instance
                    ( Assert
                        (RationalFitsInIntegerBits num den i)
                        (FixedRangeError num den i (SFixed i f))
                    , Assert
                        (CanRepresentExactly num den f)
                        (FixedPrecisionError num den f (SFixed i f))
                    ) =>
                    SafePositiveRationalLiteral num den (SFixed i f)

                  test :: SFixed 2 8
                  test = 256.5
                |]
                [ "too large"
                , "requires more than 2 bits"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "UFixed negative error" $
              assertCompileError
                debug
                [__i|
                  import Prelude
                  import GHC.TypeLits
                  import GHC.TypeError
                  import SafeLiterals
                  data UFixed (i :: Nat) (f :: Nat) = UFixed Rational

                  type NegativeRationalError num den typ =
                    TypeError
                      ( 'Text "Negative rational literal -"
                          ':<>: 'ShowType num
                          ':<>: 'Text " / "
                          ':<>: 'ShowType den
                          ':<>: 'Text " is out of bounds for "
                          ':<>: 'ShowType typ
                          ':<>: 'Text "."
                          ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
                      )

                  instance
                    (NegativeRationalError num den (UFixed i f)) =>
                    SafeNegativeRationalLiteral num den (UFixed i f)

                  test :: UFixed 8 8
                  test = -0.5
                |]
                [ "Negative rational literal"
                , "out of bounds"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Ratio Word8 non-integer error" $
              assertCompileError
                debug
                [__i|
                  import Prelude
                  import Data.Word
                  import Data.Ratio
                  test :: Ratio Word8
                  test = 3.14
                |]
                [ "is not an integer"
                , "requires an integer value"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word8 non-integer error" $
              assertCompileError
                debug
                [__i|
                  import Prelude
                  import Data.Word
                  test :: Word8
                  test = 2.5
                |]
                [ "is not an integer"
                , "requires an integer value"
                ]
        ]
    ]
