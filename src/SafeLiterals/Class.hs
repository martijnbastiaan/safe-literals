{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeLiterals.Class where

import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Monoid (Ap, Product, Sum)
import Data.Ord (Down)
import Data.Ratio (Ratio)
import Data.Semigroup (Max, Min)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeNats (Div, Log2, Nat, (*), type (<=?), type (^), type Mod)
import Numeric.Natural (Natural)

import SafeLiterals.Class.TemplateHaskell (maxBoundAsNat, minBoundAsNat)

--------------------------------------------------------------------------------
-- Integer Literal Support
--------------------------------------------------------------------------------

type PositiveUnsignedError lit typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

type PositiveSignedError lit typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: ["
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

class SafePositiveIntegerLiteral (lit :: Nat) (a :: Type)

instance SafePositiveIntegerLiteral lit Natural
instance
  (Assert (lit <=? $(maxBoundAsNat @Word)) (PositiveUnsignedError lit Word $(maxBoundAsNat @Word))) =>
  SafePositiveIntegerLiteral lit Word
instance
  (Assert (lit <=? $(maxBoundAsNat @Word8)) (PositiveUnsignedError lit Word8 $(maxBoundAsNat @Word8))) =>
  SafePositiveIntegerLiteral lit Word8
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Word16))
      (PositiveUnsignedError lit Word16 $(maxBoundAsNat @Word16))
  ) =>
  SafePositiveIntegerLiteral lit Word16
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Word32))
      (PositiveUnsignedError lit Word32 $(maxBoundAsNat @Word32))
  ) =>
  SafePositiveIntegerLiteral lit Word32
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Word64))
      (PositiveUnsignedError lit Word64 $(maxBoundAsNat @Word64))
  ) =>
  SafePositiveIntegerLiteral lit Word64

instance SafePositiveIntegerLiteral lit Integer
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Int))
      (PositiveSignedError lit Int $(minBoundAsNat @Int) $(maxBoundAsNat @Int))
  ) =>
  SafePositiveIntegerLiteral lit Int
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Int8))
      (PositiveSignedError lit Int8 $(minBoundAsNat @Int8) $(maxBoundAsNat @Int8))
  ) =>
  SafePositiveIntegerLiteral lit Int8
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Int16))
      (PositiveSignedError lit Int16 $(minBoundAsNat @Int16) $(maxBoundAsNat @Int16))
  ) =>
  SafePositiveIntegerLiteral lit Int16
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Int32))
      (PositiveSignedError lit Int32 $(minBoundAsNat @Int32) $(maxBoundAsNat @Int32))
  ) =>
  SafePositiveIntegerLiteral lit Int32
instance
  ( Assert
      (lit <=? $(maxBoundAsNat @Int64))
      (PositiveSignedError lit Int64 $(minBoundAsNat @Int64) $(maxBoundAsNat @Int64))
  ) =>
  SafePositiveIntegerLiteral lit Int64

-- Not clear what to do with edge cases:
instance
  ( TypeError
      ( 'Text "Float unsupported by `safe-literals`."
          ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0."
      )
  ) =>
  SafePositiveIntegerLiteral lit Float
instance
  ( TypeError
      ( 'Text "Double unsupported by `safe-literals`."
          ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0."
      )
  ) =>
  SafePositiveIntegerLiteral lit Double

instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Complex a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Ratio a)

instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Max a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Min a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Identity a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Down a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Product a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Sum a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Const a b)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Ap f a)

safePositiveIntegerLiteral :: (SafePositiveIntegerLiteral lit a) => a -> a
safePositiveIntegerLiteral = id

type NegativeUnsignedError lit typ maxVal =
  TypeError
    ( 'Text "Negative literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

type NegativeNaturalError lit typ =
  TypeError
    ( 'Text "Negative literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. ∞]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

type NegativeSignedError lit typ minVal maxVal =
  TypeError
    ( 'Text "Negative literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

class SafeNegativeIntegerLiteral (lit :: Nat) (a :: Type)

instance (NegativeNaturalError lit Natural) => SafeNegativeIntegerLiteral lit Natural
instance (NegativeUnsignedError lit Word $(maxBoundAsNat @Word)) => SafeNegativeIntegerLiteral lit Word
instance (NegativeUnsignedError lit Word8 $(maxBoundAsNat @Word8)) => SafeNegativeIntegerLiteral lit Word8
instance (NegativeUnsignedError lit Word16 $(maxBoundAsNat @Word16)) => SafeNegativeIntegerLiteral lit Word16
instance (NegativeUnsignedError lit Word32 $(maxBoundAsNat @Word32)) => SafeNegativeIntegerLiteral lit Word32
instance (NegativeUnsignedError lit Word64 $(maxBoundAsNat @Word64)) => SafeNegativeIntegerLiteral lit Word64

instance SafeNegativeIntegerLiteral lit Integer
instance
  ( Assert
      (lit <=? $(minBoundAsNat @Int))
      (NegativeSignedError lit Int $(minBoundAsNat @Int) $(maxBoundAsNat @Int))
  ) =>
  SafeNegativeIntegerLiteral lit Int
instance
  ( Assert
      (lit <=? $(minBoundAsNat @Int8))
      (NegativeSignedError lit Int8 $(minBoundAsNat @Int8) $(maxBoundAsNat @Int8))
  ) =>
  SafeNegativeIntegerLiteral lit Int8
instance
  ( Assert
      (lit <=? $(minBoundAsNat @Int16))
      (NegativeSignedError lit Int16 $(minBoundAsNat @Int16) $(maxBoundAsNat @Int16))
  ) =>
  SafeNegativeIntegerLiteral lit Int16
instance
  ( Assert
      (lit <=? $(minBoundAsNat @Int32))
      (NegativeSignedError lit Int32 $(minBoundAsNat @Int32) $(maxBoundAsNat @Int32))
  ) =>
  SafeNegativeIntegerLiteral lit Int32
instance
  ( Assert
      (lit <=? $(minBoundAsNat @Int64))
      (NegativeSignedError lit Int64 $(minBoundAsNat @Int64) $(maxBoundAsNat @Int64))
  ) =>
  SafeNegativeIntegerLiteral lit Int64

-- Not clear what to do with edge cases:
instance
  ( TypeError
      ( 'Text "Float unsupported by `safe-literals`."
          ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0."
      )
  ) =>
  SafeNegativeIntegerLiteral lit Float
instance
  ( TypeError
      ( 'Text "Double unsupported by `safe-literals`."
          ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0."
      )
  ) =>
  SafeNegativeIntegerLiteral lit Double

instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Complex a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Ratio a)

instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Max a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Min a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Identity a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Down a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Product a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Sum a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Const a b)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Ap f a)

safeNegativeIntegerLiteral :: (SafeNegativeIntegerLiteral lit a) => a -> a
safeNegativeIntegerLiteral = id

--------------------------------------------------------------------------------
-- Rational Literal Support
--------------------------------------------------------------------------------

-- | Error message for positive rational literal out of bounds
type PositiveRationalError num den typ =
  TypeError
    ( 'Text "Rational literal "
        ':<>: 'ShowType num
        ':<>: 'Text " / "
        ':<>: 'ShowType den
        ':<>: 'Text " is out of bounds for "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

-- | Error message for negative rational literal out of bounds
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

-- | Type class for safe positive rational literals
class SafePositiveRationalLiteral (num :: Nat) (den :: Nat) (a :: Type)

-- | Type class for safe negative rational literals
class SafeNegativeRationalLiteral (num :: Nat) (den :: Nat) (a :: Type)

-- Float and Double instances (always succeed but may lose precision)
instance SafePositiveRationalLiteral num den Float
instance SafePositiveRationalLiteral num den Double
instance SafeNegativeRationalLiteral num den Float
instance SafeNegativeRationalLiteral num den Double

-- Integer types: require that the rational is actually an integer (den divides num)
-- and that the resulting integer fits in bounds

type IntegerFromRational num den = num `Div` den

type RationalMustBeInteger num den typ =
  TypeError
    ( 'Text "Rational literal "
        ':<>: 'ShowType num
        ':<>: 'Text " / "
        ':<>: 'ShowType den
        ':<>: 'Text " is not an integer."
        ':$$: 'ShowType typ
          ':<>: 'Text " requires an integer value."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

type family IsInteger (num :: Nat) (den :: Nat) :: Bool where
  IsInteger num den = ((num `Mod` den) <=? 0)

-- Natural
instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Natural)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Natural
  ) =>
  SafePositiveRationalLiteral num den Natural

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Natural)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Natural
  ) =>
  SafeNegativeRationalLiteral num den Natural

-- Integer
instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Integer)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Integer
  ) =>
  SafePositiveRationalLiteral num den Integer

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Integer)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Integer
  ) =>
  SafeNegativeRationalLiteral num den Integer

-- Word types
instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Word
  ) =>
  SafePositiveRationalLiteral num den Word

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Word
  ) =>
  SafeNegativeRationalLiteral num den Word

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word8)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Word8
  ) =>
  SafePositiveRationalLiteral num den Word8

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word8)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Word8
  ) =>
  SafeNegativeRationalLiteral num den Word8

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word16)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Word16
  ) =>
  SafePositiveRationalLiteral num den Word16

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word16)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Word16
  ) =>
  SafeNegativeRationalLiteral num den Word16

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word32)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Word32
  ) =>
  SafePositiveRationalLiteral num den Word32

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word32)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Word32
  ) =>
  SafeNegativeRationalLiteral num den Word32

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word64)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Word64
  ) =>
  SafePositiveRationalLiteral num den Word64

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Word64)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Word64
  ) =>
  SafeNegativeRationalLiteral num den Word64

-- Int types
instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Int
  ) =>
  SafePositiveRationalLiteral num den Int

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Int
  ) =>
  SafeNegativeRationalLiteral num den Int

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int8)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Int8
  ) =>
  SafePositiveRationalLiteral num den Int8

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int8)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Int8
  ) =>
  SafeNegativeRationalLiteral num den Int8

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int16)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Int16
  ) =>
  SafePositiveRationalLiteral num den Int16

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int16)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Int16
  ) =>
  SafeNegativeRationalLiteral num den Int16

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int32)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Int32
  ) =>
  SafePositiveRationalLiteral num den Int32

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int32)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Int32
  ) =>
  SafeNegativeRationalLiteral num den Int32

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int64)
  , SafePositiveIntegerLiteral (IntegerFromRational num den) Int64
  ) =>
  SafePositiveRationalLiteral num den Int64

instance
  ( Assert (IsInteger num den) (RationalMustBeInteger num den Int64)
  , SafeNegativeIntegerLiteral (IntegerFromRational num den) Int64
  ) =>
  SafeNegativeRationalLiteral num den Int64

-- Ratio instances - delegate to the underlying integer type
instance (SafePositiveIntegerLiteral num a, SafePositiveIntegerLiteral den a) => SafePositiveRationalLiteral num den (Ratio a)
instance (SafePositiveIntegerLiteral num a, SafePositiveIntegerLiteral den a) => SafeNegativeRationalLiteral num den (Ratio a)

-- Wrapper instances
instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Complex a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Complex a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Max a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Max a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Min a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Min a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Identity a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Identity a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Down a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Down a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Product a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Product a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Sum a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Sum a)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Const a b)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Const a b)

instance (SafePositiveRationalLiteral num den a) => SafePositiveRationalLiteral num den (Ap f a)
instance (SafeNegativeRationalLiteral num den a) => SafeNegativeRationalLiteral num den (Ap f a)

safePositiveRationalLiteral :: (SafePositiveRationalLiteral num den a) => a -> a
safePositiveRationalLiteral = id

safeNegativeRationalLiteral :: (SafeNegativeRationalLiteral num den a) => a -> a
safeNegativeRationalLiteral = id

--------------------------------------------------------------------------------
-- Fixed Point Rational Literal Support
--------------------------------------------------------------------------------

-- | Check if (num * 2^F) mod den == 0, meaning the rational can be represented exactly
type family CanRepresentExactly (num :: Nat) (den :: Nat) (f :: Nat) :: Bool where
  CanRepresentExactly num den f = ((num * (2 ^ f)) `Mod` den) <=? 0

-- | Error for rational that cannot be represented exactly in fixed-point
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

-- | Error for rational that is too large for the integer bits
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

-- | Check if num/den < 2^i (for positive values)
type family RationalFitsInIntegerBits (num :: Nat) (den :: Nat) (i :: Nat) :: Bool where
  RationalFitsInIntegerBits num den i = num <=? ((den * (2 ^ i)))
