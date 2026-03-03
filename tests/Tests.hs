module Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import Tests.Rational qualified as Rational
import Tests.Unsigned qualified as Unsigned
import Tests.Word qualified as Word

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Word.tests
    , Unsigned.tests
    , Rational.tests
    ]
