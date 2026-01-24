module Math.IODoubleVectorTest (tests) where

import qualified Math.IODoubleVector as V
import Test.Tasty
import Test.Tasty.HUnit

test_elemOp :: Assertion
test_elemOp = do
    v <- V.vector [0, 1 .. 9]
    let
        expectedResult = [1, 2 .. 10]
    V.elemOp (+1) v
    result <- V.values v
    assertEqual "" expectedResult result

tests :: TestTree
tests = testGroup "Math.IODoubleVectorTest"
    [ testCase "elemOp" test_elemOp
    ]
