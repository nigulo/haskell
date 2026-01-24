module Math.IODoubleLinearEquationsTest (tests) where

import Math.IODoubleLinearEquations
import Math.IODoubleVector as Vector
import Math.IODoubleMatrix as Matrix
import Utils.Test
import Test.Tasty
import Test.Tasty.HUnit

test_solveGauss :: Assertion
test_solveGauss = do
    -- System of equations:
    -- 2x +  y +  3z = 1
    -- 2x + 6y +  8z = 3
    -- 6x + 8y + 18z = 5
    -- Solution:
    -- (x, y, z) = (3/10, 2/5, 0)
    m <- matrix [
        [2, 1, 3, 1],
        [2, 6, 8, 3],
        [6, 8, 18, 5]
        ]
    let
        expectedResult = [3/10, 2/5, 0]
    result <- solveGauss m >>= values
    Utils.Test.assertEqualDoubleList expectedResult result

tests :: TestTree
tests = testGroup "Math.IODoubleLinearEquationsTest"
    [ testCase "solveGauss" test_solveGauss
    ]
