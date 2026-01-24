module Math.LinearEquationsTest (tests) where

import Math.LinearEquations
import Math.Vector as Vector
import Math.Matrix as Matrix
import Utils.Test
import Test.Tasty
import Test.Tasty.HUnit

test_solveGauss :: Assertion
test_solveGauss = do
    let
        -- System of equations:
        -- 2x +  y +  3z = 1
        -- 2x + 6y +  8z = 3
        -- 6x + 8y + 18z = 5
        -- Solution:
        -- (x, y, z) = (3/10, 2/5, 0)
        m = matrix [
            [2, 1, 3, 1],
            [2, 6, 8, 3],
            [6, 8, 18, 5]
            ]
        expectedResult = [3/10, 2/5, 0]
        result = values $ solveGauss m
    Utils.Test.assertEqualDoubleList expectedResult result

test_backSubstitutionIO :: Assertion
test_backSubstitutionIO = do
    let
        m = matrix [
            [2, 1, 3, 1],
            [2, 6, 8, 3],
            [6, 8, 18, 5]
            ]
        expectedResult = [3/10, 2/5, 0]
        m1 = f m where
            f = foldr1 (.) [calcForward i | i <- [2, 1, 0]]
    v <- backSubstitutionIO m1
    let
        result = values v
    Utils.Test.assertEqualDoubleList expectedResult result

tests :: TestTree
tests = testGroup "Math.LinearEquationsTest"
    [ testCase "solveGauss" test_solveGauss
    , testCase "backSubstitutionIO" test_backSubstitutionIO
    ]
