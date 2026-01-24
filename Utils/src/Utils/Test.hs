module Utils.Test (
    assertEqualDouble,
    assertEqualFloat,
    assertEqualDoubleList,
    assertEqualFloatList,
    assertEqualApprox,
    assertEqualApproxList
    ) where

import Test.Tasty.HUnit

assertEqualDouble :: Double -> Double -> Assertion
assertEqualDouble x y =
    assertEqual "" (fromIntegral (round (x * 1e11) :: Integer) * 1e-11 :: Double) (fromIntegral (round (y * 1e11) :: Integer) * 1e-11 :: Double)

assertEqualFloat :: Double -> Double -> Assertion
assertEqualFloat x y =
    assertEqual "" (fromIntegral (round (x * 1e6) :: Integer) * 1e-6 :: Double) (fromIntegral (round (y * 1e6) :: Integer) * 1e-6 :: Double)

assertEqualDoubleList :: [Double] -> [Double] -> Assertion
assertEqualDoubleList [] [] = return ()
assertEqualDoubleList (x:xs) (y:ys) = do
    assertEqualDouble x y
    assertEqualDoubleList xs ys
assertEqualDoubleList _ _ = assertFailure "Lists have different lengths"

assertEqualFloatList :: [Double] -> [Double] -> Assertion
assertEqualFloatList [] [] = return ()
assertEqualFloatList (x:xs) (y:ys) = do
    assertEqualFloat x y
    assertEqualFloatList xs ys
assertEqualFloatList _ _ = assertFailure "Lists have different lengths"

assertEqualApprox :: Double -> Double -> Double -> Assertion
assertEqualApprox epsilon x y =
    assertEqual "" (fromIntegral (round (x / epsilon) :: Integer) * epsilon :: Double) (fromIntegral (round (y / epsilon) :: Integer) * epsilon :: Double)

assertEqualApproxList :: Double -> [Double] -> [Double] -> Assertion
assertEqualApproxList _ [] [] = return ()
assertEqualApproxList epsilon (x:xs) (y:ys) = do
    assertEqualApprox epsilon x y
    assertEqualApproxList epsilon xs ys
assertEqualApproxList _ _ _ = assertFailure "Lists have different lengths"
