{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Utils.Test (
    assertEqualDouble, 
    assertEqualFloat,
    assertEqualDoubleList,
    assertEqualFloatList
    ) where

import Test.Framework

assertEqualDouble x y = 
    assertEqual (fromIntegral (round (x * 1e11) :: Integer) * 1e-11 :: Double) (fromIntegral (round (y * 1e11) :: Integer) * 1e-11 :: Double)        

assertEqualFloat x y = 
    assertEqual (fromIntegral (round (x * 1e6) :: Integer) * 1e-6 :: Double) (fromIntegral (round (y * 1e6) :: Integer) * 1e-6 :: Double)        

assertEqualDoubleList [] [] = return () 
assertEqualDoubleList (x:xs) (y:ys) = do 
    assertEqualDouble x y
    assertEqualDoubleList xs ys

assertEqualFloatList [] [] = return () 
assertEqualFloatList (x:xs) (y:ys) = do 
    assertEqualFloat x y
    assertEqualFloatList xs ys