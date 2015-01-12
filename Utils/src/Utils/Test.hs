{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Utils.Test (assertEqualDouble, assertEqualFloat) where

import Test.Framework

assertEqualDouble x y = 
    --assertBool $ abs (x - y) < 1e-14        
    assertEqual (fromIntegral (round (x * 1e11) :: Integer) * 1e-11 :: Double) (fromIntegral (round (y * 1e11) :: Integer) * 1e-11 :: Double)        

assertEqualFloat x y = 
    assertEqual (fromIntegral (round (x * 1e6) :: Integer) * 1e-6 :: Double) (fromIntegral (round (y * 1e6) :: Integer) * 1e-6 :: Double)        
