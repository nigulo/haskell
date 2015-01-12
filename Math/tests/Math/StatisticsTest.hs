{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Math.StatisticsTest where

import Math.Statistics
import Utils.Test
import qualified Data.Map as M
import Test.Framework
import qualified Data.Vector.Unboxed as V

test_normalCDF = do
    let 
        result1 = normalCDF 0 1 0
        result2 = normalCDF 2 1.5 2.4
        result3 = normalCDF (-1) 2 (-0.3)
    assertEqual 0.5 result1
    assertEqual 0.6051370895359748 result2
    assertEqualDouble 0.636830651175619 result3

test_normalKSTest = do
    let
        dat = V.fromList [0, 0.2, 0.4, 0.2, 0.7, -0.2, -0.1, 0.1, 0.5, 0, 0.1, -0.6, 0, 0, 0.1, -0.1, 0.1, 0.2, -0.1, 0.3, -0.2, 0.3, 0.4, -0.5, 0.6, 0.5, -0.3]
        result = normalKSTest 0 1 dat
    assertEqualDouble 0.3096291794497859 result
