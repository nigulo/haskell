{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.DataTest where

import Regression.Data as D
import Test.Framework
import qualified Data.Vector.Unboxed as V

test_getTangent = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, 0.75), (3, 0)] 
        result = D.xys1 $ D.getTangent dat
        expectedResult = V.fromList [(1, (0.75 - 0.5) / 2), (2, (0 - 2) / 2)]
    assertEqual expectedResult result

test_getExtrema = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -1), (3, -3), (4, 0), (5, 1)] 
        result = D.getExtrema dat
        expectedResult = (V.fromList [(3, -3)], V.fromList [(1, 2)])
    assertEqual expectedResult result

test_getZeroCrossings = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)] 
        result = D.getZeroCrossings dat
        expectedResult = V.fromList [1, 3]
    assertEqual expectedResult result
