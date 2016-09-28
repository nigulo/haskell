{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.DataTest where

import Regression.Data as D
import Test.Framework
import qualified Data.Vector.Unboxed as V

test_xs = do
    let
        dat0 = data0' $ V.fromList [0, 1, 2, 3, 4] 
        dat1 = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)] 
        dat2 = data2' $ V.fromList [(0, 0, 0.5), (0, 1, 2), (0, 2, -3),
            (1, 0, 0.5), (1, 1, 2), (1, 2, -3),
            (2, 0, 0.5), (2, 1, 2), (2, 2, -3)] 
        spec1 = spectrum1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)]
    assertEqual [[], [], [], [], []] (D.xs dat0)
    assertEqual [[0], [1], [2], [3], [4]] (D.xs dat1)
    assertEqual 
        [[0, 0], [0, 1], [0, 2],
        [1, 0], [1, 1], [1, 2],
        [2, 0], [2, 1], [2, 2]] (D.xs dat2)
    assertEqual [[0], [1], [2], [3], [4]] (D.xs spec1)
    

test_getTangent = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, 0.75), (3, 0)] 
        result = D.xys1 $ D.getTangent dat
        expectedResult = V.fromList [(1, (0.75 - 0.5) / 2), (2, (0 - 2) / 2)]
    assertEqual expectedResult result

test_getExtrema = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -1), (3, -3), (4, -3), (5, 0), (6, 1), (7, 1), (8, 2)] 
        result = D.getExtrema dat True
        expectedResult = (V.fromList [(4, -3)], V.fromList [(1, 2)])
    assertEqual expectedResult result

test_getZeroCrossings = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)] 
        result = D.getZeroCrossings dat
        expectedResult = V.fromList [1, 3]
    assertEqual expectedResult result

test_isEvenlySampled = do
    let
        dat0 = data0' $ V.fromList [0, 1, 2, 3, 4] 
        dat11 = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)] 
        dat12 = data1' $ V.fromList [(0, 0.5), (1, 2), (2.5, -3), (3, 0), (4, 1)] 
        dat2 = data2' $ V.fromList [(0, 0, 0.5), (0, 1, 2), (0, 2, -3), (0, 3, 0), (0, 4, 1),
            (1, 0, 0.5), (1, 1, 2), (1, 2, -3), (1, 3, 0), (1, 4, 1),
            (2, 0, 0.5), (2, 1, 2), (2, 2, -3), (2, 3, 0), (2, 4, 1),
            (3, 0, 0.5), (3, 1, 2), (3, 2, -3), (3, 3, 0), (3, 4, 1),
            (4, 0, 0.5), (4, 1, 2), (4, 2, -3), (4, 3, 0), (4, 4, 1)] 
        spec1 = spectrum1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)]
    assertEqual False (D.isEvenlySampled dat0)
    assertEqual True (D.isEvenlySampled dat11)
    assertEqual False (D.isEvenlySampled dat12)
    assertEqual False (D.isEvenlySampled dat2)
    assertEqual True (D.isEvenlySampled spec1)
    