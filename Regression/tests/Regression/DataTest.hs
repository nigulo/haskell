module Regression.DataTest (tests) where

import Regression.Data as D
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector.Unboxed as V

tests :: TestTree
tests = testGroup "Data"
    [ testCase "xs" test_xs
    , testCase "getTangent" test_getTangent
    , testCase "getExtrema" test_getExtrema
    , testCase "getZeroCrossings" test_getZeroCrossings
    , testCase "isEvenlySampled" test_isEvenlySampled
    ]

test_xs :: Assertion
test_xs = do
    let
        dat0 = data0' $ V.fromList [0, 1, 2, 3, 4]
        dat1 = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)]
        dat2 = data2' $ V.fromList [(0, 0, 0.5), (0, 1, 2), (0, 2, -3),
            (1, 0, 0.5), (1, 1, 2), (1, 2, -3),
            (2, 0, 0.5), (2, 1, 2), (2, 2, -3)]
        spec1 = spectrum1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)]
    D.xs dat0 @?= [[], [], [], [], []]
    D.xs dat1 @?= [[0], [1], [2], [3], [4]]
    D.xs dat2 @?=
        [[0, 0], [0, 1], [0, 2],
        [1, 0], [1, 1], [1, 2],
        [2, 0], [2, 1], [2, 2]]
    D.xs spec1 @?= [[0], [1], [2], [3], [4]]


test_getTangent :: Assertion
test_getTangent = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, 0.75), (3, 0)]
        result = D.xys1 $ D.getTangent dat
        expectedResult = V.fromList [(1, (0.75 - 0.5) / 2), (2, (0 - 2) / 2)]
    result @?= expectedResult

test_getExtrema :: Assertion
test_getExtrema = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -1), (3, -3), (4, -3), (5, 0), (6, 1), (7, 1), (8, 2)]
        result = D.getExtrema dat True
        expectedResult = (V.fromList [(4, -3)], V.fromList [(1, 2)])
    result @?= expectedResult

test_getZeroCrossings :: Assertion
test_getZeroCrossings = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, -3), (3, 0), (4, 1)]
        result = D.getZeroCrossings dat
        expectedResult = V.fromList [1, 3]
    result @?= expectedResult

test_isEvenlySampled :: Assertion
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
    D.isEvenlySampled dat0 @?= False
    D.isEvenlySampled dat11 @?= True
    D.isEvenlySampled dat12 @?= False
    D.isEvenlySampled dat2 @?= False
    D.isEvenlySampled spec1 @?= True
