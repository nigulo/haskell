module Regression.LeastSquaresTest (tests) where

import Regression.LeastSquares as LSQ
import Math.Vector as V
import Math.Matrix as M
import Test.Tasty
import Test.Tasty.HUnit
import Data.List

tests :: TestTree
tests = testGroup "LeastSquares"
    [ testCase "LSQ" test_LSQ
    ]

--------------------------------------------------------------------------------
-- Longley's test
x :: [[Double]]
x = [
    [1, 83.0, 234289, 2356, 1590, 107608, 1947],
    [1, 88.5, 259426, 2325, 1456, 108632, 1948],
    [1, 88.2, 258054, 3682, 1616, 109773, 1949],
    [1, 89.5, 284599, 3351, 1650, 110929, 1950],
    [1, 96.2, 328975, 2099, 3099, 112075, 1951],
    [1, 98.1, 346999, 1932, 3594, 113270, 1952],
    [1, 99.0, 365385, 1870, 3547, 115094, 1953],
    [1, 100.0, 363112, 3578, 3350, 116219, 1954],
    [1, 101.2, 397469, 2904, 3048, 117388, 1955],
    [1, 104.6, 419180, 2822, 2857, 118734, 1956],
    [1, 108.4, 442769, 2936, 2798, 120445, 1957],
    [1, 110.8, 444546, 4681, 2637, 121950, 1958],
    [1, 112.6, 482704, 3813, 2552, 123366, 1959],
    [1, 114.2, 502601, 3931, 2514, 125368, 1960],
    [1, 115.7, 518173, 4806, 2572, 127852, 1961],
    [1, 116.9, 554894, 4007, 2827, 130081, 1962]]

y :: [Double]
y = [
    60323,
    61122,
    60171,
    61187,
    63221,
    63639,
    64989,
    63761,
    66019,
    67857,
    68169,
    66513,
    68655,
    69564,
    69331,
    70551]

-- the result
bExpected :: [Double]
bExpected = [-3482258.6345964866,
            15.061872271428058,
            -3.581917929262539e-2,
            -2.020229803817295,
            -1.0332268671735234,
            -5.1104105653362264e-2,
            1829.1514646138853]

--------------------------------------------------------------------------------

gentleman :: LSQ.LSQState
gentleman =
    let
        xMatrix = M.matrix x
        yVect = V.vector y
    in
        foldl' (\state i -> LSQ.addMeasurement (V.vector (getRow i xMatrix)) (V.get i yVect) 1 state) (LSQ.initialize (getNumColumns xMatrix)) [0 .. length y - 1]

test_LSQ :: Assertion
test_LSQ = do
    let
        result = LSQ.solve (LSQ.invert gentleman)
    values result @?= bExpected
