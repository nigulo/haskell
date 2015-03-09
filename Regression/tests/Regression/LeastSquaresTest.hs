{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.LeastSquaresTest where

import Regression.LeastSquares as LSQ
import Regression.CUDALeastSquares as CLSQ
import Math.Vector as V
import Math.Matrix as M
import Data.Array.Accelerate as A
import Test.Framework
import Data.List


--------------------------------------------------------------------------------
-- Longley's test
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
bExpected = [-3482258.6345964866, 
            15.061872271428058, 
            -3.581917929262539e-2,
            -2.020229803817295, 
            -1.0332268671735234, 
            -5.1104105653362264e-2,
            1829.1514646138853]
bExpected1 = [-3482258.634595804, 
            15.061872271372636, 
            -3.581917929259047e-2,
            -2.020229803816817, 
            -1.0332268671735911, 
            -5.110410565358403e-2,
            1829.1514646135445]
bExpected2 = [-3482258.6345964866,
            15.061872271428058,
            -3.581917929262539e-2,
            -2.020229803817295,
            -1.0332268671735234,
            -5.1104105653362264e-2,
            1829.1514646138853]

--------------------------------------------------------------------------------

m = [[16.0,1626.9,6203175.0,51093.0,41707.0,1878784.0,31272.0],
            [1626.9,167172.09,6.467006497e8,5289080.100000001,4293173.699999999,1.921396506e8,3180539.9000000004],
            [6203175.0,6.467006497e8,2.553151559929e12,2.0650541815e10,1.6632945158e10,7.38680235369e11,1.2131170206e10],
            [51093.0,5289080.100000001,2.0650541815e10,1.76254267e8,1.31452803e8,6.066485555e9,9.9905864e7],
            [41707.0,4293173.699999999,1.6632945158e10,1.31452803e8,1.15981677e8,4.92386424e9,8.1537068e7],
            [1878784.0,1.921396506e8,7.38680235369e11,6.066485555e9,4.92386424e9,2.2134014265e11,3.672577089e9],
            [31272.0,3180539.9000000004,1.2131170206e10,9.9905864e7,8.1537068e7,3.672577089e9,6.1121464e7]]

v = [1045072.0,
            1.0681617720000002e8,
            4.1032273457e11,
            3.361978021e9,
            2.740941335e9,
            1.23068464014e11,
            2.042836838e9]

m1 :: M.Matrix Double
m1 = addColumn v (M.matrix m)


gentleman :: LSQ.LSQState
gentleman =
    let 
        xMatrix = M.matrix x
        yVect = V.vector y
    in 
        foldl' (\state i -> LSQ.addMeasurement (V.vector (getRow i xMatrix)) (V.get i yVect) 1 state) (LSQ.initialize (getNumColumns xMatrix)) [0 .. length y - 1]

cudaGentleman :: CLSQ.LSQState
cudaGentleman =
    let 
        numColumns = length (head x)
    in 
        --for__ 0 (length y - 1) (CLSQ.initialize numColumns) (\i state -> CLSQ.addMeasurements (x Prelude.!! i) (y Prelude.!! i) 1 state)
        --for__ 0 (length y - 1) (CLSQ.initialize numColumns) (\i state -> CLSQ.addMeasurements [(x Prelude.!! i)] [(y Prelude.!! i)] 1 state)
        CLSQ.addMeasurements x y (repeat 1) $ CLSQ.initialize numColumns

--stirling :: LSQ.LSQState -> LSQ.LSQState
--stirling state = for_ 0 (getLength s - 1) state (\i state -> LSQ.addConstraint (V.vector (getRow i r)) (V.get i s) 0 state)




        --putStr (show (solveGauss m1) ++ "\n")
        --putStr (show (solveCramer m v) ++ "\n")
        --putStr (show (LSQ.solve x y) ++ "\n")
        --putStr (show (solve1 x y) ++ "\n")
        --putStr (show ((transpose x) `mul` x) ++ "\n")

test_LSQ = do
    --putStrLn (show (calcForward 1 (calcForward 0 m)))
    --putStrLn (show (calcBackward m))
    
    --print (LSQ.solve (stirling (invert gentleman)))
    let
        result = (LSQ.solve (LSQ.invert gentleman))
    assertEqual bExpected (values result)


test_CLSQ = do
    --putStrLn (show (calcForward 1 (calcForward 0 m)))
    --putStrLn (show (calcBackward m))
    
    --print (LSQ.solve (stirling (invert gentleman)))
    let
        cudaResult = (CLSQ.solve (CLSQ.invert cudaGentleman))
    assertEqual bExpected2 cudaResult
