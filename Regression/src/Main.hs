
module Main where

import Regression.IOLeastSquares as LSQ
import Regression.CUDALeastSquares as CLSQ
import Math.LinearEquations
import Math.Matrix as M
import Math.IODoubleMatrix as IOM
import Math.IODoubleVector as IOV
import Math.Vector as V
import Math.Function as F
import Regression.Polynom
import Regression.Functions
import Regression.AnalyticData as AD
--import Utils.Misc
import System.Random
import System.CPUTime

import Control.Monad
import qualified Data.Vector.Unboxed as V

--------------------------------------------------------------------------------
-- Longley's test
x = [
    [1, 83.0, 234289, 2356, 1590, 107608, 1947],
    [1, 88.5, 259426, 2325, 1456, 108632, 1948] {-,
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
    [1, 116.9, 554894, 4007, 2827, 130081, 1962]-}
    ]

y = [
    60323,
    61122 {-,
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
    70551-}
    ]

-- the result
b = [-3482258.63459581833,
            15.06187227137329497,
            -0.0358191792925910166,
            -2.02022980381682509,
            -1.03322686717359198,
            -0.0511041056535807145,
            1829.15146461355185]
--------------------------------------------------------------------------------

{-
m :: Matrix Double
m = matrix [[16.0,1626.9,6203175.0,51093.0,41707.0,1878784.0,31272.0],
            [1626.9,167172.09,6.467006497e8,5289080.100000001,4293173.699999999,1.921396506e8,3180539.9000000004],
            [6203175.0,6.467006497e8,2.553151559929e12,2.0650541815e10,1.6632945158e10,7.38680235369e11,1.2131170206e10],
            [51093.0,5289080.100000001,2.0650541815e10,1.76254267e8,1.31452803e8,6.066485555e9,9.9905864e7],
            [41707.0,4293173.699999999,1.6632945158e10,1.31452803e8,1.15981677e8,4.92386424e9,8.1537068e7],
            [1878784.0,1.921396506e8,7.38680235369e11,6.066485555e9,4.92386424e9,2.2134014265e11,3.672577089e9],
            [31272.0,3180539.9000000004,1.2131170206e10,9.9905864e7,8.1537068e7,3.672577089e9,6.1121464e7]]

v :: Vector Double
v = vector [1045072.0,
            1.0681617720000002e8,
            4.1032273457e11,
            3.361978021e9,
            2.740941335e9,
            1.23068464014e11,
            2.042836838e9]

m1 :: Matrix Double
m1 = addColumn (values v) m

--gentleman :: LSQState
--gentleman = for_ 0 (getLength y - 1) (initialize (getNumColumns x)) (\i state -> addMeasurement (Vector (getRow i x)) (V.get i y) 1 state)


--stirling :: LSQState -> LSQState
--stirling state = for_ 0 (getLength s - 1) state (\i state -> addConstraint (Vector (getRow i r)) (V.get i s) 0 state)

sinus = sin
dsinus :: Int -> Double -> Double
dsinus i x = case i `mod` 4 of
    0 -> sin x
    1 -> cos x
    2 -> -(sin x)
    3 -> -(cos x)

cosine = cos
dcosine :: Int -> Double -> Double
dcosine i x = case i `mod` 4 of
    0 -> cos x
    1 -> -(sin x)
    2 -> -(cos x)
    3 -> sin x

x = 5
fx = (7 * x ^ 4 + x ^ 2) * sin x
dfx = (7 * x ^4 + x ^ 2) * cos x + (28 * x ^ 3 + 2 * x) * sin x
d2fx = - (7 * x ^ 4 + x ^ 2) * sin x + 2 * (28 * x ^ 3 + 2 * x) * cos x + (84 * x ^ 2 + 2) * sin x
d3fx = - (7 * x ^ 4 + x ^ 2) * cos x - 3 * (28 * x ^ 3 + 2 * x) * sin x + 3 * (84 * x ^ 2 + 2) * cos x + 168 * x * sin x

fx1 = (1 + 1.5 * x + x ^ 2 + 0.5 * x ^ 3) * sin x + (-1.5 + x - 0.5 * x ^ 2 + x ^ 3) * cos x;
dfx1 = (1.5 + 2 * x + 3 * 0.5 * x ^ 2) * sin x + (1 + 1.5 * x + x ^ 2 + 0.5 * x ^ 3) * cos x +
        (1 - x + 3 * x ^ 2) * cos x - (-1.5 + x - 0.5 * x ^ 2 + x ^ 3) * sin x;

pol = modulatedPolynom [(2, 1), (4, 7)] (Just sinus) (Just dsinus)
pol1 = modulatedUnitPolynoms [(PolynomTemplate (3, Just sinus, Just dsinus)), (PolynomTemplate (3, Just cosine, Just dcosine))]
pol2 = setCoefs [[(1, 1.5), (3, 0.5)], [(0, -1.5), (2, -0.5)]] pol1
-}

loadData :: IO ([[Double]], [Double], [Double])
loadData = 
    do
        datStr <- readFile ("data.txt")
        return $ unzip3 $ map (\datLine ->
            let
                xyw :: [Double] = read datLine
                xy = init xyw
                w = last xyw
                x = init xy
                y = last xy
            in
                (x, y, w)
            ) (lines datStr) 


gentleman :: [[Double]] -> [Double] -> [Double] -> (IO LSQ.IOLSQState)
gentleman x y w = do
    let 
        xMatrix = M.matrix x
    initialState <- LSQ.initialize (M.getNumColumns xMatrix)
    foldM (\state i -> do
            xVect <- IOV.vector (x !! i ++ [y !! i])
            LSQ.addMeasurement xVect (w !! i) state
        ) initialState [0 .. length y - 1]  

cudaGentleman :: [[Double]] -> [Double] -> [Double] -> CLSQ.LSQState
cudaGentleman x y w =
    let 
        numColumns = length (head x)
    in 
        --for__ 0 (length y - 1) (CLSQ.initialize numColumns) (\i state -> CLSQ.addMeasurements (x Prelude.!! i) (y Prelude.!! i) 1 state)
        CLSQ.addMeasurements x y w $ CLSQ.initialize numColumns

main :: IO ()
--main = putStr (show (calcForward 1 (calcForward 0 m)))
--main = putStr (show (calcBackward m))
main = 
    do
        --putStr (show (solveGauss m1) ++ "\n")
        --putStr (show (solveCramer m v) ++ "\n")
        --putStr (show (LSQ.solve x y) ++ "\n")
        --putStr (show (solve1 x y) ++ "\n")
        --putStr (show ((transpose x) `mul` x) ++ "\n")
        --print (LSQ.solve (stirling (invert gentleman)))
        --print (dsinus 4 1)
        ---------------------------------------
        --print (getValue 5 pol - fx)
        --print (getDerivative 1 5 pol - dfx)
        --print (getDerivative 2 5 pol - d2fx)
        --print (getDerivative 3 5 pol)
        --print d3fx
        ----------------------------------------
        --print (getValue 5 pol2 - fx1)
        --print (getTangent 5 pol2 - dfx1)
        
          --let pol@(Polynom _) = read "Polynom [([(0,-0.9320642862898423),(1,4.177629473517001e-3),(2,-6.6775717651294175e-6),(3,3.891358887611803e-9)],Just sin((0.9377888518178487)*(x)),Just 1),([(0,-0.6284696589995866),(1,3.6611938820369262e-3),(2,-5.965147311821722e-6),(3,3.157074417250837e-9)],Just cos((0.9377888518178487)*(x)),Just 1)]"
          --let pol@(Polynom _) = read "Polynom [([(0,-0.9320642862898423),(1,4.177629473517001e-3),(2,-6.6775717651294175e-6),(3,3.891358887611803e-9)],Nothing,Nothing),([(0,-0.6284696589995866),(1,3.6611938820369262e-3),(2,-5.965147311821722e-6),(3,3.157074417250837e-9)],Nothing,Nothing)]"
          --let pol@(Polynom _) = read "Polynom [([(0,-0.9320642862898423),(1,4.177629473517001e-3),(2,-6.6775717651294175e-6),(3,3.891358887611803e-9)],Just sin((0.9377888518178487)*(x)),Just ((0.9377888518178487)^(i))*(sin(((0.9377888518178487)*(x))+((i)*(1.5707963267948966))))),([(0,-0.6284696589995866),(1,3.6611938820369262e-3),(2,-5.965147311821722e-6),(3,3.157074417250837e-9)],Just cos((0.9377888518178487)*(x)),Just ((0.9377888518178487)^((i)+(1.0)))*(sin(((0.9377888518178487)*(x))+(((i)+(1.0))*(1.5707963267948966)))))]"
          --print pol
          
        --print (getDerivative 1 3 (modulatedPolynom [(0, 17), (1, 11), (2, 12), (3, 13), (4, 14), (5, 15)] sinus dsinus))
        --print ((polynom [(0, 17), (1, 11), (3, 13), (4, 14), (5, 0)]) `plus` 
      --          (polynom [(0, 15), (2, 12), (3, 13), (4, 14), (5, 10)]))
        --print (for_ 0 0 (undefined) (\_ _ -> LSQ.solve func1))
{-        state <- return (initialize (getNumColumns x))
        print state
        state1 <- return (update (Vector (getRow 0 x)) (V.get 0 y) 1 state)
        print state1
        state2 <- return (update (Vector (getRow 1 x)) (V.get 1 y) 1 state1)
        print state2
 -}

        --let f = AnalyticData [(0, 1, F.function "rnd(0,1)")]
            --let f = F.add
        --print $ AD.getValues [[0]] (mkStdGen 1) f
        {-
        let
            --pol@(Polynom _) = read "Polynom [([(0,1.0),(1,1.0),(2,1.0),(3,1.0)],Just (Function {initialExpr = \"1.0\", expr = 1.0, defs = [], varDefs = [], funcDefs = [], unknownVarNames = [], unknownFuncNames = []}),Just (Function {initialExpr = \"0.0\", expr = 0.0, defs = [], varDefs = [], funcDefs = [], unknownVarNames = [], unknownFuncNames = []})),([(0,1.0),(1,1.0),(2,1.0),(3,1.0)],Just (Function {initialExpr = \"sin((6.283185307179586)*(x))\", expr = sin((6.283185307179586)*(x)), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"x\"], unknownFuncNames = []}),Just (Function {initialExpr = \"((6.283185307179586)^(i))*(sin(((6.283185307179586)*(x))+((i)*(1.5707963267948966))))\", expr = ((6.283185307179586)^(i))*(sin(((6.283185307179586)*(x))+((i)*(1.5707963267948966)))), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"i\",\"x\"], unknownFuncNames = []})),([(0,1.0),(1,1.0),(2,1.0),(3,1.0)],Just (Function {initialExpr = \"sin((12.566370614359172)*(x))\", expr = sin((12.566370614359172)*(x)), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"x\"], unknownFuncNames = []}),Just (Function {initialExpr = \"((12.566370614359172)^(i))*(sin(((12.566370614359172)*(x))+((i)*(1.5707963267948966))))\", expr = ((12.566370614359172)^(i))*(sin(((12.566370614359172)*(x))+((i)*(1.5707963267948966)))), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"i\",\"x\"], unknownFuncNames = []})),([(0,1.0),(1,1.0),(2,1.0),(3,1.0)],Just (Function {initialExpr = \"cos((6.283185307179586)*(x))\", expr = cos((6.283185307179586)*(x)), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"x\"], unknownFuncNames = []}),Just (Function {initialExpr = \"((6.283185307179586)^((i)+(1.0)))*(sin(((6.283185307179586)*(x))+(((i)+(1.0))*(1.5707963267948966))))\", expr = ((6.283185307179586)^((i)+(1.0)))*(sin(((6.283185307179586)*(x))+(((i)+(1.0))*(1.5707963267948966)))), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"i\",\"x\"], unknownFuncNames = []})),([(0,1.0),(1,1.0),(2,1.0),(3,1.0)],Just (Function {initialExpr = \"cos((12.566370614359172)*(x))\", expr = cos((12.566370614359172)*(x)), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"x\"], unknownFuncNames = []}),Just (Function {initialExpr = \"((12.566370614359172)^((i)+(1.0)))*(sin(((12.566370614359172)*(x))+(((i)+(1.0))*(1.5707963267948966))))\", expr = ((12.566370614359172)^((i)+(1.0)))*(sin(((12.566370614359172)*(x))+(((i)+(1.0))*(1.5707963267948966)))), defs = [], varDefs = [], funcDefs = [], unknownVarNames = [\"i\",\"x\"], unknownFuncNames = []}))]"
            pol@(Polynom _) = read "Polynom [([(0,1.0),(1,1.0),(2,1.0),(3,1.0)],Nothing,Nothing)]"
        putStrLn (show pol)
        
        time <- getCPUTime
        print $ sum $ map (\x -> x * x) [0 :: Double, 1 .. 10000000]
        time1 <- getCPUTime
        putStrLn $ show (time1 - time)
        time2 <- getCPUTime
        print $ V.sum $ V.map (\x -> x * x) (V.generate 10000000 (\i -> fromIntegral i :: Double))
        time3 <- getCPUTime
        putStrLn $ show (time3 - time2)
        time4 <- getCPUTime
        print $ sum $ map (\x -> x * x) [0 :: Double, 1 .. 10000000]
        time5 <- getCPUTime
        putStrLn $ show (time5 - time4)
        -}
        
        (x, y, w) <- loadData
        --w <- return $ repeat 1
        time1231 <- getCPUTime
        let
            calc =
                --return $ CLSQ.solve (CLSQ.invert (cudaGentleman x y w))
                cudaGentleman x y w
            calc2 =
                --return $ LSQ.solve (LSQ.invert (gentleman x y w))
                do
                    LSQ.IOLSQState (b, d, e) <- gentleman x y w
                    IOM.getValues b
        res <- calc2
        print $ res
        time123123 <- getCPUTime
        putStrLn $ show $ fromIntegral (time123123 - time1231) / 1e12

                
        g <- getStdGen 
        
        time6 <- getCPUTime
        let
            func :: F.Function Double = F.function "cos(x)"
            ad = AD.AnalyticData [([0], [100], func)]
        extrema <- return $ AD.getExtrema 10000000 (Just 6.28) g ad
        --print extrema
        time7 <- getCPUTime
        putStrLn $ show $ fromIntegral (time7 - time6) / 1e12
        
        return ()
