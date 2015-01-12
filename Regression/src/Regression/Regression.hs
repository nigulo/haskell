
module Regression.Regression (
    fitWithSpline_, 
    fitWithSpline, 
    envelope
    ) where

import Debug.Trace
import Regression.Data as D
import Utils.Misc
import Utils.List
import Regression.AnalyticData as AD
import Regression.Spline as S
import Regression.Polynom as P
import Regression.IOLeastSquares as LSQ
--import Regression.CUDALeastSquares as LSQ
import Regression.Utils
import Math.Expression
import Math.Function as F
import Math.Function as F
import Math.IOMatrix
import Math.IOVector
import Math.IOLinearEquations
import qualified Math.Vector as Vector
import qualified Math.Matrix as Matrix
import qualified Math.LinearEquations as LinearEquations 

import Control.Concurrent
import System.CPUTime
import qualified Data.Vector.Unboxed as V



fitWithSpline :: Polynom   -- ^ Set of unit polynoms
               -> Int       -- ^ Number of knots
               -> Data      -- ^ Data to be fitted
               -> Bool
               -> Int -- ^ Smoothness up to i'th derivative (0, 1 or 2)
               -> (Double -> IO ()) -- ^ progressUpdate func
               -> IO Spline    -- ^ Result
fitWithSpline unitPolynoms numNodes dat strict smoothUpTo puFunc =
  if strict 
   then return $ interpolateWithSpline numNodes dat
   else
    do
    let 
        x1 = D.xMin1 dat
        x2 = D.xMax1 dat
        ranks = getRanks unitPolynoms
        numTerms = sum ranks + length ranks
        count = getCount unitPolynoms
        termSums = scanl1 (\x y -> x + y + 1) (0:ranks)
        --unitPolynom = modulatedUnitPolynom template
        -- | Range for which each polynom will be defined
        step = (x2 - x1) / fromIntegral numNodes
        -- | Width of local fitting area
        --doubleStep = step * 2;
        --halfStep = 0;--step / 2; 
        steps = 
            let
                forSteps i numSteps dat steps =
                    let
                        stepLength = step * fromIntegral numSteps
                        xLeft = x1 + (sum0 steps) -- fromIntegral i * step * fromIntegral numSteps
                        (datLeft, datRight) = split1 (xLeft + stepLength) dat
                        vals = D.values1 $ datLeft
                    in
                    if (i >= numNodes) 
                        then steps
                        else
                        let 
                                neededPoints = 4 * count -- if (i == 0 || i + numSteps == numNodes) then 4 * count else 8 * count
                            in
                            if (V.length vals < neededPoints) 
                                    then
                                    if (i + numSteps == numNodes) 
                                            then (if steps == [] then [] else init steps) ++ [(if steps == [] then 0 else last steps) + stepLength]
                                    else forSteps i (numSteps + 1) dat steps
                                else forSteps (i + numSteps) 1 datRight (steps ++ [stepLength])
            in
                forSteps 0 1 dat []
        numPolynoms = length steps                
        forData i (lsqState, dat) = do
            let
                xLeft = x1 + sum (take i steps)
                (datLeft, datRight) = split1 (xLeft + (steps !! i)) dat
                vals = D.values1 datLeft
                --vals = D.values1 $ D.subSet1 (xLeft, xLeft + (steps !! i)) dat
                leadingZeroCoefs = replicate (numTerms * i) 0
                trailingZeroCoefs = replicate (numTerms * (numPolynoms - i - 1)) 0
                forj j lsqState = do
                    let 
                        (x, y, w) = vals V.! j
                        xVect =
                            leadingZeroCoefs ++ 
                            concat (P.getValues x unitPolynoms) ++
                            trailingZeroCoefs
                    --threadDelay 100
                    --puFunc $ (fromIntegral i + (fromIntegral j / (fromIntegral (length xs - 1)))) / (fromIntegral numPolynoms - 1) / 2
                    --putStrLn $ show $ xVect ++ [y] ++ [w]
                    LSQ.addMeasurement xVect y w lsqState
            state <- forM__ 0 (V.length vals - 1) lsqState $ {-# SCC "forj" #-} forj
            --state <- foldl (\x f -> x >>= f) (return lsqState) [forj j vals | j <- [0 .. length vals - 1]]
            --print state
            puFunc $ fromIntegral i / (fromIntegral numPolynoms - 1) / 2
            return (state, datRight)
        
        forConstraints i lsqState = do
            let
                numInitialTerms = replicate (numTerms * i) 0
                numFinalTerms = replicate (numTerms * (numPolynoms - i - 2)) 0
                forConstraint j vals state = do
                    let 
                        termsBefore = replicate (sum [length (vals !! k) | k <- [0 .. j - 1]]) 0
                        termsAfter = replicate (sum [length (vals !! k) | k <- [j + 1 .. length vals - 1]]) 0
                    st <- 
                        LSQ.addConstraint (
                                numInitialTerms ++ 
                                termsBefore ++ 
                                vals !! j ++
                                termsAfter ++ 
                                termsBefore ++ 
                                (map (\val -> -val) (vals !! j)) ++
                                termsAfter ++ 
                                numFinalTerms
                            ) 0 0 state
                    return st
                xRight = x1 + sum (take (i + 1) steps)
                continuity state = do
                    let vals = P.getValues xRight unitPolynoms 
                    forM_ 0 (length vals - 1) vals state $ {-# SCC "forConstraint" #-} forConstraint
                smoothness state = 
                    if (smoothUpTo >= 1) then
                        do
                            let tangents = P.getTangents xRight unitPolynoms

                            forM_ 0 (length tangents - 1) tangents state forConstraint
                    else return state
                smoothness2 state = 
                    if (smoothUpTo >=2) then
                        do
                            let derivatives = P.getDerivatives 2 xRight unitPolynoms

                            forM_ 0 (length derivatives - 1) derivatives state forConstraint
                    else return state
           
            s1 <- continuity lsqState
            --puFunc $ 0.5 + 0.3 * (fromIntegral i / (fromIntegral numPolynoms - 2) / 2)
            s2 <- smoothness s1
            --puFunc $ 0.5 + 0.6 * (fromIntegral i / (fromIntegral numPolynoms - 2) / 2)
            s3 <- smoothness2 s2
            puFunc $ 0.5 + fromIntegral i / (fromIntegral numPolynoms - 2) / 2
            return s3
         
    --puFunc $ 0
    time <- getCPUTime
    time <- getCPUTime >>= \t -> return $ t - time
    --putStrLn "A:" >> print (time `div` 1000000000)
    state <- LSQ.initialize (numTerms * numPolynoms)
    --putStrLn "A1:" >> print (time `div` 1000000000)
    (state1, _) <- forM__ 0 (numPolynoms - 1) (state, dat) ${-# SCC "forData" #-} forData
    --putStrLn "A2:" >> print (time `div` 1000000000)
    --puFunc $ 0.5
    time <- getCPUTime >>= \t -> return $ t - time
    --putStrLn ("B:"  ++ show state1) >> print (time `div` 1000000000)
    state2 <- LSQ.invert state1
    --putStrLn "B1:" >> print (time `div` 1000000000)
    --putStrLn ("BC:"  ++ show state1) >> print (time `div` 1000000000)
    state3 <- forM__ 0 (numPolynoms - 2) state1 $ {-# SCC "forConstraints" #-} forConstraints
    --putStrLn "C1:" >> print (time `div` 1000000000)
    time <- getCPUTime >>= \t -> return $ t - time
    --putStrLn ("C:" ++ show state2) >> print (time `div` 1000000000)
    coefs <- solve state3 >>= \v -> Math.IOVector.values v
    --coefs <- return $ LSQ.solve state3
    time <- getCPUTime >>= \t -> return $ t - time
    --putStrLn ("CD:" ++ show coefs) >> print (time `div` 1000000000)
        
    puFunc $ 0.5
    let 
        spline = AnalyticData [
            ([x1 + (sum (take j steps))], 
            [x1 + (sum (take (j + 1) steps))], 
            setCoefs [[(i, coefs !! (j * numTerms + (termSums !! k) + i)) | i <- [0 .. ranks !! k]] | k <- [0 .. count - 1]] unitPolynoms)
            | j <- [0 .. numPolynoms - 1]]
    time <- getCPUTime >>= \t -> return $ t - time
    putStrLn "D:" >> print (time `div` 1000000000)
    puFunc $ 0
    return $ spline

fitWithSpline_ :: Int       -- ^ Polynom rank
               -> Int       -- ^ Number of knots
               -> Data      -- ^ Data to be fitted
               -> Bool
               -> Int -- ^ Smoothness up to i'th derivative (0, 1 or 2)
               -> (Double -> IO ()) -- ^ progressUpdate func
               -> IO Spline    -- ^ Result
fitWithSpline_ rank numPolynoms dat strict smoothUpTo puFunc = fitWithSpline (unitPolynom rank) numPolynoms dat strict smoothUpTo puFunc

-- | Interpolates with cubic splines (argument 'numPols' is ignored)
interpolateWithSpline :: Int -> Data -> Spline
interpolateWithSpline numPols dat = 
    let 
        vals = D.values1 dat
        --xMin = D.xMin dat
        --xMax = D.xMax dat
        xs = D.xs1 dat
        numData = V.length vals
        pol = unitPolynom 3
        forData i =
            let
                numTermsBefore = replicate (i * 4) 0
                numTermsAfter = replicate ((numData - 2 - i) * 4) 0
                subVals = [vals V.! i, vals V.! (i + 1)]
                equs = map (\(x, y, _) -> numTermsBefore ++ (concat (P.getValues x pol)) ++ numTermsAfter ++ [y]) subVals
            in 
                equs
        forConstraint n i =
            let
                numTermsBefore = replicate (i * 4) 0
                numTermsAfter = replicate ((numData - 3 - i) * 4) 0
                x = (xs V.! (i + 1))
                derivs1 = concat $ P.getDerivatives n x pol
                derivs2 = map (\val -> -val) derivs1
                equ = numTermsBefore ++ derivs1 ++ derivs2 ++ numTermsAfter ++ [0]
            in 
                equ
        equations = concat [forData i | i <- [0 .. numData - 2]]
            ++ [forConstraint 1 i | i <- [0 .. numData - 3]]
            ++ [forConstraint 2 i | i <- [0 .. numData - 3]]
            ++ [(concat (P.getDerivatives 2 (V.head xs) pol)) ++ numTerms ++ [0]]
            ++ [(numTerms ++ (concat (P.getDerivatives 2 (V.last xs) pol)) ++ [0])] where
                numTerms = replicate ((numData - 2) * 4) 0
        m = Matrix.matrix equations
        solution = Vector.values $ LinearEquations.solveGauss (
            trace ("numData " ++ (show numData) ++ 
                   " numR " ++ (show (Matrix.getNumRows m)) ++ 
                   " numC " ++ (show (Matrix.getNumColumns m)) ++
                   " m " ++ (show m)) 
                m)
        forSolution i =
            let
                xLeft = xs V.! i
                xRight = xs V.! (i + 1)
                --xLeft = xMin + (fromIntegral i) * step
                --xRight = xMin + (fromIntegral i + 1) * step
                subSolution = [solution !! (i * 4 + j) | j <- [0 .. 3]]
            in
                trace ("subxs1=" ++ (show xLeft) ++ "subxs2=" ++ (show xRight) ++ "++" ++ (show subSolution)) $
                    ([xLeft], [xRight], polynom $ zip [0 .. 3] subSolution)
    in
        trace (show solution) AnalyticData [forSolution i | i <- [0 .. numData - 2]]


--------------------------------------------------------------------------------

-- | Returns upper or lower envelope estimation for given data set
envelope :: Bool      -- ^ Upper (True) or lower (False)
         -> Int       -- ^ Polynom rank
         -> Int       -- ^ Number of knots
         -> Double    -- ^ Max standard deviation allowed
         -> Bool      -- ^ Strict or statistical extrema detection
         -> Data      -- ^ Data to be fitted
         -> (Double -> IO ()) -- ^ progressUpdate func
         -> (Spline -> IO ()) -- ^ intermediate splines
         -> (Data -> IO ()) -- ^ intermediate data
         -> IO Spline    -- ^ Result
envelope upper rank knots sdev strictExtremaDetection dat puFunc splineFunc weightFunc =
    envelope1 upper rank knots sdev dat (-1) where
        envelope1 upper rank knots sdev dat prevSdev =
            do
                let
                    sigma = (D.xMax1 dat - D.xMin1 dat) / fromIntegral knots / 2
                    (xs, ys, ws) = V.unzip3 $ D.values1 dat;
                    fori i =
                        let
                            y = ys V.! i
                            x = xs V.! i
                            w = if strictExtremaDetection 
                                then
                                    let 
                                        ysi = [ys V.! j | j <- [max (i - 1) 0 .. min (i + 1) (V.length xs - 1)]]
                                        extremaFunc = 
                                            if upper then (<= y)
                                            else (>= y)
                                    in
                                        if all extremaFunc ysi && length ysi == 3
                                            then 1
                                            else 0
                                else let w1 =
                                          sum [((y - ys V.! j) * norm (x - (xs V.! j))) | j <- [0 .. i - 1]] +
                                          sum [((y - ys V.! j) * norm (x - (xs V.! j))) | j <- [i + 1 .. V.length xs  -1]] where
                                              --norm x = 1 / abs x * sigma
                                              norm x = exp (-(x * x / 2 / sigma / sigma)) / sigma / 2.5
                                      in 
                                        if upper then w1 else -w1
                        in 
                            --if upper then ws ++ [w] else ws ++ [-w]
                            --if upper then w else (-w)
                            w

                    --ws21 = forl_ [0 .. (length xs - 1)] [] fori
                    ws21 = V.fromList [fori i | i <- [0 .. (V.length xs - 1)]]
                    minW = V.minimum ws21
                    maxW = V.maximum ws21
                    wDiff = maxW - minW
                    ws2 = if prevSdev == -1 
                        then 
                            V.map (\w -> ((w - minW) / wDiff) ^ 1) ws21
                        else 
                            ws
                    ws3 = ws2 --zipWith (*) ws ws2
                    vals = V.zip3 xs ys ws3
                    vals2 = V.filter (\(_, _, w) -> w >= 0.25) vals
                    dat2 = data1 $ vals2;
                
                putStrLn "VALS: " >> (print vals)
                --weightFunc $ Data $ map (\(x, y, w) -> (x, w, w)) $ D.values dat2
                weightFunc dat2
                spline <- fitWithSpline_ rank knots dat2 strictExtremaDetection 2 puFunc
                splineFunc spline
                let 
                    newSdev = stdev dat (Right (Left spline))

                if strictExtremaDetection || newSdev <= sdev 
                    then 
                        return spline
                    else if prevSdev > 0 && (newSdev >= prevSdev) 
                        then 
                            return spline
                    else 
                        do
                            let
                                splineValues = AD.getValues_ (map (\x -> [x]) (V.toList xs)) spline
                                n = V.length xs;
                                
                                
                                (diffs, ys2) = unzip (zipWith zipFunc (V.toList ys) splineValues)
                                zipFunc y y1 =
                                    let diff = if upper then y - y1 else y1 - y
                                    in 
                                        if diff <= 0 then (diff, y1) else (diff, y)
                                
--                                minDiff = minimum diffs
--                                maxDiff = maximum diffs
                                --ws2 = replicate (length xs) 1 
                                --ws2 = zipWith (*) ws $ map (\diff -> ((diff - minDiff) / (maxDiff - minDiff)) ^ 1) diffs
                                newData = data1 $ V.zip3 xs (V.fromList ys2) ws3
                            envelope1 upper rank knots sdev newData newSdev


