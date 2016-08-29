
module TSA.Period (
    calcDispersions) where

import Debug.Trace

import Regression.Functions as FS
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U

import TSA.CommonParams
import TSA.Params
import TSA.Data

import Data.List
import Data.Maybe
import Utils.Misc
import Utils.Concurrent

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.CPUTime
import System.IO
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

calcDispersions :: DataParams -> Double -> Double -> Int -> Int -> String -> Bool 
    -> TaskEnv 
    -> IO DataParams
calcDispersions dataParams periodStart' periodEnd' precision method name bootstrap taskEnv = do
    let 
        periodStart = min periodStart' periodEnd'
        periodEnd = max periodStart' periodEnd'
        freqStart = 1 / periodEnd
        freqEnd = 1 / periodStart
        step = (freqEnd - freqStart) / (fromIntegral precision)
        puFunc = progressUpdateFunc taskEnv

        findDispersions :: Int -> Either D.Data (Either S.Spline FS.Functions) -> (Double -> IO ()) -> IO SubData 
        findDispersions method (Left dat) puFunc = do
            let
                ys = D.ys dat
                (mean, var) = meanVarianceUnb ys
                datCentered = D.setY (V.map (\y -> y - mean) ys) dat 
                sigLevel pValue = -2 * var * log (pValue)
                mapFunc i puFunc = do 
                    let
                        freq = (freqStart + fromIntegral i * step)
                    dispersionAndInfo <- 
                        case method of 
                            0 -> do --LeastSquares
                                let
                                    r = V.sum $ V.map (^2) (D.ys datCentered)
                                    reduction = leastSquares datCentered freq
                                    dispersion = reduction / r
                                    pValue = exp(-reduction/2/var)
                                (logFunc taskEnv) ("p-value for " ++ show freq ++ " " ++ show pValue)
                                return dispersion
                            1 -> do --StringLength
                                do
                                    let 
                                        ymin = D.yMin dat
                                        yNorm = D.yMax dat - ymin
                                        vals = D.xys1 dat --map (\(x, y) -> (x, (y - ymin) / yNorm)) $ D.xys1 dat
                                    stringLength vals freq
                    puFunc 1
                    return dispersionAndInfo
            (logFunc taskEnv) ("Significance level 0.1: " ++ show (sigLevel 0.1))
            (logFunc taskEnv) ("Significance level 0.05: " ++ show (sigLevel 0.05))
            (logFunc taskEnv) ("Significance level 0.01: " ++ show (sigLevel 0.01))
            dispersions <- calcConcurrently mapFunc puFunc (taskInitializer taskEnv) (taskFinalizer taskEnv) [0 .. precision - 1]
            let
                norm = maximum dispersions
                normalizedDispersions =  
                    if False -- norm > 0 
                        then map (\d -> d / norm) dispersions 
                        else dispersions
                freqSpec = D.Spectrum2 ((freqStart, step), V.zip (V.fromList normalizedDispersions) (V.replicate (length normalizedDispersions) 1))
            return $ SD1 freqSpec

    let
        numSubData = length $ dataSet dataParams
    
    subDispersions <- zipWithM (\sdp i -> do
            let
                SD1 dat = subData sdp
            dat1 <- if bootstrap then reshuffleData dat else return dat
            disps <- findDispersions method (Left dat1) (\percent -> puFunc (percent * (fromIntegral i / fromIntegral numSubData)))
            puFunc $ fromIntegral i / fromIntegral numSubData
            return disps
        ) (dataSet dataParams) [1 ..]
    let
        dispersions = createDataParams_ name (map (\sd -> createSubDataParams_ sd) subDispersions)
            
    bestPeriods <- mapM (\(i, SubDataParams _ (SD1 periodSpec)) -> do
            let
                freqDisps = sortBy (\(_, disp1) (_, disp2) -> compare disp1 disp2) $ V.toList $ D.getMinima periodSpec False
                freqs = map fst freqDisps
            (logFunc taskEnv) ("Possible freqs for " ++ name ++ "[" ++ show i ++ "] = " ++ (show freqs))
            case freqDisps of 
                 [] -> return Nothing
                 ((freq, disp):_) -> return $ Just (1 / freq, (1 - disp) ^ 2)  
        ) (zip [1, 2 ..] (dataSet dispersions))
    let
        periodMean = meanWeighted $ V.fromList $ catMaybes bestPeriods
        periodVar = varianceWeighted $ V.fromList $ catMaybes bestPeriods
        periodDisp = mean $ V.map (\(p, d) -> 1 - sqrt d) $ V.fromList $ catMaybes bestPeriods
        
    (logFunc taskEnv) ("Period mean stdev disp: " ++ show periodMean ++ " " ++ show periodVar ++ " " ++ show periodDisp)

    puFunc 1
    return dispersions

leastSquares :: Data -> Double -> Double
leastSquares dat freq =
    let
        xys = D.xys1 dat
        --(ss, cs) = V.foldl1' (\(ss, cs) (s, c) -> (ss + s, cs + c)) $ V.map (\(x, _) ->
        --        let
        --            phase = 4 * pi * freq * x
        --        in
        --            sin phase, cos phase 
        --    ) xys
        --tau = atan ss / cs / (4 * pi * freq) 
        (cc, ss, cs, yc, ys) = V.foldl1' (\(ccs, sss, css, ycs, yss) (cc, ss, cs, yc, ys) -> (ccs + cc, sss + ss, css + cs, ycs + yc, yss + ys)) $ V.map (\(x, y) -> 
                let
                    phase = 2 * pi * freq * x 
                    c = cos phase
                    s = sin phase
                in
                    (c * c, s * s, c * s, y * c, y * s)
            ) xys
        d = cc * ss - cs * cs
        reduction = ((ss * yc - cs * ys) * yc + (cc * ys - cs * yc) * ys) / d
    in reduction

stringLength :: V.Vector (Double, Double) -> Double -> IO Double
stringLength xys freq =
    do 
        let
            phaseData = sortVectorBy (\(x1, _) (x2, _) -> compare x1 x2) $ V.map (\(x, y) -> (snd (properFraction (x * freq)), y)) xys
            
            calculateLength [] = return 0
            calculateLength (_:[]) = return 0
            calculateLength ((x1, y1):(x2, y2):xys) =
                do
                    let 
                        dx = x2 - x1
                        dy = y2 - y1
                    tailLength <- calculateLength ((x2, y2):xys)
                    return $ sqrt (dx * dx + dy * dy) + tailLength
        calculateLength (V.toList phaseData)
        
