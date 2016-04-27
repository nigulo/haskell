
module TSA.Period (
    calcDispersions) where

import Debug.Trace

import Regression.Functions as FS
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U

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
                mapFunc i puFunc = 
                    do
                        dispersionAndInfo <- 
                            case method of 
                                0 -> do --LeastSquares
                                    leastSquares dat (1 / (freqStart + fromIntegral i * step))
                                1 -> do --StringLength
                                    do
                                        let 
                                            ymin = D.yMin dat
                                            yNorm = D.yMax dat - ymin
                                            vals = D.xys1 dat --map (\(x, y) -> (x, (y - ymin) / yNorm)) $ D.xys1 dat
                                        stringLength vals (1 / (freqStart + fromIntegral i * step))
                        puFunc 1
                        return dispersionAndInfo
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
        dispersions = createDataParams_ name (map (\sd -> createSubDataParams__ sd) subDispersions)
            
    bestPeriods <- mapM (\(i, SubDataParams _ (SD1 periodSpec) _) -> do
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

leastSquares :: Data -> Double -> IO Double
leastSquares dat period =
    do 
        g <- getStdGen 
        spline <-  
            do
                let
                    templates =
                        [PolynomTemplate (3, Nothing, Nothing)] ++ 
                        [PolynomTemplate (1, Just (F.fromExpression(sine freq)), Just (F.fromExpression (dsin freq)))] ++
                        [PolynomTemplate (1, Just (F.fromExpression(cosine freq)), Just (F.fromExpression (dcos freq)))]                         
                            where
                                freq = 2 * pi / period
                fitWithSpline (modulatedUnitPolynoms templates) 1 dat 2 (\_ -> return ())
        let
            SD1 residue = subDataBinaryOp (F.subtr) (SD1 dat) (SD2 spline) True g
            vals = values1 residue
            (disp, norm) = V.foldl' (\(sum, wSum) (_, y, w) -> (sum + y * y * w, wSum + w)) (0, 0) vals
        return $ disp / norm

stringLength :: V.Vector (Double, Double) -> Double -> IO Double
stringLength xys period =
    do 
        let
            phaseData = sortVectorBy (\(x1, _) (x2, _) -> compare x1 x2) $ V.map (\(x, y) -> (snd (properFraction (x / period)), y)) xys
            
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
        
