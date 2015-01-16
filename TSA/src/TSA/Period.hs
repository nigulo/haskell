
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
import System.CPUTime
import System.IO
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

calcDispersions :: DataParams -> Double -> Double -> Int -> Int -> String -> Bool -> (Double -> IO ()) -> (String -> IO ()) -> IO [(Double, DataParams)]
calcDispersions dataParams periodStart' periodEnd' precision method name bootstrap puFunc logFunc = do
    let 
        periodStart = min periodStart' periodEnd'
        periodEnd = max periodStart' periodEnd'
        freqStart = 1 / periodEnd
        freqEnd = 1 / periodStart
        step = (freqEnd - freqStart) / (fromIntegral precision)

        findDispersions :: Int -> Double -> (Double, [(Double, Double)]) -> Either D.Data (Either S.Spline FS.Functions) -> (Double -> IO ()) -> IO (Either D.Data (Either S.Spline FS.Functions), [(Double, Double)]) 
        findDispersions method corrLen (prevCorrLen, info) (Left dat) puFunc = do
            let
                mapFunc i puFunc = 
                    do
                        dispersionAndInfo <- 
                            case method of 
                                0 -> do --LeastSquares
                                    res <- leastSquares dat (1 / (freqStart + fromIntegral i * step))
                                    return (res, (0, 0))
                                1 -> do --StringLength
                                    do
                                        let 
                                            ymin = D.yMin dat
                                            yNorm = D.yMax dat - ymin
                                            vals = D.xys1 dat --map (\(x, y) -> (x, (y - ymin) / yNorm)) $ D.xys1 dat
                                        res <- stringLength vals (1 / (freqStart + fromIntegral i * step))
                                        return (res, (0, 0))
                        --puFunc $ (fromIntegral i) / (fromIntegral precision)
                        return dispersionAndInfo
            dispersionAndInfo <- calcConcurrently mapFunc puFunc [0 .. precision]
            let
                (dispersions, info) = unzip dispersionAndInfo
                norm = maximum dispersions
                normalizedDispersions =  
                    if False -- norm > 0 
                        then map (\d -> d / norm) dispersions 
                        else dispersions
                freqSpec = D.Spectrum ([(freqStart, step)], zip normalizedDispersions (replicate (length normalizedDispersions) 1))
            return (Left freqSpec, info)
    let
        namesCorrLenghts = [(name, 0)]
        corrLenFunc :: [(String, Double)] -> Double -> [[(Double, Double)]] -> IO [(Double, DataParams)]
        corrLenFunc [] _ _ = return []
        corrLenFunc ((name, corrLen):namesCorrLenghts) prevCorrLen prevInfos = do
            subDispersionsAndInfo <- mapM (\(sdp, prevInfo) -> do
                    let
                        Left dat = subData sdp
                    dat1 <- if bootstrap then reshuffleData dat else return dat
                    findDispersions method corrLen (prevCorrLen, prevInfo) (Left dat1) (puFunc)
                ) (zip (dataSet dataParams) prevInfos)
            let
                (subDispersions, infos) = unzip subDispersionsAndInfo
                dispersions = DataParams {
                        dataName = name, 
                        dataSet = map (\sd -> SubDataParams {subDataRange = U.dataRange sd, subData = sd, subDataBootstrapSet = []}) subDispersions
                    }
                    
            bestPeriods <- mapM (\(i, SubDataParams _ (Left periodSpec) _) -> do
                    let
                        freqDisps = sortBy (\(_, disp1) (_, disp2) -> compare disp1 disp2) $ V.toList $ D.getMinima periodSpec
                        freqs = map fst freqDisps
                    logFunc ("Possible freqs for " ++ name ++ "[" ++ show i ++ "] = " ++ (show freqs))
                    case freqDisps of 
                         [] -> return Nothing
                         ((freq, disp):_) -> return $ Just (1 / freq, (1 - disp) ^ 2)  
                ) (zip [1, 2 ..] (dataSet dispersions))
            let
                periodMean = meanWeighted $ V.fromList $ catMaybes bestPeriods
                periodVar = varianceWeighted $ V.fromList $ catMaybes bestPeriods
                periodDisp = mean $ V.map (\(p, d) -> 1 - sqrt d) $ V.fromList $ catMaybes bestPeriods
                
            logFunc ("Period mean stdev disp: " ++ show periodMean ++ " " ++ show periodVar ++ " " ++ show periodDisp)
            next <- corrLenFunc namesCorrLenghts corrLen infos
            return $ (corrLen, dispersions):next
    dispersions <- corrLenFunc namesCorrLenghts 0 (repeat (repeat (0, 0)))
    puFunc 0
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
                fitWithSpline (modulatedUnitPolynoms templates) 1 dat False 2 (\_ -> return ())
        let
            Left residue = binaryOp (F.subtr) (Left dat) (Right (Left spline)) True g
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
        
