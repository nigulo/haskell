module TSA.Correlation (findCorrelation) where

import Regression.Data as D
import Regression.Utils as U
import Regression.AnalyticDataWrapper as ADW
import qualified Math.Function as F
import Utils.Concurrent

import TSA.CommonParams
import TSA.Params
import TSA.Data

import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative
import Debug.Trace

import Statistics.LinearRegression
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Test.KolmogorovSmirnov
import Statistics.Resampling.Bootstrap
import System.Random
import System.Random.MWC


findCorrelation :: TaskEnv -> DataParams -> DataParams -> Double -> [Double] -> String -> IO [Data]
findCorrelation taskEnv dataParams1 dataParams2' precision shifts name = do
    g <- getStdGen 
    let 
        getRange dataParams =
            case unboxSubData $ subData $ head $ dataSet dataParams of
                Left d -> (D.xMin1 d, D.xMax1 d) 
                Right ad -> (ADW.xMin1 ad, ADW.xMax1 ad)
        shiftData dataParams 0 = return dataParams
        shiftData dataParams shift = do
            let 
                func i j d _ = return $ subDataConstantOp (F.add) d shift False g
            applyToData1 func dataParams "" taskEnv
        (xMin1, xMax1) = getRange dataParams1
        mapFunc shift puFunc = do 
            dataParams2 <- shiftData dataParams2' shift
            let 
                (xMin2, xMax2) = getRange dataParams2
        
                xMin = max xMin1 xMin2
                xMax = min xMax1 xMax2
            if xMin < xMax then 
                do
                    let
                        maybeYs sdp1 sdp2 = 
                            case (subData sdp1, subData sdp2) of
                                (SD1 d1, SD1 d2) ->
                                    let 
                                        subD1 = D.subSet1 (xMin, xMax) d1  
                                        subD2 = D.subSet1 (xMin, xMax) d2  
                                    in
                                    if D.xs subD1 == D.xs subD2 then Just (D.ys subD1, D.ys subD2) else Nothing
                                otherwise -> Nothing
                        (ys1, ys2) = 
                            case maybeYs (head $ dataSet dataParams1) (head $ dataSet dataParams2) of
                                Just (ys1, ys2) -> (ys1, ys2)
                                otherwise ->
                                    let
                                        step =  (xMax - xMin) / precision
                                        xs = map (\x -> [x]) (init [xMin, xMin + step .. xMax] ++ [xMax])
                                        (_, ys1) = unzip $ U.getValues xs (unboxSubData $ subData $ head $ dataSet dataParams1) g
                                        (_, ys2) = unzip $ U.getValues xs (unboxSubData $ subData $ head $ dataSet dataParams2) g
                                    in
                                        (V.fromList ys1, V.fromList ys2)
                        (alpha, beta, r2) = linearRegressionRSqr ys1 ys2
                        (alphaDisp, betaDist) = linearRegressionDistributions (alpha, beta) ys1 ys2
                        Just stdBeta = maybeStdDev betaDist
                        stdYs1 = stdDev $ normalFromSample ys1
                        stdYs2 = stdDev $ normalFromSample ys2
                        stdR = stdBeta * stdYs1 / stdYs2
                    rndVects <- mapM (\_ -> do rndVect :: V.Vector Int <- withSystemRandom . asGenST $ \gen -> uniformVector gen (V.length ys1); return rndVect) [0 .. 999]
                    let
                        resamples = map (\rndVect -> V.unzip (V.map (\r -> let i = r `mod` (V.length ys1) in (ys1 V.! i, ys2 V.! i)) rndVect)) rndVects
                        correls = Data.List.sort $ map (\(ys1, ys2) -> correl ys1 ys2) resamples
                        --stdCorrels = stdDev $ normalFromSample (V.fromList correls)
                        ksStat = kolmogorovSmirnovD (normalFromSample (V.fromList correls)) (V.fromList correls) 
                    --(logFunc taskEnv) stateRef ("alpha, beta" ++ show alpha ++ ", " ++ show beta)
                    (logFunc taskEnv) ("Correlation coefficient for " ++ name ++ ", shift=" ++ show shift ++ ": " ++ (show (signum beta * sqrt r2)) ++ ", 90% confInt=[" ++ (show (correls !! 49)) ++ ", " ++ (show (correls !! 949)) ++ "]")
                    return $ D.data1' (V.zip ys1 ys2)
                else return $ D.data1' V.empty
    calcConcurrently mapFunc (progressUpdateFunc taskEnv) (taskInitializer taskEnv) (taskFinalizer taskEnv) shifts

