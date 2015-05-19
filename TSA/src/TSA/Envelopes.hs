module TSA.Envelopes (envelopes) where


import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Functions as FS
import Regression.Utils as U
import qualified Math.Function as F

import TSA.Params
import TSA.Data
import TSA.LeastSquares
import TSA.SpecificPoints

import Utils.Misc
import Utils.Concurrent

import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Parallel as MP
import System.Random

import Control.Applicative


envelopes :: EnvParams -> (String, String, String) -> TaskEnv -> DataUpdateFunc String -> IO ()
envelopes params (upperName, lowerName, meanName) taskEnv (DataUpdateFunc dataUpdateFunc) =
    do
        g <- getStdGen 
        let
            Just dataParams = envData params
            Left dat = subData $ head $ dataSet $ dataParams
            puFunc = progressUpdateFunc taskEnv
        
            findExtremaOfOrder minimaDp maximaDp extremaSkipped = do
                if extremaSkipped < (envStartExtrema params - 1)
                    then do
                        (minimaDp, _) <- findExtrema minimaDp 0 False "extrema" defaultTaskEnv
                        (_, maximaDp) <- findExtrema maximaDp 0 False "extrema" defaultTaskEnv
                        findExtremaOfOrder minimaDp maximaDp  (extremaSkipped + 1)
                    else return (minimaDp, maximaDp)
        (minimaDp, maximaDp) <- findExtrema dataParams 0 False "extrema" defaultTaskEnv
        (minimaDpOfOrder, maximaDpOfOrder) <- findExtremaOfOrder minimaDp maximaDp 0
        let
            Left minima = subData $ head $ dataSet minimaDpOfOrder
            Left maxima = subData $ head $ dataSet maximaDpOfOrder
            numMinima = D.dataLength minima 
            numMaxima = D.dataLength maxima
            numExtrema = min numMinima numMaxima
            fitUpperParams = FitParams {
                        fitPolynomRank = 3,
                        fitType = FitTypeSpline,
                        fitSplineParams = SplineParams {splineNumNodes = ceiling ((fromIntegral numMinima) / 3)},
                        fitPeriod = 0,
                        fitNumHarmonics = 0
                    }
            fitLowerParams = FitParams {
                        fitPolynomRank = 3,
                        fitType = FitTypeSpline,
                        fitSplineParams = SplineParams {splineNumNodes = ceiling ((fromIntegral numMaxima) / 3)},
                        fitPeriod = 0,
                        fitNumHarmonics = 0
                    }
        if numExtrema > 1
            then do
                [upperEnv, lowerEnv] <- case envMethod params of
                    False -> MP.sequence [
                        fitData fitUpperParams maxima defaultTaskEnv,
                        fitData fitLowerParams minima defaultTaskEnv]
                    True -> MP.sequence [
                        R.interpolateWithSpline maxima,
                        R.interpolateWithSpline minima]
                let 
                    envMean = (upperEnv `S.add` lowerEnv) `S.divide` 2
                    residue = U.binaryOp (F.subtr) (Left dat) (Right (Left envMean)) True g
                            
                dataUpdateFunc (Right (Left upperEnv)) upperName False
                dataUpdateFunc (Right (Left lowerEnv)) lowerName False
                dataUpdateFunc (Right (Left envMean)) meanName False
                dataUpdateFunc residue ((dataName dataParams) ++ "_r") False
        else        
            return ()
        