
module TSA.LeastSquares (fitData) where

import Debug.Trace

import Math.Function as F
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U
import Regression.AnalyticData as AD
import Regression.Bootstrap as B

import TSA.Params

import Utils.Misc
import Utils.Xml

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import System.Environment
import Math.Expression
import System.Random

import qualified Data.Vector.Unboxed as V
import Statistics.Test.KolmogorovSmirnov
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Types
import qualified Statistics.Sample as Sample

import qualified Utils.Xml as Xml

{-
main :: IO ()
main = do --mpiWorld $ \size rank ->
    arg <- getArgs
    byteStr <- B.readFile "lock"
    let
        sks = head args
        sls = args !! 1
        sfreqs = read $ args !! 2
        ks = case elemIndex '-' sks of 
                Just i -> let (kMin, kMax) = split i sks in [read kMin .. read kMax]
                Nothing -> read sks 
        ls = case elemIndex '-' sls of 
                Just i -> let (lMin, lMax) = split i sls in [read lMin .. read lMax] 
                Nothing -> read sls 
        freqs = case elemIndex '-' sfreqs of 
                Just i -> let (minFreqStep, maxFreq) = split i sfreqs in 
                        case elemIndex ',' minFreqStep of
                        Just j -> let (minFreq, step) = split j minFreqStep in [read minFreq, read minFreq + read step .. read maxFreq]
                        Nothing -> [read minFreqStep, read minFreqStep + (read maxFreq - read minFreqStep) / 1000 .. read maxFreq]
                Nothing -> read sfreqs
        
    dat <- Xml.parseFromFile "data" "data" >>= \doc -> return (Xml.fromDocument doc)
    fitParams <- Xml.parseFromFile "fitparams" "fitparams" >>= \doc -> return (Xml.fromDocument doc)
    g <- getStdGen 
    if rank == 0 then
        do
            let splineParams = createDataParams_ ("spline" ++ show rank) [createSubDataParams__ (Right (Left spline))]
            let 
                Left diff = U.binaryOp F.subtr (Left dat) (Right (Left spline)) True g
            Xml.renderToFile (Xml.toDocument diff) "diff"
            splineExtrema <- findExtrema splineParams 1000000 (dataName splineParams) (\_ -> return ())
            let
                Left minima = subData $ head $ dataSet $ snd splineExtrema
            Xml.renderToFile (Xml.toDocument minima) "minima"
    else
        return ()
    unlockFile fileLock
-}

fitData :: FitParams -> Data -> TaskEnv -> IO Spline
fitData fitParams dat taskEnv = do
    let
        rank = fitPolynomRank fitParams
        fitTyp = fitType fitParams
        per = fitPeriod fitParams
        harmonics = fitNumHarmonics fitParams
    
    case fitTyp of
        FitTypeSpline -> do
            let
                numNodes = splineNumNodes (fitSplineParams fitParams)
            case per of 
                0 -> fitWithSpline_ rank numNodes dat 2 (progressUpdateFunc taskEnv)
                per -> do
                        let
                            templates =
                                [PolynomTemplate (rank, Nothing, Nothing)] ++ 
                                [PolynomTemplate (rank, Just (fromExpression (sine freq)), Just (fromExpression (dsin freq))) | freq <- freqs] ++
                                [PolynomTemplate (rank, Just (fromExpression (cosine freq)), Just (fromExpression (dcos freq))) | freq <- freqs]                         
                                    where
                                        freqs = [2 * pi / per * fromIntegral i | i <- [1 .. harmonics]]
                        --putStrLn ("modulatedUnitPolynoms: " ++ render (toDocument (modulatedUnitPolynoms templates)))
                        --putStrLn ("numNodes: " ++ show numNodes)
                        --putStrLn ("dat: " ++ render (toDocument (dat)))
                        fitWithSpline (modulatedUnitPolynoms templates) numNodes dat 2 (progressUpdateFunc taskEnv)
        FitTypeHarmonic -> do
            let
                coverageFactor = harmonicCoverageFactor (fitHarmonicParams fitParams)
                slowHarmonics = harmonicCount (fitHarmonicParams fitParams)
                x1 = D.xMin1 dat
                x2 = D.xMax1 dat
                carrierFreq = 2 * pi / per 
                freq = 2 * pi / (coverageFactor * (x2 - x1))
                -- First derivatives are needed in analytic signal calculations
                dSinCos f1 f2 = "(" ++ show f1 ++ "*cos("++ show f1 ++"*x)*cos("++ show f2 ++"*x))-(" ++ show f2 ++ "*sin("++ show f1 ++"*x)*sin("++ show f2 ++"*x))"
                dSinSin f1 f2 = "(" ++ show f1 ++ "*cos("++ show f1 ++"*x)*sin("++ show f2 ++"*x))+(" ++ show f2 ++ "*sin("++ show f1 ++"*x)*cos("++ show f2 ++"*x))"
                dCosSin f1 f2 = "-(" ++ show f1 ++ "*sin("++ show f1 ++"*x)*sin("++ show f2 ++"*x))+(" ++ show f2 ++ "*cos("++ show f1 ++"*x)*cos("++ show f2 ++"*x))"
                dCosCos f1 f2 = "-(" ++ show f1 ++ "*sin("++ show f1 ++"*x)*cos("++ show f2 ++"*x))-(" ++ show f2 ++ "*cos("++ show f1 ++"*x)*sin("++ show f2 ++"*x))"
                templates =
                    [PolynomTemplate (0, Nothing, Nothing)] ++
                    [PolynomTemplate (0, Just (fromExpression (sine slowFreq)), Just (fromExpression (dsin slowFreq))) | slowFreq <- slowFreqs] ++
                    [PolynomTemplate (0, Just (fromExpression (cosine slowFreq)), Just (fromExpression (cosine slowFreq))) | slowFreq <- slowFreqs] ++
                    concat [
                    [PolynomTemplate (0, Just (fromExpression (sine freq)), Just (fromExpression (dsin freq)))] ++
                    [PolynomTemplate (0, Just (fromExpression (cosine freq)), Just (fromExpression (dcos freq)))] ++
                    [PolynomTemplate (0, Just (function ("sin(" ++ show slowFreq ++ "*x)*cos(" ++ show freq ++ "*x)")), Just (function (dSinCos slowFreq freq))) | slowFreq <- slowFreqs] ++                         
                    [PolynomTemplate (0, Just (function ("sin(" ++ show slowFreq ++ "*x)*sin(" ++ show freq ++ "*x)")), Just (function (dSinSin slowFreq freq))) | slowFreq <- slowFreqs] ++                         
                    [PolynomTemplate (0, Just (function ("cos(" ++ show slowFreq ++ "*x)*sin(" ++ show freq ++ "*x)")), Just (function (dCosSin slowFreq freq))) | slowFreq <- slowFreqs] ++                         
                    [PolynomTemplate (0, Just (function ("cos(" ++ show slowFreq ++ "*x)*cos(" ++ show freq ++ "*x)")), Just (function (dCosCos slowFreq freq))) | slowFreq <- slowFreqs]
                     | freq <- freqs]
                        where
                            slowFreqs = [freq * fromIntegral i | i <- [1 .. slowHarmonics]]
                            freqs = [carrierFreq * fromIntegral i | i <- [1 .. harmonics]]
            putStrLn ("num parameters: " ++ (show (Prelude.length templates)))
            --putStrLn ("modulatedUnitPolynoms: " ++ render (toDocument (modulatedUnitPolynoms templates)))
            --putStrLn ("dat: " ++ render (toDocument (dat)))
            fitWithSpline (modulatedUnitPolynoms templates) 1 dat 0 (progressUpdateFunc taskEnv)

