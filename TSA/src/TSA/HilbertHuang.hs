module Main where

import Math.Function as F
import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import Regression.Bootstrap as B
import TSA.LeastSquares
import TSA.Extrema
import TSA.Envelopes
import TSA.AnalyticSignal
import TSA.Params

import Data.List as List
import System.Random
import System.Environment
import Filesystem
import Control.Concurrent.MVar
import Control.Monad

import qualified Utils.Xml as Xml
--import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Filesystem.Path.CurrentOS as F
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String as UTF8
import qualified Data.Vector.Unboxed as V

import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Test.KolmogorovSmirnov
import qualified Statistics.Sample as Sample

import System.Random
import System.Random.MWC

precision = 0.05

main :: IO ()
main = do --mpiWorld $ \size rank ->
    args <- getArgs
    let
        fileName = head args

    byteStr <- B.readFile fileName
    let
        xys :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ lines $ UTF8.decode $ B.unpack byteStr

        dat = D.data1' $ V.fromList xys
    imf 1 dat


imf :: Int -> Data -> IO ()
imf modeNo dat = do

    g <- getStdGen 
    let
        dataParams = DataParams {
            dataName = "data",
            dataSet = [
                SubDataParams {
                    subDataRange = U.dataRange (Left dat),
                    subData = (Left dat),
                    subDataBootstrapSet = []
                }
            ]
        }
    (minimaDp, maximaDp) <- findExtrema dataParams 0 "extrema" (\_ -> return ())
    let
        Left minima = subData $ head $ dataSet minimaDp 
        Left maxima = subData $ head $ dataSet maximaDp
        numExtrema = max (D.dataLength minima) (D.dataLength maxima)
    numNodes <- 
        if numExtrema > (D.dataLength dat) `div` 5
            then do
                (minimaDp, _) <- findExtrema minimaDp 0 "extrema" (\_ -> return ())
                (_, maximaDp) <- findExtrema maximaDp 0 "extrema" (\_ -> return ())
                let
                    Left minima = subData $ head $ dataSet minimaDp 
                    Left maxima = subData $ head $ dataSet maximaDp
                return $ max (D.dataLength minima) (D.dataLength maxima)
            else return numExtrema
    putStrLn $ "numNodes:" ++ show numNodes

    let
    
        imfStep :: Data -> Double -> IO Data
        imfStep dat sdev =
            do
        
                let
                    envParams = EnvParams {
                            envUpperParams = FitParams {
                                fitPolynomRank = 3,
                                fitType = FitTypeSpline,
                                fitSplineParams = SplineParams {splineNumNodes = numNodes}
                            },
                            envLowerParams = FitParams {
                                fitPolynomRank = 3,
                                fitType = FitTypeSpline,
                                fitSplineParams = SplineParams {splineNumNodes = numNodes}
                            },
                            envPrecision = 10,
                            envExtrema = EnvExtremaStatistical,
                            envData = Just (DataParams {
                                dataName = "data",
                                dataSet = [
                                    SubDataParams {
                                        subDataRange = U.dataRange (Left dat),
                                        subData = (Left dat),
                                        subDataBootstrapSet = []
                                    }
                                ]
                            })
                        } 
        
        
                Left dat2 <- envelopes envParams ("upper", "lower", "mean") (\_ -> return ()) (putStrLn) (DataUpdateFunc (\_ _ _ -> return ()))
                let 
                    sdev2 = U.stdev dat (Left dat2)
                            
                putStrLn $ "sdev=" ++ show sdev2 ++ ", needed=" ++ show sdev
                            
                if sdev2 < sdev
                    then 
                            return dat2
                    else
                        do
                            imfStep dat2 sdev

    let
        sdev = (Sample.stdDev (D.ys dat)) * precision

    imfDat <- imfStep dat sdev
    
    let
        byteStr = B.pack (UTF8.encode (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList (D.xys1 imfDat))))
    B.writeFile ("imf" ++ show modeNo ++ ".csv") byteStr
    
    let
        dataUpdateFunc = DataUpdateFunc $ \(Left dat) name _ -> do 
            case name of
                "amplitude" -> do
                    let
                        byteStr = B.pack (UTF8.encode (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList (D.xys1 dat))))
                    B.writeFile ("amplitude" ++ show modeNo ++ ".csv") byteStr
                "phase" -> return ()
                "frequency" -> return () 
                "conjugated" -> return ()
        asParams = AnalyticSignalParams {
                asRealData = Just (DataParams {
                        dataName = "imf",
                        dataSet = [
                            SubDataParams {
                                subDataRange = U.dataRange (Left imfDat),
                                subData = (Left imfDat),
                                subDataBootstrapSet = []
                            }
                        ]
                    }
                ),
                asImagData = Nothing
            }    
    analyticSignal asParams 0 ("amplitude", "phase", "frequency", "conjugated") (\_ -> return ()) (putStrLn) dataUpdateFunc 
    
    let 
        diff = D.subtr dat imfDat
        sdev2 = Sample.stdDev (D.ys diff)
    if sdev2 < sdev
        then 
                return ()
        else
            do
                imf (modeNo + 1) diff
