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

main :: IO ()
main = do --mpiWorld $ \size rank ->
    args <- getArgs
    let
        fileName = head args

    byteStr <- B.readFile fileName
    let
        xys :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ lines $ UTF8.decode $ B.unpack byteStr

        dat = D.data1' $ V.fromList xys
    imf dat


imf :: Data -> IO ()
imf dat = do

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
    
        imfStep :: Double -> Data -> IO Data
        imfStep sdev dat =
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
        
        
                Left dat2 <- envelopes envParams ("upper", "lower", "mean") (\_ -> return ()) (putStrLn) (\_ _ _ -> return ())
                let 
                    sdev2 = U.stdev dat (Left dat2)
                            
                putStrLn $ "sdev=" ++ show sdev2 ++ ", needed=" ++ show sdev
                            
                if sdev2 < sdev
                    then 
                            return dat2
                    else
                        do
                            imfStep sdev dat2

    imf <- imfStep 0.1 dat
    return ()
