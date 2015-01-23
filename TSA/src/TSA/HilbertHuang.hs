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
import System.Random.MWC.Distributions

precision = 0.05

main :: IO ()
main = do --mpiWorld $ \size rank ->
    args <- getArgs
    let
        fileName = head args
        noisePercent :: Double = if (length args > 1) then read (args !! 1) else 0
        count :: Double = if (length args > 2) then read (args !! 2) else 1

    byteStr <- B.readFile fileName
    let
        xys :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ lines $ UTF8.decode $ B.unpack byteStr

        dat = D.data1' $ V.fromList xys
        sdev = Sample.stdDev (D.ys dat)
        noiseSdev = sdev * noisePercent
    
    g <- getStdGen 
    gen <- createSystemRandom

    
    imfSums <- foldM (\res _ -> do
        datWithNoise <- if noisePercent > 0 
            then do
                valsWithNoise <-
                    V.mapM (\(x, y, w) -> do
                            r :: Double <- asGenIO (normal 0 noiseSdev) gen
                            return (x, y + r, w)
                        ) (D.values1 dat)
                return $ D.data1 valsWithNoise
            else 
                return dat

        imfs <- imf 1 datWithNoise
        if length res == 0 
            then
                return imfs
            else
                return $ zipWith (\dat1 dat2 -> 
                        let 
                            Left datSum = U.binaryOp (F.add) (Left dat) (Left dat2) True g
                        in 
                            datSum
                    ) res imfs 
        ) [] [1 .. count]
    
    let
        imfMeans = if count > 1 
            then
                map (\dat ->
                        let 
                            Left datMean = U.constantOp (F.mult) (Left dat) (1 / count) True
                        in 
                            datMean
                    ) imfSums
            else
                imfSums
                
    zipWithM_ (\modeNo dat -> do
            storeData dat ("imf" ++ show modeNo)
            calcAnalyticSignal dat modeNo
        ) [1 ..] imfMeans
        
    let 
        recDat = foldl1 (\res dat -> 
                let 
                    Left datSum = U.binaryOp (F.add) (Left res) (Left dat) True g
                in 
                    datSum
            ) imfMeans
        diff = D.subtr dat recDat
        sdev2 = Sample.stdDev (D.ys diff)
    putStrLn $ "Reconstruction error: " ++ show (sdev2 / sdev)

storeData :: Data -> String -> IO ()
storeData dat name = do
    let
        byteStr = B.pack (UTF8.encode (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList (D.xys1 dat))))
    B.writeFile (name ++ ".csv") byteStr

calcAnalyticSignal :: Data -> Int -> IO ()
calcAnalyticSignal imfDat modeNo = do
    let
        dataUpdateFunc = DataUpdateFunc $ \(Left dat) name _ -> do 
            case name of
                "amplitude" -> do
                    storeData dat ("amplitude" ++ show modeNo)
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

imf :: Int -> Data -> IO [Data]
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
        numExtrema = min (D.dataLength minima) (D.dataLength maxima)
    if numExtrema > 1
        then do
            numNodes <- 
                if numExtrema > (D.dataLength dat) `div` 5
                    then do
                        (minimaDp, _) <- findExtrema minimaDp 0 "extrema" (\_ -> return ())
                        (_, maximaDp) <- findExtrema maximaDp 0 "extrema" (\_ -> return ())
                        let
                            Left minima = subData $ head $ dataSet minimaDp 
                            Left maxima = subData $ head $ dataSet maximaDp
                        return $ (min (D.dataLength minima) (D.dataLength maxima)) - 1
                    else return (numExtrema - 1)
            putStrLn $ "numNodes:" ++ show numNodes
        
            let
                sdev = (Sample.stdDev (D.ys dat)) * precision
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
        
            imfDat <- imfStep dat sdev
            
            --storeData imfDat ("imf" ++ show modeNo)
            --calcAnalyticSignal imfDat modeNo
            
            let 
                diff = D.subtr dat imfDat
            --    sdev2 = Sample.stdDev (D.ys diff)
            --if sdev2 < sdev
            --    then 
            --            return ()
            --    else
            --        do
            imf (modeNo + 1) diff >>= \otherImfs -> return (imfDat:otherImfs)
        else
            return [dat]
