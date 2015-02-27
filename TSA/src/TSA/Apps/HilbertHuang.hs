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
import TSA.Data
import Utils.IO

import Data.List as List
import System.Random
import System.Environment
import System.IO
import System.CPUTime
import Filesystem
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader

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
numLinesToSkip = 1

data Env = Env {
    modesToSkip :: Double,
    logFilePrefix :: String
    }
    

main :: IO ()
main = do
    args <- getArgs
    let
        fileName = head args
        modesToSkip :: Double = if (length args > 1) then read (args !! 1) else 0
        noisePercent :: Double = if (length args > 2) then read (args !! 2) else 0
        count :: Double = if (length args > 3) then read (args !! 3) else 1

    putStrLn $ "Processing " ++ show fileName
    let
        logFilePrefix = case elemIndex '.' fileName of
            Just i -> (take i fileName)
            Nothing -> fileName
        env = Env modesToSkip logFilePrefix
    byteStr <- B.readFile fileName
    let
        xys :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ drop numLinesToSkip $ lines $ UTF8.decode $ B.unpack byteStr

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

        imfs <- runReaderT (imf 1 datWithNoise) env
        if length res == 0 
            then
                return imfs
            else
                return $ zipWith (\(freq1, dat1) (freq2, dat2) -> 
                        let 
                            Left datSum = U.binaryOp (F.add) (Left dat) (Left dat2) True g
                        in 
                            (freq1 + freq2, datSum)
                    ) res imfs 
        ) [] [1 .. count]
    
    let
        imfMeans = if count > 1 
            then
                map (\(freq, dat) ->
                        let 
                            Left datMean = U.constantOp (F.mult) (Left dat) (1 / count) True
                        in 
                            (freq / count, datMean)
                    ) imfSums
            else
                imfSums
                
    zipWithM_ (\modeNo (freq, dat) -> do
            --storeData dat ("imf" ++ show modeNo)
            runReaderT (calcAnalyticSignal dat modeNo freq) env
        ) [1 ..] imfMeans
        
    let 
        recDat = foldl1 (\res dat -> 
                let 
                    Left datSum = U.binaryOp (F.add) (Left res) (Left dat) True g
                in 
                    datSum
            ) (map (snd) imfMeans)
        diff = D.subtr dat recDat
        sdev2 = Sample.stdDev (D.ys diff)
    putStrLn $ "Reconstruction error: " ++ show (sdev2 / sdev)

storeData :: Data -> String -> IO ()
storeData dat fileName = do
    let
        byteStr = B.pack (UTF8.encode (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList (D.xys1 dat))))
    B.writeFile (fileName ++ ".csv") byteStr

calcAnalyticSignal :: Data -> Int -> Double -> ReaderT Env (IO) ()
calcAnalyticSignal imfDat modeNo freq = do
    let
        asParams = AnalyticSignalParams {
                asRealData = Just (createDataParams_ "imf" [createSubDataParams__ (Left imfDat)]),
                asImagData = Nothing
            }
    env <- ask
    asData <- liftIO $ analyticSignal asParams 0 ("amplitude", "phase", "frequency", "conjugated") (\_ -> return ()) (putStrLn) (DataUpdateFunc (\_ _ _ -> return ()))
    let
        Just (Left frequency) = lookup "frequency" asData
        Just (Left amplitude) = lookup "amplitude" asData
    let
        freqVals = D.ys frequency
        amplitudeVals = D.ys amplitude
        logText = show modeNo ++ ": " ++ show (Sample.mean freqVals / 2 / pi) ++ " " {- ++ "(" ++ show freq  ++ ") "-} ++  show (Sample.stdDev freqVals / 2 / pi) ++
             " " ++ show (Sample.mean amplitudeVals) ++ " " ++  show (Sample.stdDev amplitudeVals)
    liftIO $ putStrLn logText 
    --liftIO $ storeData frequency ("frequency" ++ show modeNo)
    --liftIO $ storeData amplitude ("amplitude" ++ show modeNo)
    liftIO $ appendToFile (logFilePrefix env ++ ".log") (logText ++ "\n")

imf :: Int -> Data 
    -> ReaderT Env (IO) [(Double, Data)] -- ^ Mean frequency and data
imf modeNo dat = do

    g <- liftIO $ getStdGen
    env <- ask
    let
        dataParams = createDataParams_ "data" [createSubDataParams__ (Left dat)]

    (minimaDp, maximaDp) <- liftIO $ findExtrema dataParams 0 "extrema" (\_ -> return ())
    let
        Left minima = subData $ head $ dataSet minimaDp 
        Left maxima = subData $ head $ dataSet maximaDp
        numExtrema = min (D.dataLength minima) (D.dataLength maxima)
        
        findNumNodes minimaDp maximaDp numExtrema modesSkipped = do
            if modeNo == 1 && modesSkipped < (modesToSkip env)
                then do
                    (minimaDp, _) <- findExtrema minimaDp 0 "extrema" (\_ -> return ())
                    (_, maximaDp) <- findExtrema maximaDp 0 "extrema" (\_ -> return ())
                    let
                        Left minima = subData $ head $ dataSet minimaDp 
                        Left maxima = subData $ head $ dataSet maximaDp
                    findNumNodes minimaDp maximaDp (min (D.dataLength minima) (D.dataLength maxima)) (modesSkipped + 1)
                else return (numExtrema - 1)
    if numExtrema > 1
        then do
            numNodes <- liftIO $ findNumNodes minimaDp maximaDp numExtrema 0
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
                                    envPrecision = 1 / precision,
                                    envExtrema = EnvExtremaStatistical,
                                    envData = Just (DataParams {
                                        dataName = "data",
                                        dataSet = [createSubDataParams__ (Left dat)]
                                    })
                                } 
                
                
                        Left dat2 <- envelopes envParams ("upper", "lower", "mean") (\_ -> return ()) (putStrLn) (DataUpdateFunc (\_ _ _ -> return ()))
                        let 
                            sdev2 = U.stdev dat (Left dat2)
                                    
                        if sdev2 < sdev
                            then 
                                    return dat2
                            else
                                do
                                    imfStep dat2 sdev
        
            imfDat <- liftIO $ do
                putStr $ "Calculating (number of nodes = " ++ show numNodes ++ ") ... "
                hFlush stdout
                time1 <- getCPUTime
                imfDat <- imfStep dat sdev
                time2 <- getCPUTime
                putStrLn $ "done in " ++ show (fromIntegral (time2 - time1) / 1e12) ++ " CPU secs"
                return imfDat
            
            
            let 
                diff = D.subtr dat imfDat
            imf (modeNo + 1) diff >>= \otherImfs -> return ((fromIntegral numNodes / (D.xMax1 imfDat - D.xMin1 imfDat) , imfDat):otherImfs)
        else
            return [(0, dat)]
