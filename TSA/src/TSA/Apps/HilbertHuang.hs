module Main where

import Math.Function as F
import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import TSA.LeastSquares
import TSA.SpecificPoints
import TSA.AnalyticSignal
import TSA.Params
import TSA.Data
import Utils.IO

import Data.List as List
import Data.Char
import System.Random
import System.Environment
import System.IO
import System.CPUTime
import System.Directory
import Filesystem
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import qualified Control.Monad.Parallel as MP

import qualified Utils.Xml as Xml
import Filesystem.Path.CurrentOS as F
import qualified Data.Vector.Unboxed as V

import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Test.KolmogorovSmirnov
import qualified Statistics.Sample as Sample

import Control.Concurrent
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
    if args == []
        then
            collect
        else
            calc args

calc :: [String] -> IO ()
calc args = do
    let
        fileName = head args
        modesToSkip :: Double = if (length args > 1) then read (args !! 1) else 0
        downSample :: Int = if (length args > 2) then read (args !! 2) else 1
        noisePercent :: Double = if (length args > 3) then read (args !! 3) else 0
        count :: Double = if (length args > 4) then read (args !! 4) else 1

    putStrLn $ "Processing " ++ show fileName
    let
        logFilePrefix = case elemIndex '.' fileName of
            Just i -> (take i fileName)
            Nothing -> fileName
        env = Env modesToSkip logFilePrefix
    exists <- doesFileExist (logFilePrefix ++ ".log")
    if exists then return () else do
        str <- Utils.IO.readFromFile fileName
        let
            xys :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ drop numLinesToSkip $ lines $ str
            xysList = V.fromList xys
    
            dat = D.data1' $ V.generate (V.length xysList `div` downSample) (\i -> xysList V.! (i * downSample))
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
                    
        logTexts <- MP.mapM (\(modeNo, (freq, dat)) -> do
                runReaderT (calcAnalyticSignal dat modeNo freq) env
            ) $ zip [1 ..] imfMeans
        mapM_ (\logText -> do
                putStrLn logText 
                appendToFile (logFilePrefix ++ ".log") (logText ++ "\n")
            ) logTexts
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
        str = (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList (D.xys1 dat)))
    Utils.IO.writeToFile (fileName ++ ".csv") str

calcAnalyticSignal :: Data -> Int -> Double -> ReaderT Env (IO) (String {-logText-})
calcAnalyticSignal imfDat modeNo freq = do
    let
        asParams = AnalyticSignalParams {
                asRealData = Just (createDataParams_ "imf" [createSubDataParams__ (Left imfDat)]),
                asImagData = Nothing
            }
    env <- ask
    asData <- liftIO $ analyticSignal asParams 0 ("amplitude", "phase", "frequency", "conjugated") defaultTaskEnv (DataUpdateFunc (\_ _ _ -> return ()))
    let
        Just (Left frequency) = lookup "frequency" asData
        Just (Left amplitude) = lookup "amplitude" asData
    let
        --(freqMean, freqVar) = Sample.meanVarianceUnb $ D.ys frequency
        (enMean, enVar) = Sample.meanVarianceUnb $ V.map (^2) $ D.ys amplitude
        logText = show modeNo ++ ": " ++ show freq ++
             " " ++ show enMean ++ " " ++  show (sqrt enVar)
    liftIO $ storeData imfDat (logFilePrefix env ++ "_imf_" ++ show modeNo)
    liftIO $ storeData frequency (logFilePrefix env ++ "_freq_" ++ show modeNo)
    liftIO $ storeData amplitude (logFilePrefix env ++ "_amp_" ++ show modeNo)
    return logText

imf :: Int -> Data 
    -> ReaderT Env (IO) [(Double, Data)] -- ^ Mean frequency and data
imf modeNo dat = do

    g <- liftIO $ getStdGen
    env <- ask
    let
        findExtremaOfOrder minimaDp maximaDp modesSkipped = do
            if modeNo == 1 && modesSkipped < (modesToSkip env)
                then do
                    (minimaDp, _) <- findExtrema minimaDp 0 "extrema" defaultTaskEnv
                    (_, maximaDp) <- findExtrema maximaDp 0 "extrema" defaultTaskEnv
                    findExtremaOfOrder minimaDp maximaDp  (modesSkipped + 1)
                else return (minimaDp, maximaDp)
        datSDev = Sample.stdDev (D.ys dat)
        sdev = datSDev * precision
        dataParams = createDataParams_ "data" [createSubDataParams__ (Left dat)]
    (minimaDp, maximaDp) <- liftIO $ findExtrema dataParams 0 "extrema" defaultTaskEnv
    (minimaDpOfOrder, maximaDpOfOrder) <- liftIO $ findExtremaOfOrder minimaDp maximaDp 0
    let
        Left minima = subData $ head $ dataSet minimaDpOfOrder
        Left maxima = subData $ head $ dataSet maximaDpOfOrder
        numMinima = D.dataLength minima 
        numMaxima = D.dataLength maxima
        numExtrema = min numMinima numMaxima
        numLowerNodes = ceiling ((fromIntegral numMaxima) / 3)
        numUpperNodes = ceiling ((fromIntegral numMinima) / 3)
    if numExtrema > 1
        then do
            let
                imfStep :: Data -> Data -> Data -> Double -> Int -> Int -> Int -> IO Data
                imfStep stepDat stepMinima stepMaxima prevSdev numUpperNodes numLowerNodes i =
                    do
                        let
                            fitUpperParams = FitParams {
                                        fitPolynomRank = 3,
                                        fitType = FitTypeSpline,
                                        fitSplineParams = SplineParams {splineNumNodes = numUpperNodes},
                                        fitPeriod = 0,
                                        fitNumHarmonics = 0
                                    }
                            fitLowerParams = FitParams {
                                        fitPolynomRank = 3,
                                        fitType = FitTypeSpline,
                                        fitSplineParams = SplineParams {splineNumNodes = numLowerNodes},
                                        fitPeriod = 0,
                                        fitNumHarmonics = 0
                                    }
                
                        [upperEnv, lowerEnv] <- MP.sequence [
                            fitData fitUpperParams stepMaxima defaultTaskEnv,
                            fitData fitLowerParams stepMinima defaultTaskEnv]

                        let 
                            envMean = (upperEnv `S.add` lowerEnv) `S.divide` 2
                            Left dat2 = U.binaryOp (F.subtr) (Left stepDat) (Right (Left envMean)) True g
                            Left maxima2 = U.binaryOp (F.subtr) (Left stepMaxima) (Right (Left envMean)) True g
                            Left minima2 = U.binaryOp (F.subtr) (Left stepMinima) (Right (Left envMean)) True g
                            sdev2 = U.stdev stepDat (Left dat2)
                        
                        --putStrLn $ "sdev: " ++ show sdev2 ++ ", needed: " ++ show sdev
                        --if modeNo == 3 then storeData stepDat ("last_imf" ++ show i) else return ()
                        if sdev2 < sdev
                            then 
                                    return dat2
                            else
                                if i > 10 && (sdev2 > prevSdev / 2) -- || sdev2 > datSDev * 2)
                                    then
                                        -- diverging? increase number of nodes
                                        imfStep dat minima maxima 0 (numUpperNodes + 1) (numLowerNodes + 1) 0
                                    else
                                        imfStep dat2 minima2 maxima2 sdev2 numUpperNodes numLowerNodes (i + 1)
    
            imfDat <- liftIO $ do
                putStr $ "Extracting mode " ++ show modeNo ++ " (" ++ show numExtrema ++ ") ... "
                hFlush stdout
                time1 <- getCPUTime
                imfDat <- imfStep dat minima maxima 0 numUpperNodes numLowerNodes 0
                time2 <- getCPUTime
                putStrLn $ "done in " ++ show (fromIntegral (time2 - time1) / 1e12) ++ " CPU secs"
                return imfDat
                
            let 
                diff = D.subtr dat imfDat
            imf (modeNo + 1) diff >>= \otherImfs -> return ((fromIntegral numExtrema / (D.xMax1 imfDat - D.xMin1 imfDat), imfDat):otherImfs)
        else
            return [(0, dat)]

collect :: IO ()
collect = do
    workDir <- getWorkingDirectory
    fileNames <- listDirectory workDir >>= \names -> filterM (isFile) names
    allModes' <- mapM (\fileName -> do
            str <- Utils.IO.readFromFile fileName
            putStrLn str
            let
                lat_r = dropWhile isLetter $ take (length fileName - 4) fileName
                Just i = elemIndex '_' lat_r
                lat :: Double = read $ take i lat_r
                r :: Double = read $ drop (i + 1) lat_r
                modes :: [(Double, Double, Double, Double, Double)] = map (\line -> 
                        let 
                            [_, freq, en, enStd] = words line 
                        in 
                            (lat, r, read freq, read en, read enStd)
                    ) $ lines $ str
            return modes
        ) $ filter (isSuffixOf ".log") $ map (map (toLower) . F.encodeString . filename) fileNames
    let
        --maxModes = maximum $ map length allModes'
        --allModes = List.transpose $ map (\modes@((lat, r, _, _, _):_) -> modes ++ replicate (maxModes - length modes) (lat, r, 0, 0, 0)) allModes'
        allModes = List.transpose allModes'
    zipWithM_ (\mode modeNo -> do
            let
                sortedMode = sortBy (\(lat1, r1, _, _, _) (lat2, r2, _, _, _) -> compare (lat1, r1) (lat2, r2)) mode
                sortedModeWithPrev = zip sortedMode (tail sortedMode ++ [last sortedMode])
                blankLine ((lat, _, _, _, _), (prevLat, _, _, _, _)) = if lat /= prevLat then "\n" else "" 
                ens = concatMap (\line@((lat, r, freq, en, enStd), _) -> show lat ++ " " ++ show r ++ " " ++ show en ++ "\n" ++ (blankLine line)) sortedModeWithPrev
                freqs = concatMap (\line@((lat, r, freq, en, enStd), _) -> show lat ++ " " ++ show r ++ " " ++ show freq ++ "\n" ++ (blankLine line)) sortedModeWithPrev
            Utils.IO.writeToFile ("ens" ++ show modeNo ++ ".csv") ens
            Utils.IO.writeToFile ("freqs" ++ show modeNo ++ ".csv") freqs
        ) allModes [1 ..]
        
        
        