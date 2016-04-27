module Main where

import Math.Function as F
import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import Regression.Regression as R
import TSA.LeastSquares
import TSA.SpecificPoints
import TSA.AnalyticSignal
import TSA.CommonParams
import TSA.RegressionParams
import TSA.Params
import TSA.Data
import Utils.IO
import qualified Utils.Misc as Misc

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

precision = 0.01
numLinesToSkip = 1
interpolateOrFit = True

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
    
            dat = D.data1' $ V.generate (V.length xysList `div` downSample) (\i -> 
                    let 
                        (xSum, ySum) = foldl' (\(xSum, ySum) j -> let (x, y) = xysList V.! j in (xSum + x, ySum + y)) (0, 0) [i * downSample .. i * downSample + downSample - 1]
                    in
                        (xSum / fromIntegral downSample, ySum / fromIntegral downSample)
                )
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
    
            imfs <- runReaderT (imf 1 datWithNoise sdev) env
            if length res == 0 
                then
                    return imfs
                else
                    return $ zipWith (\(freq1, dat1) (freq2, dat2) -> 
                            let 
                                SD1 datSum = subDataBinaryOp (F.add) (SD1 dat) (SD1 dat2) True g
                            in 
                                (freq1 + freq2, datSum)
                        ) res imfs 
            ) [] [1 .. count]
    
        let
            imfMeans = if count > 1 
                then
                    map (\(freq, dat) ->
                            let 
                                SD1 datMean = subDataConstantOp (F.mult) (SD1 dat) (1 / count) True g
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
                        SD1 datSum = subDataBinaryOp (F.add) (SD1 res) (SD1 dat) True g
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
                asRealData = Just (createDataParams_ "imf" [createSubDataParams__ (SD1 imfDat)]),
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
    liftIO $ 
        do
            storeData imfDat (logFilePrefix env ++ "_imf_" ++ show modeNo)
            storeData frequency (logFilePrefix env ++ "_freq_" ++ show modeNo)
            storeData amplitude (logFilePrefix env ++ "_amp_" ++ show modeNo)
    return logText

imf :: Int -> Data -> Double
    -> ReaderT Env (IO) [(Double, Data)] -- ^ Mean frequency and data
imf modeNo dat fullSDev = do

    g <- liftIO $ getStdGen
    env <- ask
    let
        findExtrema d =
            let
                (minima, maxima) = D.getExtrema d False
            in
                (minima, maxima) 

        findExtremaOfOrder (minima, maxima) modesSkipped =
            if modeNo == 1 && modesSkipped < (modesToSkip env)
                then 
                    let
                        (minima1, _) = findExtrema (D.data1' minima)
                        (_, maxima1) = findExtrema (D.data1' maxima)
                    in
                        findExtremaOfOrder (minima1, maxima1) (modesSkipped + 1)
                else
                    (minima, maxima)
        (datMean, datVar) = Sample.meanVariance (D.ys dat)
        xMin = D.xMin1 dat
        xMax = D.xMax1 dat
        sdev = precision * sqrt datVar
        (minima, maxima) = findExtremaOfOrder (findExtrema dat) 0
    let
        numMinima = V.length minima 
        numMaxima = V.length maxima
        numExtrema = min numMinima numMaxima
        numLowerNodes = ceiling $ (fromIntegral numMaxima)
        numUpperNodes = ceiling $ (fromIntegral numMinima)
    if numExtrema > 2 -- should be 1, but there are cases when iteration doesn't converge (NZC=2, NE=4)
        then do
            let
                imfStep :: Data -> V.Vector (Double, Double) -> V.Vector (Double, Double) -> Double -> Int -> Int -> Int -> IO (Data, Int)
                imfStep stepDat stepMinima stepMaxima prevSDev numLowerNodes numUpperNodes i =
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
                        

                        let
                            vals = D.xys1 stepDat
                            firstVal = V.head vals
                            lastVal = V.last vals
                            stepMinima1 =
                                if snd firstVal < -prevSDev then V.cons firstVal stepMinima else stepMinima
                            stepMaxima1 =
                                if snd firstVal > prevSDev then V.cons firstVal stepMaxima else stepMaxima
                            stepMinima2 =
                                if snd lastVal < -prevSDev then V.snoc stepMinima1 lastVal else stepMinima1
                            stepMaxima2 =
                                if snd lastVal > prevSDev then V.snoc stepMaxima1 lastVal else stepMaxima1
                        [lowerEnv, upperEnv] <- if interpolateOrFit
                            then 
                                sequence [
                                    R.interpolateWithSpline $ D.data1' stepMinima, 
                                    R.interpolateWithSpline $ D.data1' stepMaxima]
                            else
                                MP.sequence [
                                    fitData fitLowerParams (D.data1' stepMinima) defaultTaskEnv,
                                    fitData fitUpperParams (D.data1' stepMaxima) defaultTaskEnv]
                        let 
                            envMean = (upperEnv `S.add` lowerEnv) `S.divide` 2
                            SD1 dat2 = subDataBinaryOp (F.subtr) (SD1 stepDat) (SD2 envMean) True g
                            (datMean, datVar) = Sample.meanVariance $ D.ys dat2 --V.map snd $ filteredVals
                            (minima2, maxima2) = findExtremaOfOrder (findExtrema dat2) 0
                            numLowerNodes2 = ceiling $ (fromIntegral (V.length minima2))
                            numUpperNodes2 = ceiling $ (fromIntegral (V.length maxima2))
                            zeroCrossings = D.getZeroCrossings dat2
                            numExtrema = V.length minima2 + V.length maxima2
                            numZeroCrossings = V.length zeroCrossings
                        
                        putStrLn $ "M: " ++ show datMean ++ ", V: " ++ show datVar ++ ", NE: " ++ show numExtrema ++ ", NZC: " ++ show numZeroCrossings
                        --putStrLn $ "sdev: " ++ show sdev2 ++ ", needed: " ++ show sdev
                        if i == 100 then storeData stepDat ("last_imf" ++ show i) else return ()
                        if fromIntegral (numExtrema - numZeroCrossings) <= 1
                        --if abs datMean < 3 * sqrt (datVar / fromIntegral (D.dataLength dat2)) && fromIntegral (numExtrema - numZeroCrossings) <= max 1 (precision * fromIntegral numZeroCrossings)
                            then 
                                    return (dat2, numZeroCrossings)
                            else
                            --    if i > 5 && (sdev2 > prevSdev / 2) -- || sdev2 > datSDev * 2)
                            --        then
                            --            -- diverging? increase number of nodes
                            --            imfStep dat minima maxima 0 (numUpperNodes + 1) (numLowerNodes + 1) 0
                            --        else
                                        imfStep dat2 minima2 maxima2 (sqrt datVar) numLowerNodes2 numUpperNodes2 (i + 1)
    
            (imfDat, numZeroCrossings) <- liftIO $ do
                putStr $ "Extracting mode " ++ show modeNo ++ " (" ++ show numExtrema ++ ") ... "
                hFlush stdout
                time1 <- getCPUTime
                (imfDat, numZeroCrossings) <- imfStep dat minima maxima (sqrt datVar) numLowerNodes numUpperNodes 0
                time2 <- getCPUTime
                putStrLn $ "done in " ++ show (fromIntegral (time2 - time1) / 1e12) ++ " CPU secs"
                return (imfDat, numZeroCrossings) 
                
            let 
                diff = D.subtr dat imfDat
            imf (modeNo + 1) diff fullSDev >>= \otherImfs -> return ((0.5 * fromIntegral numZeroCrossings / (D.xMax1 imfDat - D.xMin1 imfDat), imfDat):otherImfs)
        else
            return [(0, dat)]

normalize = True

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
                modes :: [(Double, Double, Double, Double)] = map (\line -> 
                        let 
                            [_, freq, en, enStd] = words line 
                        in 
                            (lat, r, read freq, read en)
                    ) $ lines $ str
            return modes
        ) $ filter (isSuffixOf ".log") $ map (map (toLower) . F.encodeString . filename) fileNames
    let
        allModes'' = List.transpose allModes'
        totalEnergies = map (\modes -> foldl' (\s1 (_, _, _, en) -> s1 + en) 0 modes) allModes''
        totalEnergy = sum totalEnergies
        allModes = if normalize 
            then 
                map (map (\(lat, r, freq, en) -> (lat, r, freq, en / totalEnergy))) allModes'' 
            else 
                allModes''
    putStrLn "Relative energies of the modes:"
    print $ map (/totalEnergy) totalEnergies
    zipWithM_ (\mode modeNo -> do
            let
                sortedMode = sortBy (\(lat1, r1, _, _) (lat2, r2, _, _) -> compare (lat1, r1) (lat2, r2)) mode
                sortedModeWithPrev = zip sortedMode (tail sortedMode ++ [last sortedMode])
                blankLine ((lat, _, _, _), (prevLat, _, _, _)) = if lat /= prevLat then "\n" else "" 
                ens = concatMap (\line@((lat, r, freq, en), _) -> show lat ++ " " ++ show r ++ " " ++ show en ++ "\n" ++ (blankLine line)) sortedModeWithPrev
                freqs = concatMap (\line@((lat, r, freq, en), _) -> show lat ++ " " ++ show r ++ " " ++ show freq ++ "\n" ++ (blankLine line)) sortedModeWithPrev
            Utils.IO.writeToFile ("ens" ++ show modeNo ++ ".csv") ens
            Utils.IO.writeToFile ("freqs" ++ show modeNo ++ ".csv") freqs
        ) allModes [1 ..]
        
        
        