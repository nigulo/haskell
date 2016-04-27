module Main (main) where

import TSA.D2
import TSA.Params
import qualified Utils.Xml as Xml
import Regression.Data as D
import Utils.Concurrent

import System.Environment
import System.IO
import Statistics.Sample as Sample
import qualified Data.Vector.Unboxed as V
import Data.List

df = 0.1

main :: IO ()
main = do
    [periodStartS, periodEndS, corrStartS, corrEndS, methodS, precisionS, bootstrapCountS] <- getArgs
    let
        periodStart = read periodStartS 
        periodEnd = read periodEndS 
        corrLenStart = read corrStartS
        corrLenEnd = read corrEndS
        method = read methodS
        precision = read precisionS
        bootstrapCount = read bootstrapCountS
        freqStart = 1 / periodEnd
        freqEnd = 1 / periodStart
        
    dp <- Xml.parseFromFile "data" "data" >>= \doc -> return (Xml.fromDocument doc)
    handle <- openFile ("result") AppendMode
    let
        SD1 dat = subData (head (dataSet dp))
        taskEnv = defaultTaskEnv {logFunc = \str -> hPutStr handle (str ++ "\n")}
    bins <- phaseBins dat (getBinSize freqEnd)
    dispersions <- calcDispersions' bins freqStart freqEnd corrLenStart corrLenEnd method precision (dataName dp ++ "_d2") True df taskEnv

    -- Bootstrap stuff
    let
        corrLens =
            if corrLenStart == corrLenEnd
                then 
                    [corrLenStart]
                else
                    [corrLenStart, corrLenStart + (corrLenEnd - corrLenStart) / fromIntegral precision .. corrLenEnd]
        freqStep = (freqEnd - freqStart) / (fromIntegral precision)
        freqs =
            if periodStart == periodEnd
                then 
                    [freqStart]
                else
                    [freqStart + fromIntegral i * freqStep | i <- [0 .. precision]]
    minDisps <- calcConcurrently (\i _ -> do
            disps <- mapM (\freq ->  
                    mapM (\corrLen -> do 
                            bsBins <- bootstrapBins method bins corrLen freq df
                            dispDat <- calcDispersions' bsBins freq freq corrLen corrLen method 1 (dataName dp ++ "_d2" ++ (show i)) True df taskEnv
                            --putStrLn $ show freq ++ " " ++ show (V.head (D.ys dispDat))
                            return (corrLen, freq, V.head (D.ys dispDat))
                        ) corrLens 
                ) freqs
            return $ minimumBy (\(_, _, disp1) (_, _, disp2) -> compare disp1 disp2) $ concat disps
        ) (progressUpdateFunc taskEnv) (taskInitializer taskEnv) (taskFinalizer taskEnv) [1 .. bootstrapCount]
    if minDisps == []
        then return ()
        else do
            let
                (freqMean, freqVar) = Sample.meanVarianceUnb $ V.fromList $ map (\(_, freq, disp) -> freq) minDisps
            
            putStrLn ("Minimum: " ++ show freqMean ++ ", " ++ show freqVar)
    hClose handle
    
    