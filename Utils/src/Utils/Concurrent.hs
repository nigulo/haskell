module Utils.Concurrent (calcConcurrently, calcConcurrently_, calcConcurrently__) where

import Control.Concurrent.MVar

import Control.Concurrent.SSem as SSem
import Control.Concurrent
import Data.List

calcConcurrently :: (a -> (Double -> IO ()) -> IO b) -> (Double -> IO ()) -> [a] -> IO [b]
calcConcurrently f puFunc vals = do
    numCapabilities <- getNumCapabilities
    let
        numVals = length vals
    sem <- SSem.new (-numVals + 1)
    resultsRef <- newMVar []
    pctRef <- newMVar 0
    mapM_ (\(val, i) -> forkOn (i `mod` numCapabilities) $ 
        do
            result <- f val (\pct ->
                    modifyMVar_ pctRef (\oldPct -> do
                        if pct > oldPct 
                            then 
                                do
                                    puFunc pct
                                    return pct
                            else
                                return oldPct
                        )
                )
            modifyMVar_ resultsRef (\results -> return (results ++ [(i, result)]))
            SSem.signal sem
        ) (zip vals [0 .. (length vals) - 1])
    SSem.wait sem
    results <- readMVar resultsRef
    return $ map snd $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) results

calcConcurrently_ :: (a -> IO b) -> [a] -> IO [b]
calcConcurrently_ f vals = do
    numCapabilities <- getNumCapabilities
    let
        numVals = length vals
    sem <- SSem.new (-numVals + 1)
    resultsRef <- newMVar []
    mapM_ (\(val, i) -> forkOn (i `mod` numCapabilities) $ 
        do
            result <- f val
            modifyMVar_ resultsRef (\results -> return (results ++ [(i, result)]))
            SSem.signal sem
        ) (zip vals [0 .. (length vals) - 1])
    SSem.wait sem
    results <- readMVar resultsRef
    return $ map snd $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) results

calcConcurrently__ :: (a -> b) -> [a] -> IO [b]
calcConcurrently__ f vals = do
    numCapabilities <- getNumCapabilities
    let
        numVals = length vals
    sem <- SSem.new (-numVals + 1)
    resultsRef <- newMVar []
    mapM_ (\(val, i) -> forkOn (i `mod` numCapabilities) $ 
        do
            let
                result = f val
            modifyMVar_ resultsRef (\results -> return (results ++ [(i, result)]))
            SSem.signal sem
        ) (zip vals [0 .. (length vals) - 1])
    SSem.wait sem
    results <- readMVar resultsRef
    return $ map snd $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) results

