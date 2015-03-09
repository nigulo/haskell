module Utils.Concurrent (
    calcConcurrently, 
    calcConcurrently_, 
    calcConcurrently__,
    calcConcurrently') where

import Control.Concurrent.MVar

import Control.Concurrent.SSem as SSem
import Control.Concurrent
import Control.Monad
import Data.List
import System.Random

calcConcurrently' :: Int -- ^ Number of CPU cores 
    -> (a -> (Double -> IO ()) -> IO b) -- ^ Function to run concurrently 
    -> (Double -> IO ()) -- ^ Progress update function 
    -> (ThreadId -> IO ()) -- ^ Function run once per every thread launched
    -> IO () -- ^ Function run once per every thread when thead is about to finish
    -> [a] -- ^ List of initial values
    -> IO [b] -- ^ List of final values
calcConcurrently' numCapabilities f puFunc initializer finalizer vals = do
    let
        valCount = length vals
        numValsPerThread = valCount `div` numCapabilities
    sem <- SSem.new (-numCapabilities + 1)
    resultsRef <- newMVar []
    pctRef <- newMVar (replicate valCount 0)
    foldM_ (\vals i -> do
            let
                (initVals, tailVals) = if i < numCapabilities - 1
                    then
                        splitAt numValsPerThread vals
                    else (vals, [])
            threadSem <- SSem.new 1
            threadId <- forkFinally (
                do
                    SSem.wait threadSem
                    results <- zipWithM (\val j -> do
                            result <- f val (\pct ->
                                    modifyMVar_ pctRef $ \oldPcts -> do
                                        let
                                            pctIndex = i * numValsPerThread + j
                                            newPcts = take pctIndex oldPcts ++ [pct] ++ drop (pctIndex + 1) oldPcts
                                        puFunc $ sum newPcts / fromIntegral valCount
                                        return newPcts
                                )
                            return result
                        ) initVals [0 ..]
                    modifyMVar_ resultsRef (\allResults -> return (allResults ++ [(i, results)]))
                    SSem.signal sem
                ) (\_ -> finalizer)
            initializer threadId
            SSem.signal threadSem
            return tailVals
        ) vals [0 .. numCapabilities - 1]
    SSem.wait sem
    results <- readMVar resultsRef
    return $ concatMap snd $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) results

calcConcurrently :: (a -> (Double -> IO ()) -> IO b) 
    -> (Double -> IO ()) 
    -> (ThreadId -> IO ())
    -> IO ()
    -> [a] 
    -> IO [b]
calcConcurrently f puFunc initializer finalizer vals = 
    getNumCapabilities >>= \numCapabilities -> calcConcurrently' numCapabilities f puFunc initializer finalizer vals  

calcConcurrently_ :: (a -> IO b) -> [a] -> IO [b]
calcConcurrently_ f = calcConcurrently (\x _ -> f x) (\_ -> return ()) (\_ -> return ()) (return ())

calcConcurrently__ :: (a -> b) -> [a] -> IO [b]
calcConcurrently__ f = calcConcurrently_ (\x -> return (f x))
