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

calcConcurrently' :: Int -> (a -> (Double -> IO ()) -> IO b) -> (Double -> IO ()) -> [a] -> IO [b]
calcConcurrently' numCapabilities f puFunc vals = do
    let
        numValsPerThread = length vals `div` numCapabilities
    putStrLn $ "-------" ++ show numValsPerThread ++ ", " ++ show numCapabilities
    sem <- SSem.new (-numCapabilities + 1)
    resultsRef <- newMVar []
    pctRef <- newMVar 0
    foldM_ (\vals i -> do
            let
                (initVals, tailVals) = if i < numCapabilities
                    then
                        splitAt numValsPerThread vals
                    else (vals, [])
            _ <- forkIO $ do
                results <- mapM (\val -> do
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
                        return result
                    ) initVals
                modifyMVar_ resultsRef (\allResults -> return (allResults ++ [(i, results)]))
                SSem.signal sem
            return tailVals
        ) vals [1 .. numCapabilities]
    SSem.wait sem
    results <- readMVar resultsRef
    return $ concatMap snd $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) results

calcConcurrently :: (a -> (Double -> IO ()) -> IO b) -> (Double -> IO ()) -> [a] -> IO [b]
calcConcurrently f puFunc vals = getNumCapabilities >>= \numCapabilities -> calcConcurrently' numCapabilities f puFunc vals  

calcConcurrently_ :: (a -> IO b) -> [a] -> IO [b]
calcConcurrently_ f = calcConcurrently (\x _ -> f x) (\_ -> return ())

calcConcurrently__ :: (a -> b) -> [a] -> IO [b]
calcConcurrently__ f = calcConcurrently_ (\x -> return (f x))
