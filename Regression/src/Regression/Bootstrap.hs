module Regression.Bootstrap (
        bootstrapSplines, 
        upperLowerSplines,
        bootstrapSpline
        ) where

import Debug.Trace

import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import Math.Function as F

import Utils.Misc
import System.Random
import Control.Concurrent.SSem as SSem
import Control.Concurrent
import Control.Concurrent.MVar
import System.CPUTime
import System.Random.MWC
import qualified Utils.Xml as Xml


bootstrapSplines :: Int -> (Data -> (Double -> IO ()) -> IO Spline) -> Spline -> Data -> (Double -> IO ()) -> IO [Spline]
bootstrapSplines bootstrapCount fitFunc spline dat progressUpdateFunc = do
    time <- getCPUTime
    numCapabilities <- getNumCapabilities
    sem <- SSem.new (-bootstrapCount + 1)
    splinesRef <- newMVar []
    stdGen <- getStdGen
    let 
        Left diff = U.binaryOp F.subtr (Left dat) (Right (Left spline)) True stdGen
    mapM_ (\i -> forkOn (i `mod` numCapabilities) $ 
        do
            bsData <- U.bootstrap (Left spline) dat diff
            bsSpline <- fitFunc bsData (\percent -> progressUpdateFunc (percent * fromIntegral i / fromIntegral bootstrapCount))
            modifyMVar_ splinesRef (\splines -> return (splines ++ [bsSpline]))
            SSem.signal sem
        ) [1 .. bootstrapCount]
    SSem.wait sem
    splines <- readMVar splinesRef
    time <- getCPUTime >>= \t -> return $ t - time
    putStrLn $ "Bootstrap time:"  ++ show (fromIntegral time / 1e12) ++ "s"
    return splines

bootstrapSpline :: Int -> (Data -> (Double -> IO ()) -> IO Spline) -> Spline -> Data -> Data -> IO (Spline)
bootstrapSpline i fitFunc spline dat diff = do
    bsData <- U.bootstrap (Left spline) dat diff
    bsSpline <- fitFunc bsData (\_ -> return ())
    return bsSpline    

-- | Returns upper and lower splines representing standard deviation of the given bootstrap set 
upperLowerSplines :: Spline -> [Spline] -> (Spline {-lower-}, Spline {-upper-})
upperLowerSplines spline bsSplines =
    let 
        func (upperSpline, lowerSpline) bsSpline =
            let
                splineDiff = S.subtr spline bsSpline
                splineSquareDiff = S.splineProduct splineDiff splineDiff
                splineSquareDiffDivCount = S.divide splineSquareDiff (fromIntegral (length bsSplines))
                splineSquareDiffDivCountNeg = S.mult splineSquareDiffDivCount (-1)
            in
                (S.splineSum upperSpline splineSquareDiffDivCount, S.splineSum lowerSpline splineSquareDiffDivCountNeg)
    in
        foldl func (spline, spline) bsSplines

