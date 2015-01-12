module Math.Statistics (
    mean, stdev, stdev_, cov, cov_, corr, 
    meanw, stdevw, stdevw_, covw, covw_, corrw,
    var, var_, varw, varw_,
    cumulProbDist, cumulProbDist_, 
    normalCDF, normalKSTest,
    twoSampleKSTest
    ) where

import Data.List
import Statistics.Distribution.Normal
import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants
import Utils.Misc
import qualified Data.Vector.Unboxed as V

-- | Calculates the mean of the data set
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- | Calculates the mean of the data set with weights
meanw :: [Double] -> [Double] -> Double -> Double
meanw xs ws wSum = (foldl (\z (x, w) -> z + w * x) 0 (zip xs ws)) / wSum

-- | Calculates the mean of the data set with weights
meanw_ :: [Double] -> [Double] -> Double
meanw_ xs ws = meanw xs ws (sum ws)

-- | Calculates the covariance of two data sets
--   Here we assume that both data sets have the same length
cov :: [Double] -> [Double] -> Double -> Double -> Double
cov xs ys xMean yMean = 
    let
        n = fromIntegral (length xs)
    in
        (foldl (\z (x, y) -> z + x * y) 0 (zip xs ys) - n * xMean * yMean) / (n - 1)

cov_ :: [Double] -> [Double] -> Double
cov_ xs ys = cov xs ys (mean xs) (mean ys) 

-- | Calculates the covariance of two data sets with weights
--   Here we assume that both data sets have the same length
covw :: [Double] -> [Double] -> [Double] -> Double -> Double -> Double -> Double
covw xs ys ws xMean yMean wSum = (foldl (\z (x, y, w) -> z + w * (x - xMean) * (y - yMean)) 0 (zip3 xs ys ws)) / wSum

covw_ :: [Double] -> [Double] -> [Double] -> Double
covw_ xs ys ws =
    let
        wSum = sum ws
    in
        covw xs ys ws (meanw xs ws wSum) (meanw ys ws wSum) wSum

-- | Calculates the variance of the data set
var :: [Double] -> Double -> Double
var xs xMean = cov xs xs xMean xMean

var_ :: [Double] -> Double
var_ xs = var xs (mean xs) 

-- | Calculates the variance of the data set with weights
varw :: [Double] -> [Double] -> Double -> Double -> Double
varw xs ws xMean wSum = covw xs xs ws xMean xMean wSum

varw_ :: [Double] -> [Double] -> Double
varw_ xs ws = varw xs ws (mean xs) (sum ws) 

-- | Calculates the standard deviation of the data set
stdev :: [Double] -> Double -> Double
stdev xs xMean = sqrt (var xs xMean)

stdev_ :: [Double] -> Double
stdev_ xs = stdev xs (mean xs) 

-- | Calculates the standard deviation of the data set with weights
stdevw :: [Double] -> [Double] -> Double -> Double -> Double
stdevw xs ws xMean wSum = sqrt (varw xs ws xMean wSum)

stdevw_ :: [Double] -> [Double] -> Double
stdevw_ xs ws = stdevw xs ws (mean xs) (sum ws) 

-- | Calculates the Pearson product-moment correlation coefficient of two data sets
corr :: [Double] -> [Double] -> Double
corr  xs ys = 
    let
        xMean = mean xs
        yMean = mean ys
    in
        (cov xs ys xMean yMean) / (stdev xs xMean) / (stdev ys yMean)

-- | Calculates the Pearson product-moment correlation coefficient of two data sets with weights
corrw :: [Double] -> [Double] -> [Double] -> Double
corrw  xs ys ws = 
    let
        wSum = sum ws
        xMean = meanw xs ws wSum
        yMean = meanw ys ws wSum
    in
        (covw xs ys ws xMean yMean wSum) / (stdevw xs ws xMean wSum) / (stdevw ys ws yMean wSum)

-- | Calculates the cumulative probability distribution of xs using coordinate samples
cumulProbDist :: V.Vector Double -> V.Vector Double -> V.Vector (Double {-x-}, Double {-cumulative probability-})
cumulProbDist xSample xs = 
    (V.map (\x -> (x, fromIntegral (V.length (V.filter (< x) xs)) / (fromIntegral n))) (sortVector $ nubVector xs))  where
        n = V.length xs

cumulProbDist_ :: V.Vector Double -> V.Vector (Double {-x-}, Double {-cumulative probability-})
cumulProbDist_ xs = cumulProbDist (sortVector $ nubVector xs) xs

normalCDF :: Double -> Double -> Double -> Double
normalCDF mean stdev x = erfc ((mean - x) / (m_sqrt_2 * stdev)) / 2

normalKSTest :: Double -> Double -> V.Vector Double -> Double
normalKSTest mean stdev xs =
    let
        xSample = sortVector $ nubVector xs
        normDist = V.map (normalCDF mean stdev) xSample
        dist = snd $ V.unzip $ cumulProbDist xSample xs
    in
        V.maximum $ V.zipWith (\p1 p2 -> abs (p1 - p2)) normDist dist

-- | Two-sample Kolmogorov-Smirnov test
twoSampleKSTest :: V.Vector Double -> V.Vector Double -> Double
twoSampleKSTest sample1 sample2 =
    let 
        xMin = max (V.minimum sample1) (V.minimum sample2)
        xMax = min (V.maximum sample1) (V.maximum sample2)
        xRange = xMax - xMin
        xStep = xRange / (fromIntegral (min (V.length sample1) (V.length sample2)))
        xSample = V.generate (round (xRange / xStep)) (\i -> xMin + (fromIntegral i) * xStep)
        dist1 = snd $ V.unzip $ cumulProbDist xSample (V.filter (\x -> x >= xMin && x <= xMax) sample1)
        dist2 = snd $ V.unzip $ cumulProbDist xSample (V.filter (\x -> x >= xMin && x <= xMax) sample2)
    in
        V.maximum $ V.zipWith (\p1 p2 -> abs (p1 - p2)) dist1 dist2