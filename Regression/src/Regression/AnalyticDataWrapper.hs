
module Regression.AnalyticDataWrapper (
    AnalyticDataWrapper,
    analyticDataWrapper,
    xMins,
    xMaxs,
    xMin1,
    xMax1,
    is2d,
    is3d,
    dim,
    getValues,
    getValues_,
    getValue,
    getValue_,
    getValue',
    getValue'_,
    getMinima,
    getMaxima,
    getExtrema,
    getZeroCrossings
   ) where

import qualified Regression.AnalyticData as AD
import qualified Math.Function as F
import Utils.List
import Utils.Misc
import Data.List
import System.Random
import Debug.Trace
import qualified Data.Vector.Unboxed as V

data AnalyticDataWrapper = forall d . F.Fn d => AnalyticDataWrapper (AD.AnalyticData d)

instance F.Fn AnalyticDataWrapper where
    getValue x f g (AnalyticDataWrapper ad) = F.getValue x f g ad 
    getValue_ xs g (AnalyticDataWrapper ad) = F.getValue_ xs g ad 

    constantOp op (AnalyticDataWrapper ad) k = analyticDataWrapper $ F.constantOp op ad k
    --binaryOp op (AnalyticDataWrapper (ad1 :: AD.AnalyticData d)) (AnalyticDataWrapper (ad2 :: AD.AnalyticData d2)) = analyticDataWrapper $ F.binaryOp op ad1 ad2  
    --binaryOp op (AnalyticDataWrapper ad1@(AD.AnalyticData (F.Function _))) (AnalyticDataWrapper ad2@(AD.AnalyticData (F.Function _))) = analyticDataWrapper $ F.binaryOp op ad1 ad2  

    
    
analyticDataWrapper :: forall d . (F.Fn d) => AD.AnalyticData d -> AnalyticDataWrapper
analyticDataWrapper = AnalyticDataWrapper

xMins :: AnalyticDataWrapper -> [Double]
xMins (AnalyticDataWrapper ad) = AD.xMins ad

-- | Returns maximum x-value for which the analytic data is defined
xMaxs :: AnalyticDataWrapper -> [Double]
xMaxs (AnalyticDataWrapper ad) = AD.xMaxs ad

xMin1 :: AnalyticDataWrapper -> Double
xMin1 = head.xMins

xMax1 :: AnalyticDataWrapper -> Double
xMax1 = head.xMaxs

is2d :: AnalyticDataWrapper -> Bool
is2d ad = 
    case xMins ad of
        _:[] -> True
        otherwise -> False

is3d :: AnalyticDataWrapper -> Bool
is3d ad = 
    case xMins ad of
        _:_:[] -> True
        otherwise -> False

-- | return number of independent ordinal values this data set is defined on
dim :: AnalyticDataWrapper -> Int
dim ad = length $ xMins ad

-- | Returns y-values for given array of x-values
getValues :: RandomGen g => [[Double]] -> g -> AnalyticDataWrapper -> [Double]
getValues xs g (AnalyticDataWrapper ad) = AD.getValues xs g ad  

getValues_ :: [[Double]] -> AnalyticDataWrapper -> [Double]
getValues_ xs = getValues xs (mkStdGen 1) 

getValue :: RandomGen g => [Double] -> g -> AnalyticDataWrapper -> Double
getValue x g (AnalyticDataWrapper ad) = AD.getValue x g ad

getValue_ :: [Double] -> AnalyticDataWrapper -> Double
getValue_ x d = head $ getValues_ [x] d

getValue' :: RandomGen g => Double -> g -> AnalyticDataWrapper -> Double
getValue' x g (AnalyticDataWrapper ad) = AD.getValue' x g ad

getValue'_ :: Double -> AnalyticDataWrapper -> Double
getValue'_ x = getValue' x (mkStdGen 1)

getMinima :: (RandomGen g) => Int -> Maybe Double -> g -> AnalyticDataWrapper -> V.Vector (Double, Double)
getMinima samplingCount maybePeriod g d = fst $ getExtrema samplingCount maybePeriod g d

getMaxima :: (RandomGen g) => Int -> Maybe Double -> g -> AnalyticDataWrapper -> V.Vector (Double, Double)
getMaxima samplingCount maybePeriod g d = snd $ getExtrema samplingCount maybePeriod g d

-- | Returns an array containing minima and maxima of given data set (currently supported for 2d data only)
getExtrema :: (RandomGen g) => Int -> Maybe Double -> g -> AnalyticDataWrapper -> (V.Vector ((Double, Double)) {-minima-}, V.Vector (Double, Double) {-maxima-})
getExtrema samplingCount maybePeriod g d =
    let
        xLeft = xMin1 d
        xRight = xMax1 d
        step = (xRight - xLeft) / (fromIntegral samplingCount)
        stepAfterExtrema = case maybePeriod of
            Just period -> period / 2.1
            Nothing -> step

        getExtrema' :: (Double, Double) -> (Double, Double) -> (V.Vector (Double, Double), V.Vector (Double, Double))
        getExtrema' (x0, y0) (x1, y1) =
            if (x1 + step > xRight) then (V.empty, V.empty)
            else
              let
                    x2 = x1 + step
                    y2 = getValue' x2 g d
                    (minima, maxima) = getExtrema' (x1, y1) (x2, y2)
                    x1' = x0 + stepAfterExtrema
                    y1' = getValue' x1' g d
                    x2' = x1' + step
                    y2' = getValue' x2' g d
                    (minima', maxima') = getExtrema' (x1', y1') (x2', y2')
                in
                if (y1 - y0) < 0 && (y1 - y2) < 0 
                        then
                            (V.cons (x1, y1) minima', maxima')
                    else if (y1 - y0) > 0 && (y1 - y2) > 0 
                        then
                            (minima', V.cons (x1, y1) maxima')
                    else (minima, maxima)
        x0 = xLeft
        y0 = getValue' x0 g d
        x1 = xLeft + step
        y1 = getValue' x1 g d
    in  
        getExtrema' (x0, y0) (x1, y1) 

-- | Returns an array containing the abscissa of zero-crossings
getZeroCrossings :: (RandomGen g) => Int -> g -> AnalyticDataWrapper -> V.Vector Double
getZeroCrossings samplingCount g d =
    let
        xLeft = xMin1 d
        xRight = xMax1 d
        step = (xRight - xLeft) / (fromIntegral samplingCount)

        getZeroCrossings' :: (Double, Double) -> Int -> V.Vector Double
        getZeroCrossings' (x1, y1) i =
            let
                x2 = xLeft + fromIntegral i * step
            in
                if (x2 > xRight) then V.empty
                else
                  let
                        y2 = getValue' x2 g d
                        zeroCrossings = getZeroCrossings' (x2, y2) (i + 1)
                    in
                        if y1 == 0 then V.cons x1 zeroCrossings
                        else if signum y1 /= signum y2 then V.cons (if abs y1 < abs y2 then x1 else x2) zeroCrossings
                        else zeroCrossings
        x1 = xLeft
        y1 = getValue' x1 g d
    in  
        getZeroCrossings' (x1, y1) 1
