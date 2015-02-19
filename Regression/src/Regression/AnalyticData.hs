
module Regression.AnalyticData (
    AnalyticData (..),
    xMins,
    xMaxs,
    xMin1,
    xMax1,
    getValues,
    getValues_,
    getValue,
    getValue_,
    op,
    is2d,
    is3d,
    dim,
    getMinima,
    getMaxima,
    getExtrema
    ) where

import qualified Math.Expression as E
import qualified Math.Function as F
import Utils.List
import Utils.Misc
import Data.List
import System.Random
import Debug.Trace
import Data.Either
import Data.Maybe
import qualified Data.Vector.Unboxed as V

newtype (F.Fn d) => AnalyticData d = AnalyticData [([Double] {--xMins--}, [Double] {--xMaxs--}, d)] deriving (Show, Read, Eq)

instance (F.Fn d) => F.Fn (AnalyticData d) where
    getValue x _ g ad = head $ op (\x g d -> F.getValue x [] g d) [x] g ad
    getValue_ xs = F.getValue xs [] (mkStdGen 1) 

    constantOp op (AnalyticData ds) k = 
        let (xMins, xMaxs, fs) = unzip3 ds
        in AnalyticData (zip3 xMins xMaxs (map (\f -> F.constantOp op f k) fs))

    binaryOp op (AnalyticData s1) (AnalyticData s2) = 
        let 
            fOp :: (F.Fn f) => ([Double], [Double], f) -> ([Double], [Double], f) -> F.Function Double -> Maybe ([Double], [Double], f)
            fOp (xMins1, xMaxs1, f1) (xMins2, xMaxs2, f2) op =
                if all (\(xMin1, xMax1, xMin2, xMax2)  -> (xMin1 <= xMax2 && xMax1 >= xMin2) || (xMin2 <= xMax1 && xMax2 >= xMin1)) (zip4 xMins1 xMaxs1 xMins2 xMaxs2) 
                    then Just (map (\(x1, x2) -> max x1 x2) (zip xMins1 xMins2), map (\(x1, x2) -> min x1 x2) (zip xMaxs1 xMaxs2), F.binaryOp op f1 f2)
                    else Nothing
            newFs = [fOp (s1 !! i) (s2 !! j) op | i <- [0 .. length s1 - 1], j <- [0 .. length s2 - 1]]
        in AnalyticData $ catMaybes newFs

-- | Returns minimum x-value for which the analytic data is defined
xMins :: (F.Fn d) => AnalyticData d -> [Double]
xMins (AnalyticData ds) = 
    let 
        (xs, _, _) = unzip3 ds
    in
        foldl1 (\xs1 xs2 -> map (\(x1, x2) -> min x1 x2) (zip xs1 xs2)) xs
    --in (minimum xs)

-- | Returns maximum x-value for which the analytic data is defined
xMaxs :: (F.Fn d) => AnalyticData d -> [Double]
xMaxs (AnalyticData ds) =
    let 
        (_, xs, _) = unzip3 ds
    in
        foldl1 (\xs1 xs2 -> map (\(x1, x2) -> max x1 x2) (zip xs1 xs2)) xs
    --in (maximum xs)

xMin1 :: (F.Fn d) => AnalyticData d -> Double
xMin1 = head.xMins

xMax1 :: (F.Fn d) => AnalyticData d -> Double
xMax1 = head.xMaxs

-- | Returns y-values for given array of x-values
getValues :: RandomGen g => (F.Fn d) => [[Double]] -> g -> AnalyticData d -> [Double]
getValues xs g d = 
    op (\xs g d -> F.getValue xs [] g d) xs g d

getValues_ :: (F.Fn d) => [[Double]] -> AnalyticData d -> [Double]
getValues_ xs = getValues xs (mkStdGen 1) 

getValue :: RandomGen g => (F.Fn d) => [Double] -> g -> AnalyticData d -> Double
getValue x g d = op' (\xs g d -> F.getValue xs [] g d) x g d

getValue_ :: (F.Fn d) => [Double] -> AnalyticData d -> Double
getValue_ x d = getValue x (mkStdGen 1) d

getValue' :: RandomGen g => (F.Fn d) => Double -> g -> AnalyticData d -> Double
getValue' x g d = op'' (\xs g d -> F.getValue xs [] g d) x g d

getValue'_ :: (F.Fn d) => Double -> AnalyticData d -> Double
getValue'_ x d = getValue' x (mkStdGen 1) d

-- | Calculates the value of the analytic data at the given coordinate
op :: (F.Fn d, RandomGen g) => ([Double] -> g -> d -> Double) -> [[Double]] -> g -> AnalyticData d -> [Double]
op f xs g (AnalyticData ds) = 
    let
    
        dist :: (F.Fn ad) => [Double] -> ([Double], [Double], ad) -> (ad, Double)
        dist xs (xMins, xMaxs, ad) = (ad, sqrt $ foldl (\d x -> d + x * x) 0 $ 
            zipWith3 (\x xMin xMax -> if x >= xMin && x <= xMax then 0 else min (abs (x - xMax)) (abs (x - xMin))) xs xMins xMaxs)
    
        dists :: (F.Fn ad) => [Double] -> [([Double], [Double], ad)] -> [(ad, Double)]
        dists xs = map (dist xs) 
    
        findAD xs = fst $ minimumBy (\(d1, dist1) (d2, dist2) -> if dist1 <= dist2 then LT else GT) $ dists xs ds 
    in
        zipWith (\x g -> f x g (findAD x)) xs  (randomGens g)

-- | Calculates the value of the analytic data at the given coordinate
op' :: (F.Fn d, RandomGen g) => ([Double] -> g -> d -> Double) -> [Double] -> g -> AnalyticData d -> Double
op' f x g (AnalyticData ds) = 
    let
    
        dist :: (F.Fn ad) => [Double] -> ([Double], [Double], ad) -> (ad, Double)
        dist xs (xMins, xMaxs, ad) = (ad, sqrt $ foldl (\d x -> d + x * x) 0 $ 
            zipWith3 (\x xMin xMax -> if x >= xMin && x <= xMax then 0 else min (abs (x - xMax)) (abs (x - xMin))) xs xMins xMaxs)
    
        dists :: (F.Fn ad) => [Double] -> [([Double], [Double], ad)] -> [(ad, Double)]
        dists xs = map (dist xs) 
    
        findAD xs = fst $ minimumBy (\(d1, dist1) (d2, dist2) -> if dist1 <= dist2 then LT else GT) $ dists xs ds 
    in
        f x g (findAD x)

-- | Calculates the value of the analytic data at the given coordinate
op'' :: (F.Fn d, RandomGen g) => ([Double] -> g -> d -> Double) -> Double -> g -> AnalyticData d -> Double
op'' f x g (AnalyticData ds) = 
    let
    
        dist :: (F.Fn ad) => Double -> ([Double], [Double], ad) -> (ad, Double)
        dist x ((xMin:_), (xMax:_), ad) = (ad, 
            if x >= xMin then
                if x <= xMax then 0 
                else x - xMax
            else xMin - x
            )
        dists :: (F.Fn ad) => Double -> [([Double], [Double], ad)] -> [(ad, Double)]
        dists xs = map (dist xs) 
    
        findAD x = fst $ minimumBy (\(d1, dist1) (d2, dist2) -> if dist1 <= dist2 then LT else GT) $ dists x ds 
    in
        f [x] g (findAD x)

is2d :: (F.Fn d) => AnalyticData d -> Bool
is2d (AnalyticData (((_:[]), _, _):_)) = True
is2d _ = False

is3d :: (F.Fn d) => AnalyticData d -> Bool
is3d (AnalyticData (((_:_:[]), _, _):_)) = True
is3d _ = False

-- | return number of independent ordinal values this data set is defined on
dim :: (F.Fn d) => AnalyticData d -> Int
dim (AnalyticData ((xs, _, _):_)) = length xs

getMinima :: (F.Fn d, RandomGen g) => Int -> Maybe Double -> g -> AnalyticData d -> V.Vector (Double, Double)
getMinima samplingCount maybePeriod g d = fst $ getExtrema samplingCount maybePeriod g d

getMaxima :: (F.Fn d, RandomGen g) => Int -> Maybe Double -> g -> AnalyticData d -> V.Vector (Double, Double)
getMaxima samplingCount maybePeriod g d = snd $ getExtrema samplingCount maybePeriod g d

-- | Returns an array containing minima and maxima of given data set (currently supported for 2d data only)
getExtrema :: (F.Fn d, RandomGen g) => Int -> Maybe Double -> g -> AnalyticData d -> (V.Vector ((Double, Double)) {-minima-}, V.Vector (Double, Double) {-maxima-})
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
            