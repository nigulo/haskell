
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
    getValue',
    getValue'_,
    op,
    op',
    op'',
    is2d,
    is3d,
    dim
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
    getValue_ xs g = F.getValue xs [] g 

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
getValues = 
    op (\xs g d -> F.getValue xs [] g d)

getValues_ :: (F.Fn d) => [[Double]] -> AnalyticData d -> [Double]
getValues_ xs = getValues xs (mkStdGen 1) 

getValue :: RandomGen g => (F.Fn d) => [Double] -> g -> AnalyticData d -> Double
getValue = op' (\xs g d -> F.getValue xs [] g d)

getValue_ :: (F.Fn d) => [Double] -> AnalyticData d -> Double
getValue_ x = getValue x (mkStdGen 1)

getValue' :: RandomGen g => (F.Fn d) => Double -> g -> AnalyticData d -> Double
getValue' = op'' (\xs g d -> F.getValue xs [] g d)

getValue'_ :: (F.Fn d) => Double -> AnalyticData d -> Double
getValue'_ x = getValue' x (mkStdGen 1)

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
