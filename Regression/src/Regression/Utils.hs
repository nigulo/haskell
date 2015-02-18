
module Regression.Utils (
    binaryOp,
    constantOp,
    sampleAnalyticData,
    sampleAnalyticData_,
    sample2dAnalyticData,
    getValues,
    bootstrap,
    var,
    stdev,
    format,
    dataRange,
    reshuffleData    
    ) where

import Regression.Data as D
import qualified Regression.Polynom as P
import qualified Regression.Spline as S
import qualified Regression.Functions as FS
import qualified Regression.AnalyticData as AD
import qualified Math.Function as F
import qualified Math.Expression as E

import Data.List
import qualified Data.Vector.Unboxed as V
import System.Random
import System.Random.MWC
import Utils.Misc
import Debug.Trace
import qualified Statistics.Sample as Sample

import System.Random
import System.Random.MWC

binaryOp :: RandomGen g => F.Function Double -> Either Data (Either S.Spline FS.Functions) -> Either Data (Either S.Spline FS.Functions) -> Bool -> g -> Either Data (Either S.Spline FS.Functions)
binaryOp op (Left d1) (Left d2) yOrx _ = 
    let 
        xs2 = xs1 d2
        xs21 = V.filter (\x -> x >= V.minimum xs2 && x <= V.maximum xs2) (xs1 d1)
        d1' = sampleData1 xs21 d1
        d2' = interpolatedData1 xs21 d2
        vs = 
            if yOrx
            then
                V.zipWith (\(x1, y1, w1) (_, y2, w2) -> (x1, F.getValue_ [y1, y2] op, w1 + w2)) (values1 d1') (values1 d2')
            else
                V.zipWith (\(x1, y1, w1) (x2, _, w2) -> (F.getValue_ [x1, x2] op, y1, w1)) (values1 d1') (values1 d2')
    in
        case d1 of 
            Data _ -> Left $ data1 vs
            Spectrum _ -> Left $ spectrum1 vs
            Data2 _ -> Left $ data1 vs
            Spectrum2 _ -> Left $ spectrum1 vs
binaryOp op (Left d) (Right s) yOrx g = 
    let 
        (g1, g2) = System.Random.split g
        ys =
            case s of
                Left spline -> AD.getValues (xs d) (mkStdGen 1) spline
                Right fs -> AD.getValues (xs d) g1 fs
            
        vs = 
            if yOrx
            then zipWith (\(x1, y1, w1) (y, g) -> (x1, F.getValue [y1, y] [] g op, w1)) (V.toList (values1 d)) (zip ys (randomGens g2))
            else zipWith (\(x1, y1, w1) (x, g) -> (F.getValue [x1, x] [] g op, y1, w1)) (V.toList (values1 d)) (zip (V.toList (xs1 d)) (randomGens g2))
    in
        case d of 
            Data _ -> Left $ data1 $ V.fromList vs
            Spectrum _ -> Left $ spectrum1 $ V.fromList vs
            Data2 _ -> Left $ data1 $ V.fromList vs
            Spectrum2 _ -> Left $ spectrum1 $ V.fromList vs
binaryOp op (Right (Left s1)) (Right (Left s2)) _ g = 
    if op == F.add 
        then Right (Left (S.splineSum s1 s2)) 
        else if op == F.subtr 
            then Right (Left (S.splineDiff s1 s2))
        else Right (Left (AD.AnalyticData [([0], [0], (P.unitPolynom 0))]))

constantOp :: F.Function Double -> Either Data (Either S.Spline FS.Functions) -> Double -> Bool -> Either Data (Either S.Spline FS.Functions)
constantOp op (Left d) k yOrx = 
    let 
        vs = 
            if yOrx
            then V.map (\(x, y, w) -> (x, F.getValue_ [y, k] op, w)) (values1 d)
            else V.map (\(x, y, w) -> (F.getValue_ [x, k] op, y, w)) (values1 d)
    in
        case d of 
            Data _ -> Left $ data1 vs
            Spectrum _ -> Left $ spectrum1 vs
            Data2 _ -> Left $ data1 vs
            Spectrum2 _ -> Left $ spectrum1 vs
constantOp op (Right s) k _ = 
    case s of
        Left spline -> Right $ Left $ F.constantOp op spline k
        Right fs -> Right $ Right $ F.constantOp op fs k
    


-- | Converts a given analytic data to data of specified number of samples
sampleAnalyticData :: (F.Fn d, RandomGen g) => AD.AnalyticData d -> [Double] -> [Double] -> [Int] -> g -> Data

-- 2d analytic data sampling
sampleAnalyticData s@(AD.AnalyticData (((_:[]), _, _):_)) [minx] [maxx] [num] g =
    let 
        step = if num <= 1 then 0
                else (maxx - minx) / (fromIntegral num - 1)
    in 
        Spectrum2 ((minx, step),
            V.zip (V.fromList (AD.getValues (map (\x ->  [x]) [minx, minx + step .. maxx]) g s)) (V.replicate num 1))

-- multidimensional analytic data sampling
sampleAnalyticData s minxs maxxs nums g =
    let 
        xs =
            zipWith3 (\minx maxx num ->  
                let step = if num <= 1 then maxx - minx else (maxx - minx) / (fromIntegral num - 1)
                in [minx, minx + step .. maxx]
            ) minxs maxxs nums
        
        xss = sequence xs
        
    in 
        Data (zip3 xss (AD.getValues xss g s) (replicate (length xss) 1))

sampleAnalyticData_ :: (F.Fn d, RandomGen g) => AD.AnalyticData d -> [Int] -> g -> Data
sampleAnalyticData_ s nums g = sampleAnalyticData s (AD.xMins s) (AD.xMaxs s) nums g  
 
-- more detailed 2d analytic data sampling
sample2dAnalyticData :: (F.Fn d, RandomGen g) => 
    AD.AnalyticData d -- ^ Data to sample 
    -> (Int, Double, Int) -- ^ Number of samples within one piece, length of the piece, number of pieces
    -> g -> Data
sample2dAnalyticData s (num, size, count) g = 
    let 
        minx = size * fromIntegral (floor (AD.xMin1 s / size))
        maxx = size * fromIntegral (ceiling (AD.xMax1 s / size))
        shift = size * (max 1 (fromIntegral (floor ((maxx - minx) / (fromIntegral count) / size))))
        newCount = if shift > size then count else floor ((maxx - minx) / size) -- in case too much precision is requested
        minxs = filter (< maxx) [minx + (fromIntegral i) * shift | i <- [0 .. newCount - 1]]
        step = size / (fromIntegral num)
        vals = concat $ map (\left ->
            let 
                xs = [left + (fromIntegral i) * step | i <- [0 .. num - 1]]
            in
                zip xs (AD.getValues (map (\x ->  [x]) xs) g s)
            ) minxs
    in 
        D.data1' $ V.fromList vals

getValues :: [[Double]] -> Either D.Data (Either S.Spline FS.Functions) -> [([Double], Double)]
getValues xs (Left dat) = zip xs (D.interpolatedValues xs dat)
getValues xs (Right (Left spline)) = 
    let
        filteredXs = filterXs spline xs
    in
        zip filteredXs (map (\x -> F.getValue_ x spline) filteredXs)
getValues xs (Right (Right fns)) = 
    let
        filteredXs = filterXs fns xs
    in
        zip filteredXs (map (\x -> F.getValue_ x fns) filteredXs)

filterXs ad xs = filter (\xs1 -> all (\(x1, xMin, xMax) -> x1 >= xMin && x1 <= xMax) (zip3 xs1 (AD.xMins ad) (AD.xMaxs ad))) xs

getValues1 :: V.Vector Double -> Either D.Data (Either S.Spline FS.Functions) -> V.Vector (Double, Double)
getValues1 xs (Left dat) = V.zip xs (D.interpolatedValues1 xs dat)
getValues1 xs (Right (Left spline)) = 
    let
        filteredXs = filterXs1 spline xs
    in
        V.zip filteredXs (V.map (\x -> F.getValue_ [x] spline) filteredXs)
getValues1 xs (Right (Right fns)) = 
    let
        filteredXs = filterXs1 fns xs
    in
        V.zip filteredXs (V.map (\x -> F.getValue_ [x] fns) filteredXs)

filterXs1 ad xs = V.filter (\x -> x >= AD.xMin1 ad && x <= AD.xMax1 ad) xs

-- | returns a bootstrap version of the data based on statistical model given as analytical data
bootstrap :: (Either S.Spline FS.Functions) -> Data -> Data -> IO Data
bootstrap ad d diff = do
    stdGen <- getStdGen
    rndVect :: V.Vector Int <- withSystemRandom . asGenST $ \gen -> uniformVector gen (dataLength d)
    let
        (stdGen1, stdGen2) = split stdGen
        ds = values1 diff
        newVals = V.zipWith (\r i ->
            let
                j = r `mod` (V.length ds)
                (xi, yi, wi) = ds V.! i
                (xj, yj, wj) = ds V.! j
            in
                (xi, yj * wi / wj , wi) 
            ) rndVect (V.generate (V.length ds) (\i -> i))
        bsDiffData = if D.isData diff
            then
                D.data1 newVals
            else D.spectrum1 newVals
        Left bsData = binaryOp F.add (Left bsDiffData) (Right ad) True stdGen1
    return bsData

-- | Returns variance of data set against the other data set
var :: Data -> Either Data (Either S.Spline FS.Functions) -> Double
var dat1 (Left dat2) = 
    -- Both data sets must have the same x coordinates and weights
    Sample.varianceWeighted (V.zip (V.zipWith (\y1 y2 -> (y1 - y2)) ys1 ys2) ws1) where
        ys1 = D.ys dat1
        ws1 = D.ws dat1
        ys2 = D.ys dat2
var dat (Right ad) = 
    Sample.varianceWeighted (V.fromList (zip (zipWith (\y y1 -> (y - y1)) ys adValues) ws)) where
        (xs, ys, ws) = unzip3 $ D.values dat
        adValues = 
            case ad of
                Left spline -> AD.getValues_ xs spline
                Right fs -> AD.getValues_ xs fs
        

-- | Returns standard deviation of data set against the other data set
stdev :: Data -> Either Data (Either S.Spline FS.Functions) -> Double
stdev dat d = sqrt $ var dat d

format :: Either Data (Either S.Spline FS.Functions) -> String
format (Left d) = concatMap (\(xs, y, w) -> (concatMap (\x -> show x ++ " ") xs) ++ show y ++ " " ++ show w ++ "\n") (D.values d)
format (Right (Left s)) = show s 
format (Right (Right f)) = show f 

dataRange :: Either D.Data (Either S.Spline FS.Functions) -> ([Double], [Double])
dataRange (Left d) = (D.xMins d, D.xMaxs d)
dataRange (Right (Left s)) = (AD.xMins s, AD.xMaxs s)
dataRange (Right (Right f)) = (AD.xMins f, AD.xMaxs f)

reshuffleData :: Data -> IO Data
reshuffleData dat = 
    do 
        gen <- createSystemRandom
        let
            vals = D.values1 dat
            shuffleFunc i vs = do
                if V.null vs 
                    then return V.empty
                    else do 
                        r :: Int <- asGenIO (uniform) gen
                        let 
                            r1 = r `mod` V.length vs
                        shuffledVals <- shuffleFunc (i + 1) ((V.take r1 vs) V.++ (V.drop (r1 + 1) vs))
                        let
                            (x1, y1, w1) = vals V.! i 
                            (_, y2, w2) = vs V.! r1
                        return $ (x1, y1 + (y2 - y1) * w1 / w2, w1) `V.cons` shuffledVals 
        shuffledVals <-shuffleFunc 0 vals 
        --putStrLn (show shuffledVals)
        return $ D.data1 shuffledVals
