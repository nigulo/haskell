
module Regression.Utils (
    dataToDataOp,
    dataToADOp,
    dataConstantOp,
    sampleAnalyticData,
    sampleAnalyticData_,
    sample2dAnalyticData,
    getValues,
    getValues1,
    bootstrap,
    var,
    stdev,
    dataRange,
    reshuffleData,
    bootstrapSample    
    ) where

import Regression.Data as D
import qualified Regression.Polynom as P
import qualified Regression.Spline as S
import qualified Regression.Functions as FS
import qualified Regression.AnalyticData as AD
import qualified Regression.AnalyticDataWrapper as ADW
import qualified Math.Function as F
import qualified Math.Expression as E

import Data.List
import qualified Data.Vector.Unboxed as V
import System.Random
import System.Random.MWC hiding (uniform)
import qualified System.Random.MWC as MWC
import Utils.Misc
import Debug.Trace
import qualified Statistics.Sample as Sample

dataToDataOp :: (RandomGen g) => F.Function Double -> Data -> Data -> Bool -> g -> Data
dataToDataOp op d1 d2 yOrx g = 
    let 
        xs2 = xs1 d2
        xs21 = V.filter (\x -> x >= V.minimum xs2 && x <= V.maximum xs2) (xs1 d1)
        d1' = sampleData1 xs21 d1
        d2' = interpolatedData1 xs21 d2
        vs = 
            if yOrx
            then
                V.zipWith (\(x1, y1, w1) (_, y2, w2) -> (x1, F.getValue_ [y1, y2] g op, w1 + w2)) (values1 d1') (values1 d2')
            else
                V.zipWith (\(x1, y1, w1) (x2, _, w2) -> (F.getValue_ [x1, x2] g op, y1, w1)) (values1 d1') (values1 d2')
    in
        case d1 of 
            Data2 _ -> data1 vs
            Spectrum2 _ -> spectrum1 vs

dataToADOp :: (RandomGen g) => F.Function Double -> Data -> ADW.AnalyticDataWrapper -> Bool -> g -> Data
dataToADOp op d ad yOrx g = 
    let 
        (g1, g2) = System.Random.split g
        ys = ADW.getValues (xs d) g1 ad
            
        vs = 
            if yOrx
            then zipWith (\(x1, y1, w1) (y, g) -> (x1, F.getValue [y1, y] [] g op, w1)) (V.toList (values1 d)) (zip ys (randomGens g2))
            else zipWith (\(x1, y1, w1) (x, g) -> (F.getValue [x1, x] [] g op, y1, w1)) (V.toList (values1 d)) (zip (V.toList (xs1 d)) (randomGens g2))
    in
        case d of 
            Data2 _ -> data1 $ V.fromList vs
            Spectrum2 _ -> spectrum1 $ V.fromList vs

dataConstantOp :: (RandomGen g) => F.Function Double -> Data -> Double -> Bool -> g -> Data
dataConstantOp op d k yOrx g = 
    let 
        vs = 
            if yOrx
            then V.map (\(x, y, w) -> (x, F.getValue_ [y, k] g op, w)) (values1 d)
            else V.map (\(x, y, w) -> (F.getValue_ [x, k] g op, y, w)) (values1 d)
    in
        case d of 
            Data2 _ -> data1 vs
            Spectrum2 _ -> spectrum1 vs


-- | Converts a given analytic data to data of specified number of samples
sampleAnalyticData :: (RandomGen g) => ADW.AnalyticDataWrapper -> [Double] -> [Double] -> [Int] -> g -> Data

sampleAnalyticData s minxs maxxs nums g =
    case ADW.xMins s of
        _:[] ->
            -- 2d analytic data sampling
            let 
                [minx] = minxs 
                [maxx] = maxxs 
                [num] = nums 
                step = if num <= 1 then 0
                        else (maxx - minx) / (fromIntegral num - 1)
            in 
                Spectrum2 ((minx, step),
                    V.zip (V.fromList (ADW.getValues (map (\x ->  [x]) [minx, minx + step .. maxx]) g s)) (V.replicate num 1))
        _:_:[] ->
            -- 3d analytic data sampling
            let 
                xs =
                    zipWith3 (\minx maxx num ->  
                        let step = if num <= 1 then maxx - minx else (maxx - minx) / (fromIntegral num - 1)
                        in [minx, minx + step .. maxx]
                    ) minxs maxxs nums
                
                xss = sequence xs
                ys = ADW.getValues xss g s
            in 
                data2 $ V.fromList $ zipWith (\[x1, x2] y -> (x1, x2, y, 1)) xss ys

sampleAnalyticData_ :: (RandomGen g) => ADW.AnalyticDataWrapper -> [Int] -> g -> Data
sampleAnalyticData_ s = sampleAnalyticData s (ADW.xMins s) (ADW.xMaxs s) 
 
-- more detailed 2d analytic data sampling
sample2dAnalyticData :: (RandomGen g) => 
    ADW.AnalyticDataWrapper -- ^ Data to sample 
    -> (Int, Double, Int) -- ^ Number of samples within one piece, length of the piece, number of pieces
    -> g -> Data
sample2dAnalyticData s (num, size, count) g = 
    let 
        minx = size * fromIntegral (floor (ADW.xMin1 s / size))
        maxx = size * fromIntegral (ceiling (ADW.xMax1 s / size))
        shift = size * (max 1 (fromIntegral (floor ((maxx - minx) / (fromIntegral count) / size))))
        newCount = if shift > size then count else floor ((maxx - minx) / size) -- in case too much precision is requested
        minxs = filter (< maxx) [minx + (fromIntegral i) * shift | i <- [0 .. newCount - 1]]
        step = size / (fromIntegral num)
        vals = concat $ map (\left ->
            let 
                xs = [left + (fromIntegral i) * step | i <- [0 .. num - 1]]
            in
                zip xs (ADW.getValues (map (\x ->  [x]) xs) g s)
            ) minxs
    in 
        D.data1' $ V.fromList vals

getValues :: (RandomGen g) => [[Double]] -> Either D.Data (ADW.AnalyticDataWrapper) -> g -> [([Double], Double)]
getValues xs (Left dat) _ = zip xs (D.interpolatedValues xs dat)
getValues xs (Right ad) g = 
    let
        filteredXs = filterXs ad xs
    in
        zip filteredXs (zipWith (\x g -> F.getValue_ x g ad) filteredXs (randomGens g))

filterXs ad = filter (\xs1 -> all (\(x1, xMin, xMax) -> x1 >= xMin && x1 <= xMax) (zip3 xs1 (ADW.xMins ad) (ADW.xMaxs ad)))

getValues1 :: (RandomGen g) => V.Vector Double -> Either D.Data (ADW.AnalyticDataWrapper) -> g -> V.Vector (Double, Double)
getValues1 xs (Left dat) _ = V.zip xs (D.interpolatedValues1 xs dat)
getValues1 xs (Right ad) g = 
    let
        filteredXs = filterXs1 ad xs
    in
        V.zip filteredXs (applyToVectorWithRandomGen (\x g -> F.getValue_ [x] g ad) filteredXs g)

filterXs1 ad = V.filter (\x -> x >= ADW.xMin1 ad && x <= ADW.xMax1 ad)

-- | returns a bootstrap version of the data based on statistical model given as analytical data
bootstrap :: ADW.AnalyticDataWrapper -> Data -> Data -> IO Data
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
        bsData = dataToADOp F.add bsDiffData ad True stdGen1
    return bsData

-- | Returns variance of data set against the other data set
var :: Data -> Either Data ADW.AnalyticDataWrapper -> Double
var dat1 (Left dat2) = 
    -- Both data sets must have the same x coordinates and weights
    Sample.varianceWeighted (V.zip (V.zipWith (\y1 y2 -> (y1 - y2)) ys1 ys2) ws1) where
        ys1 = D.ys dat1
        ws1 = D.ws dat1
        ys2 = D.ys dat2
var dat (Right ad) = 
    Sample.varianceWeighted (V.fromList (zip (zipWith (\y y1 -> (y - y1)) ys adValues) ws)) where
        (xs, ys, ws) = unzip3 $ D.values dat
        adValues = ADW.getValues_ xs ad
        

-- | Returns standard deviation of data set against the other data set
stdev :: Data -> Either Data ADW.AnalyticDataWrapper -> Double
stdev dat d = sqrt $ var dat d

dataRange :: Either D.Data (ADW.AnalyticDataWrapper) -> ([Double], [Double])
dataRange (Left d) = (D.xMins d, D.xMaxs d)
dataRange (Right ad) = (ADW.xMins ad, ADW.xMaxs ad)


-- | reshuffle the data set (without replacements)
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
                        r :: Int <- asGenIO (MWC.uniform) gen
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

-- | bootstrap sample of the data
bootstrapSample :: Int -> Data -> IO Data
bootstrapSample count dat =
    do 
        gen <- createSystemRandom
        let
            vals = D.values1 dat
        vals1 <- V.generateM count (\_ -> do
            r :: Int <- asGenIO (MWC.uniform) gen
            let 
                r1 = r `mod` V.length vals
            return $ vals V.! r1
            )
        return $ data1 vals1    
            
            
            