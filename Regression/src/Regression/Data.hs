
module Regression.Data (
    Data (Data2, Data3, Spectrum2), 
    xs, 
    xsi, 
    xs1, 
    ys,
    ws,
    xys1,
    xys2,
    values, 
    values1,
    values2,
    xMins,
    xMini, 
    xMin1, 
    xMaxs,
    xMaxi, 
    xMax1, 
    yMin, 
    yMax, 
    subSet1,
    split1,
    xAt,
    yAt,
    wAt,
    valueAt,
    setY,
    setW,
    getY,
    selectData1,
    --sampleData,
    sampleData1,
    data1,
    data1',
    data2,
    data2',
    spectrum1,
    spectrum1',
    interpolate,
    interpolatedValues,
    interpolatedValue,
    --interpolatedData,
    interpolate1,
    interpolate1',
    interpolatedValues1,
    interpolatedValue1,
    interpolatedData1,
    subtr,
    getMinima,
    getMaxima,
    getExtrema,
    getTangent,
    getZeroCrossings,
    xmlElementName,
    is2d,
    is3d,
    dim,
    isData,
    isSpectrum,
    filterData,
    dataLength,
    toSpectrum
    ) where

import qualified Regression.Spline as S
import Debug.Trace
import Data.List
import qualified Utils.Xml as Xml
import Utils.Misc
import Data.Either
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S

data Data = 
    Data1 (V.Vector (Double {-y-}, Double {-w-})) |
    Data2 (V.Vector (Double {-x-}, Double {-y-}, Double {-w-})) |
    Data3 (V.Vector (Double {-x1-}, Double {-x2-}, Double {-y-}, Double {-w-})) | 
    Spectrum2 ((Double {-offset-}, Double {-step-}), V.Vector (Double {-y-}, Double {-w-}))
             deriving (Show, Read)

instance Xml.XmlElement Data where
    toElement (Data1 values) = Xml.element xmlElementName [("type", "data1"), ("version", "1")] [Right (show values)]
    toElement (Data2 values) = Xml.element xmlElementName [("type", "data2"), ("version", "1")] [Right (show values)]
    toElement (Data3 values) = Xml.element xmlElementName [("type", "data3"), ("version", "1")] [Right (show values)]
    toElement (Spectrum2 values) = Xml.element xmlElementName [("type", "spectrum2"), ("version", "1")] [Right (show values)]
    
    fromElement e =
        let
            version = maybe_ "0" $ Xml.maybeAttrValue e "version"
        in
            case (Xml.attrValue e "type") of
                "data" ->
                    case version of 
                        "0" -> Data2 $ V.fromList $ map (\s -> read s) (Xml.contentTexts e)
                        otherwise -> data2 $ read $ head $ Xml.contentTexts e
                        
                "data1" -> Data1 $ read $ head $ Xml.contentTexts e
                "data2" -> Data2 $ read $ head $ Xml.contentTexts e
                "data3" -> Data3 $ read $ head $ Xml.contentTexts e
                "spectrum" ->
                    case version of
                        "0" -> 
                            let
                                offset = read (Xml.attrValue e "offset")
                                step = read (Xml.attrValue e "step")
                                vals = map (\s -> read s) (Xml.contentTexts e)
                            in
                                spectrum1 $ V.fromList (zipWith (\(y, w) i -> (offset + step * fromIntegral i, y, w)) vals [0 ..]) 
                        otherwise -> 
                            let 
                                (offsetsAndSteps, vals) = read $ head $ Xml.contentTexts e
                            in
                                 case offsetsAndSteps of
                                    [(offset, step)] -> spectrum1 $ V.fromList (zipWith (\(y, w) i -> (offset + step * fromIntegral i, y, w)) vals [0 ..])
                "spectrum2" -> Spectrum2 $ read $ head $ Xml.contentTexts e

xmlElementName :: String
xmlElementName = "data"

xs :: Data -> [[Double]]
xs d = map (\x -> [x]) (V.toList (xs1 d))

xsi :: Int -> Data -> V.Vector Double
xsi 0 (Data1 ds) = V.empty 
xsi 0 (Data2 ds) = 
    let 
        (xs, _, _) = V.unzip3 ds
    in 
        xs
xsi 0 (Data3 ds) = 
    let 
        (xs1, _, _, _) = V.unzip4 ds
    in 
        xs1
xsi 1 (Data3 ds) = 
    let 
        (_, xs2, _, _) = V.unzip4 ds
    in 
        xs2
xsi 0 (Spectrum2 ((offset, step), values)) =
    V.generate (V.length values) (\i -> offset + step * fromIntegral i)

xs1 :: Data -> V.Vector Double
xs1 = xsi 0 

ys :: Data -> V.Vector Double
ys (Data1 ds) = 
    let 
        (y, _) = V.unzip ds
    in 
        y
ys (Data2 ds) = 
    let 
        (_, y, _) = V.unzip3 ds
    in 
        y
ys (Data3 ds) = 
    let 
        (_, _, y, _) = V.unzip4 ds
    in 
        y
ys (Spectrum2 (_, values)) = fst $ V.unzip values

ws :: Data -> V.Vector Double
ws (Data1 ds) = 
    let 
        (_, w) = V.unzip ds
    in 
        w
ws (Data2 ds) = 
    let 
        (_, _, w) = V.unzip3 ds
    in 
        w
ws (Data3 ds) = 
    let 
        (_, _, _, w) = V.unzip4 ds
    in 
        w
ws (Spectrum2 (_, values)) = snd $ V.unzip $ values

-- | Returns an array of pure measurements - (x, y, w) triples
values :: Data -> [([Double], Double, Double)]
values (Data1 ds) = map (\(y, w) -> ([], y, w)) $ V.toList ds
values (Data2 ds) = map (\(x, y, w) -> ([x], y, w)) $ V.toList ds
values (Data3 ds) = map (\(x1, x2, y, w) -> ([x1, x2], y, w)) $ V.toList ds
values (Spectrum2 ((offset, step), values)) = 
    [let (y, w) = values V.! i in ([offset + step * fromIntegral i], y, w) | i <- [0 .. V.length values - 1]]

-- | Returns an array of pure measurements - (y, w) pairs
values0 :: Data -> V.Vector (Double, Double)
values0 (Data1 ds) = ds

-- | Returns an array of pure measurements - (x, y, w) triples
values1 :: Data -> V.Vector (Double, Double, Double)
values1 (Data2 ds) = ds
values1 (Spectrum2 ((offset, step), values)) = 
    V.generate (V.length values) (\i ->
        let 
            (y, w) = values V.! i
        in
            (offset + step * fromIntegral i, y, w))

values2 :: Data -> V.Vector (Double, Double, Double, Double)
values2 (Data3 ds) = ds

-- | Returns an array of pure measurements - (x, y) pairs
xys1 :: Data -> V.Vector (Double, Double)
xys1 (Data2 ds) = V.map (\(x, y, _) -> (x, y)) ds
xys1 (Spectrum2 ((offset, step), values)) = 
    V.generate (V.length values) (\i -> (offset + step * fromIntegral i, fst $ values V.! i))

-- | Returns an array of pure measurements - (x, y) pairs
xys2 :: Data -> V.Vector (Double, Double, Double)
xys2 (Data3 ds) = V.map (\(x1, x2, y, _) -> (x1, x2, y) )ds

xAt :: Int -> Data -> [Double]
xAt _ (Data1 ds) = [] 
xAt i (Data2 ds) = 
    let 
        (x, _, _) = ds V.! i
    in
        [x]
xAt i (Data3 ds) = 
    let 
        (x1, x2, _, _) = ds V.! i
    in
        [x1, x2]
xAt i (Spectrum2 ((offset, step), _)) = [offset + step * fromIntegral i] 

yAt :: Int -> Data -> Double
yAt i (Data1 ds) = 
    let 
        (y, _) = ds V.! i
    in
        y
yAt i (Data2 ds) = 
    let 
        (_, y, _) = ds V.! i
    in
        y
yAt i (Data3 ds) = 
    let 
        (_, _, y, _) = ds V.! i
    in
        y
yAt i (Spectrum2 (_, values)) = fst (values V.! i) 

wAt :: Int -> Data -> Double
wAt i (Data1 ds) = 
    let 
        (_, w) = ds V.! i
    in
        w
wAt i (Data2 ds) = 
    let 
        (_, _, w) = ds V.! i
    in
        w
wAt i (Data3 ds) = 
    let 
        (_, _, _, w) = ds V.! i
    in
        w
wAt i (Spectrum2 (_, values)) = snd (values V.! i)

valueAt :: Int -> Data -> ([Double], Double, Double)
valueAt i (Data1 ds) = 
    let
        (y, w) = ds V.! i
    in
        ([], y, w)
valueAt i (Data2 ds) = 
    let
        (x, y, w) = ds V.! i
    in
        ([x], y, w)
valueAt i (Spectrum2 ((offset, step), values)) =
    let
        (y, w) = values V.!i
    in 
        ([offset + step * fromIntegral i], y, w)

setY :: V.Vector Double -- ^ new values of ys
     -> Data 
     -> Data
setY ys (Data1 ds) = 
    Data1 (V.zip ys ws) where
        (_, ws) = V.unzip ds
setY ys (Data2 ds) = 
    Data2 (V.zip3 xs ys ws) where
        (xs, _, ws) = V.unzip3 ds
setY ys (Data3 ds) = 
    Data3 (V.zip4 x1s x2s ys ws) where
        (x1s, x2s, _, ws) = V.unzip4 ds
setY ys (Spectrum2 (offsetAndStep, ds)) = 
    Spectrum2 (offsetAndStep, V.zip ys ws) where
        (_, ws) = V.unzip ds

setW :: V.Vector Double -- ^ new values of ws
     -> Data 
     -> Data
setW ws (Data1 ds) = 
    Data1 (V.zip ys ws) where
        (ys, _) = V.unzip ds
setW ws (Data2 ds) = 
    Data2 (V.zip3 xs ys ws) where
        (xs, ys, _) = V.unzip3 ds
setW ws (Data3 ds)= 
    Data3 (V.zip4 x1s x2s ys ws) where
        (x1s, x2s, ys, _) = V.unzip4 ds
setW ws (Spectrum2 (offsetAndStep, ds))= 
    Spectrum2 (offsetAndStep, V.zip ys ws) where
        (ys, _) = V.unzip ds

getY :: [Double] -> Data -> Maybe Double
getY (x:_) (Data2 ds) = 
    case V.find (\(x', _, _) -> x' > x) ds of
        Just (_, y, _) -> Just y
        otherwise -> 
            let 
                (_, y, _) = V.last ds
            in 
                Just y
getY (x1:x2:_) (Data3 ds) = 
    case V.find (\(x1', x2', _, _) -> x1' > x1 && x2' > x2) ds of
        Just (_, _, y, _) -> Just y
        otherwise -> 
            let 
                (_, _, y, _) = V.last ds
            in 
                Just y

-- | Returns the minimum x value for data
xMins :: Data -> [Double]
-- here we assume that the data is sorted
xMins (Data1 ds) = [] 
xMins d@(Data2 ds) = 
    let
        (x, _, _) = V.head ds
    in
        [x]  
xMins d@(Data3 ds) = 
    let
        (x1, x2, _, _) = V.head ds
    in
        [x1, x2]
xMins (Spectrum2 ((offset, _), _)) = [offset]

-- | Returns the minimum x value for data
xMini :: Int -> Data -> Double
xMini 0 d@(Data2 ds) = 
    let
        (x, _, _) = V.head ds
    in
        x
xMini 0 d@(Data3 ds) = 
    let
        (x1, _, _, _) = V.head ds
    in
        x1
xMini 1 d@(Data3 ds) = 
    let
        (_, x2, _, _) = V.head ds
    in
        x2
xMini 0 (Spectrum2 ((offset, _), _)) = offset

-- | Returns the minimum x value for data
xMin1 :: Data -> Double
xMin1 = xMini 0 

-- | Returns the maximum x value for data
xMaxs :: Data -> [Double]
-- here we assume that the data is sorted
xMaxs (Data1 ds) = [] 
xMaxs d@(Data2 ds) = 
    let
        (x, _, _) = V.last ds
    in
        [x]  
xMaxs d@(Data3 ds) = 
    let
        (x1, x2, _, _) = V.last ds
    in
        [x1, x2]
xMaxs (Spectrum2 ((offset, step), vals)) = [offset + (step * fromIntegral (V.length vals - 1))]

-- | Returns the maximum x value for data
xMaxi :: Int -> Data -> Double
xMaxi 0 d@(Data2 ds) = 
    let
        (x, _, _) = V.last ds
    in
        x  
xMaxi 0 d@(Data3 ds) = 
    let
        (x1, _, _, _) = V.last ds
    in
        x1
xMaxi 1 d@(Data3 ds) = 
    let
        (_, x2, _, _) = V.last ds
    in
        x2
xMaxi 0 (Spectrum2 ((offset, step), vals)) = offset + (step * fromIntegral (V.length vals - 1))

-- | Returns the maximum x value for data
xMax1 :: Data -> Double
xMax1 = xMaxi 0 

-- | Returns the minimum y value for data
yMin :: Data -> Double
yMin ds = V.minimum $ ys ds

-- | Returns the maximum y value for data
yMax :: Data -> Double
yMax ds = V.maximum $ ys ds

interpolate1 :: Double -> Data -> Double
interpolate1 xValue dat = interpolate1' xValue (values1 dat)

interpolate1' :: Double -> V.Vector (Double, Double, Double) -> Double
interpolate1' xValue vals =
    let
        (x1, y1, w1) = 
            --foldl1 (\v1@(x1, y1, w1) v2@(x2, y2, w2) -> if x1 >= x2 then v1 else v2) $ 
            V.maximumBy (\(x1, _, _) (x2, _, _) -> if x1 <= x2 then LT else GT) $ 
                V.filter (\(x, y, w) -> x <= xValue) vals
        (x2, y2, w2) = 
            --foldl1 (\v1@(x1, y1, w1) v2@(x2, y2, w2) -> if x1 <= x2 then v1 else v2) $ 
            V.minimumBy (\(x1, _, _) (x2, _, _) -> if x1 <= x2 then LT else GT) $ 
                V.filter (\(x, y, w) -> x >= xValue) vals
    in
        if (x2 == x1) 
        then y1 
        else y1 + (xValue - x1) * (y2 - y1) / (x2 - x1)

interpolatedValues1 :: V.Vector Double -> Data -> V.Vector Double
interpolatedValues1 xs1 dat = 
    V.map ((flip interpolate1') (values1 dat)) xs1

interpolatedValue1 :: Double -> Data -> Double
interpolatedValue1 x dat = V.head $ interpolatedValues1 (V.singleton x) dat

interpolatedData1 :: V.Vector Double -> Data -> Data
interpolatedData1 xs1 dat = 
    Data2 $ V.zip3 xs1 (interpolatedValues1 xs1 dat) (V.replicate (V.length xs1) 1)

interpolate :: [Double] -> Data -> Double
interpolate xValues dat = interpolate' xValues (values dat)

interpolate' :: [Double] -> [([Double], Double, Double)] -> Double
interpolate' xValues vals =
    let
        ysSumSqrDiffs = map (\(xs, y, _) -> 
            let
                sumSqrDiffs = sum (map (\(xValue, x) ->
                    let
                         d = xValue - x
                    in 
                        d * d
                    ) (zip xValues xs))
            in 
                (y, sumSqrDiffs)        
            ) vals
        
    in
       case find (\(y, sumSqrDiffs) -> if sumSqrDiffs == 0 then True else False) ysSumSqrDiffs of
       Just (y, _) -> y
       otherwise ->
        let 
            (yWeights, weights) = unzip $ map (\(y, sumSqrDiffs) -> (y / sumSqrDiffs, 1 / sumSqrDiffs)) ysSumSqrDiffs
        in
            (sum yWeights) / (sum weights)

        
interpolatedValues :: [[Double]] -> Data -> [Double]
interpolatedValues xs dat = 
    if is2d dat 
        then map ((flip interpolate1') (values1 dat)) (map head xs)
        else map ((flip interpolate') (values dat)) xs
        
interpolatedValue :: [Double] -> Data -> Double
interpolatedValue x dat = head $ interpolatedValues [x] dat

--interpolatedData :: [[Double]] -> Data -> Data
--interpolatedData xs1 dat = 
--    data2 $ zip3 xs1 (interpolatedValues xs1 dat) (replicate (length xs1) 1)


-- | Filters a subrange from data set from x1 (including) to x2 (excluding)
subSet1 :: (Double, Double) -> Data -> Data
subSet1 (x1, x2) d@(Data2 values) = 
    let 
        maxx = xMax1 d
        op = if x2 >= maxx then (<=) else (<)
    in Data2 (V.filter (\(x, _, _) -> x >= x1 && x `op` x2) values)
subSet1 (x1, x2) (Spectrum2 ((offset, step), values)) = 
    let 
        len = V.length values
        i1 = min (len - 1) $ max 0 $ ceiling ((x1 - offset) / step);
        i2 = min (len - 1) $ max 0 $ floor ((x2 - offset) / step);
    in 
        Spectrum2 ((offset + fromIntegral i1 * step, step), V.generate (i2 - i1 + 1) (\i -> values V.! (i1 + i)))

-- | Splits the data at the given point to two data sets
split1 :: Double -> Data -> (Data, Data)
split1 splitPoint d@(Data2 values) = 
    let 
        maxx = xMax1 d
        op = if splitPoint >= maxx then (<=) else (<)
        (lefts, rights) = V.partition (\(x, y, w) -> x `op` splitPoint) values
    in (Data2 lefts, Data2 rights)
-- !!! Spectrum part is never tested !!!
split1 splitPoint (Spectrum2 ((offset, step), values)) = 
    let 
        len = V.length values
        iSplit = min (len - 1) $ max 0 $ ceiling ((splitPoint - offset) / step)
        (lefts, rights) = V.splitAt iSplit values 
    in 
        (Spectrum2 ((offset, step), lefts), Spectrum2 ((offset + fromIntegral iSplit * step, step), rights))

-- | Filters a subrange from data set from x1 (including) to x2 (excluding)
selectData1 :: (Double, Double) -> (Double, Double) -> Bool -> Data -> Data
selectData1 (x1, x2) (y1, y2) inner d@(Data2 values) = 
    let xOp = if x2 == xMax1 d then (<=) else (<)
        yOp = if y2 == yMax d then (<=) else (<)
        op (x, y, _)= x >= x1 && x `xOp` x2 && y >= y1 && y `yOp` y2
    in Data2 $
        V.filter (\v -> if inner then op v else not (op v)) values
selectData1 (x1, x2) (y1, y2) inner (Spectrum2 ((offset, step), values)) = 
    let 
        len = V.length values
        i1 = min (len - 1) $ max 0 $ ceiling ((x1 - offset) / step);
        i2 = min (len - 1) $ max 0 $ floor ((x2 - offset) / step);
    in
        Spectrum2 ((offset + fromIntegral i1 * step, step), V.generate (i2 - i1 + 1) (\i -> values V.! (i1 + i)))

sampleData1 :: V.Vector Double -> Data -> Data
sampleData1 xs (Data2 d) = 
    Data2 $ V.filter (\(x, _, _) -> x `V.elem` xs) d
sampleData1 xs s@(Spectrum2 _) = 
    spectrum1 $ V.filter (\(x, _, _) -> x `V.elem` xs) (values1 s)

--sampleData :: [[Double]] -> Data -> Data
--sampleData xs d = 
--    data2 $ filter (\(x, _, _) -> x `elem` xs) (values d)

filterData :: (([Double], Double, Double) -> Bool) -> Data -> Data
filterData f s@(Data1 _) = data0 (V.filter (\(y, w) -> f ([], y, w)) (values0 s))
filterData f s@(Data2 _) = data1 (V.filter (\(x, y, w) -> f ([x], y, w)) (values1 s))
filterData f s@(Data3 _) = data2 (V.filter (\(x, y, z, w) -> f ([x, y], z, w)) (values2 s))
filterData f s@(Spectrum2 _) = data1 (V.filter (\(x, y, w) -> f ([x], y, w)) (values1 s))

data0 :: V.Vector (Double, Double) -> Data
data0 vals = Data1 vals

data0' :: V.Vector (Double) -> Data
data0' vals = Data1 $ V.map (\y -> (y, 1)) vals

data1 :: V.Vector (Double, Double, Double) -> Data
data1 vals = Data2 vals

data1' :: V.Vector (Double, Double) -> Data
data1' vals = Data2 $ V.map (\(x, y) -> (x, y, 1)) vals

data2 :: V.Vector (Double, Double, Double, Double) -> Data
data2 = Data3

data2' :: V.Vector (Double, Double, Double) -> Data
data2' vals = Data3 $ V.map (\(x, y, z) -> (x, y, z, 1)) vals

spectrum1 :: V.Vector (Double, Double, Double) -> Data
spectrum1 vals = 
    let
        (xs1, ys1, ws1) = V.unzip3 vals
        xmin = V.minimum xs1
        xmax = V.maximum xs1
        len = fromIntegral $ V.length vals
    in 
        Spectrum2 ((xmin, if len > 1 then (xmax - xmin) / (len - 1) else 0), V.zip ys1 ws1)

spectrum1' :: V.Vector (Double, Double) -> Data
spectrum1' vals = 
    let
        (xs1, ys1) = V.unzip vals
        xmin = V.minimum xs1
        xmax = V.maximum xs1
        len = fromIntegral $ V.length vals
    in 
        Spectrum2 ((xmin, if len > 1 then (xmax - xmin) / (len - 1) else 0), V.zip ys1 (V.replicate (V.length ys1) 1))


is1d :: Data -> Bool
is1d (Data1 _) = True
is1d _ = False

is2d :: Data -> Bool
is2d (Data2 _) = True
is2d (Spectrum2 _) = True
is2d _ = False

is3d :: Data -> Bool
is3d (Data3 _) = True
is3d _ = False

-- | return number of independent ordinal values this data set is defined on
dim :: Data -> Int
dim (Data1 _) = 0
dim (Data2 _) = 1
dim (Data3 _) = 2
dim (Spectrum2 _) = 1

isData :: Data -> Bool
isData (Data1 _) = True
isData (Data2 _) = True
isData (Data3 _) = True
isData _ = False

isSpectrum :: Data -> Bool
isSpectrum (Spectrum2 _) = True
isSpectrum _ = False

toSpectrum :: Data -> Maybe Data
toSpectrum s@(Spectrum2 _) = Just s
toSpectrum d@(Data2 _) = 
    let 
        xs = xs1 d
        diffs = V.zipWith (-) (V.tail xs) (V.init xs) 
        step = V.head diffs
    in
        if V.all (== step) diffs then Just (Spectrum2 ((V.head xs, step), V.zip (ys d) (ws d))) else Nothing
toSpectrum _ = Nothing 

dataLength :: Data -> Int
dataLength (Data1 ds) = V.length ds
dataLength (Data2 ds) = V.length ds
dataLength (Data3 ds) = V.length ds
dataLength (Spectrum2 (_, ds)) = V.length ds

--------------------------------------------------------------------------------
-- Functions only for 2D data/spectrum
--------------------------------------------------------------------------------

subtr :: Data -> Data -> Data
subtr (Data2 dat1) (Data2 dat2) = 
    let
        (xs, ys1, ws1) = V.unzip3 dat1
        (_, ys2, ws2) = V.unzip3 dat2
        ys = V.zipWith (-) ys1 ys2
        ws = V.zipWith (+) ws1 ws2
    in 
        Data2 $ V.zip3 xs ys ws
subtr (Data3 dat1) (Data3 dat2) = 
    let
        (xs1, xs2, ys1, ws1) = V.unzip4 dat1
        (_, _, ys2, ws2) = V.unzip4 dat2
        ys = V.zipWith (-) ys1 ys2
        ws = V.zipWith (+) ws1 ws2
    in 
        Data3 $ V.zip4 xs1 xs2 ys ws

-- | Use only for data with even sampling
getTangent :: Data -> Data
getTangent d = 
    let d1 = V.fromList $ tangent $ V.toList $ values1 d where
        tangent ((x0, y0, w0):(x1, y1, w1):(x2, y2, w2):vals) = 
            (x1, (y2 - y0) / (x2 - x0), 1):tangent ((x1, y1, w1):(x2, y2, w2):vals)
        tangent _ = []
    in
        case d of 
            Data2 _ -> Data2 d1
            Spectrum2 _ -> spectrum1 d1

getMinima :: Data -> Bool -> V.Vector (Double, Double)
getMinima d global = fst $ getExtrema d global

getMaxima :: Data -> Bool -> V.Vector (Double, Double)
getMaxima d global = snd $ getExtrema d global

-- | Returns an array containing minima and maxima of given data set (currently supported for 2d data only)
--   Global minima are dectected based on mean of the data set
getExtrema :: Data -> Bool -> (V.Vector (Double, Double) {-minima-}, V.Vector (Double, Double) {-maxima-})
getExtrema d global =
    let
        vals = V.toList $ xys1 d
        getExtrema' ((_, _):(_, _):[]) = (V.empty, V.empty)
        getExtrema' ((x0, y0):(x1, y1):(x2, y2):xys) =
                if y1 == y2 
                    then getExtrema' ((x0, y0):(x2, y2):xys)
                else
                    let
                        (minima, maxima) = getExtrema' ((x1, y1):(x2, y2):xys)
                    in
                        if (y1 - y0) < 0 && (y1 - y2) < 0 
                            then 
                                (V.cons (x1, y1) minima, maxima)
                            else if (y1 - y0) > 0 && (y1 - y2) > 0 
                                then 
                                    (minima, V.cons (x1, y1) maxima)
                            else (minima, maxima)
        (localMinima, localMaxima) = getExtrema' vals
        extrema = 
            if global
                then
                    let
                        yMean = S.mean $ ys d
                        globalMinima1 = V.filter (\(x, y) -> y < yMean) localMinima
                        globalMaxima1 = V.filter (\(x, y) -> y > yMean) localMaxima
                        containsPointBetween x1 x2 xys = not $ V.null $ V.filter (\(x, _) -> x > x1 && x < x2) xys
                        globalMinima = V.fromList $ map (V.minimumBy (\(_, y1) (_, y2) -> compare y1 y2)) $ groupVectorBy (\(x1, _) (x2, _) -> 
                                if containsPointBetween (min x1 x2) (max x1 x2) globalMaxima1 then False else True
                            ) globalMinima1
                        globalMaxima = V.fromList $ map (V.maximumBy (\(_, y1) (_, y2) -> compare y1 y2)) $ groupVectorBy (\(x1, _) (x2, _) -> 
                                if containsPointBetween (min x1 x2) (max x1 x2) globalMinima then False else True
                            ) globalMaxima1
                    in
                        (globalMinima, globalMaxima)
                else
                    (localMinima, localMaxima)
    in        
        extrema

-- | Returns an array containing the abscissa of zero-crossings
--   The values returned are the closest x-values taken from input data (i.e. no interpolation)
getZeroCrossings :: Data -> V.Vector Double
getZeroCrossings d =
    let
        vals = V.toList $ xys1 d
        getZeroCrossings' ((_, _):(_, _):[]) = V.empty
        getZeroCrossings' ((x1, y1):(x2, y2):xys) =
            let
                zeroCrossings = getZeroCrossings' ((x2, y2):xys)
            in
                if y1 == 0 then V.cons x1 zeroCrossings
                else if signum y1 /= signum y2 then V.cons (if abs y1 < abs y2 then x1 else x2) zeroCrossings
                else zeroCrossings
            
    in        
        getZeroCrossings' vals
