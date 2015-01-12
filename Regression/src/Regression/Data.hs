
module Regression.Data (
    Data (..), 
    xs, 
    xsi, 
    xs1, 
    ys,
    ws,
    xys,
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
    sampleData,
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
    interpolatedData,
    interpolate1,
    interpolatedValues1,
    interpolatedValue1,
    interpolatedData1,
    subtr,
    getMinima,
    getMaxima,
    getExtrema,
    getTangent,
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

--import Regression.Spline
-- | Defines a 2 dimentional measurement
--data (Real a, Real b) => Measurement a b = Measurement {x :: a, y :: b}

-- | Use Data and Spectrum constructors only for relatively small data sets
data Data = 
    Data [([Double], Double, Double)] | 
    Spectrum ([(Double {-offset-}, Double {-step-})], [(Double, Double)]) |
    Data2 (V.Vector (Double {-x-}, Double {-y-}, Double {-w-})) |
    Data3 (V.Vector (Double {-x1-}, Double {-x2-}, Double {-y-}, Double {-w-})) | 
    Spectrum2 ((Double {-offset-}, Double {-step-}), V.Vector (Double {-y-}, Double {-w-}))
             deriving (Show, Read)

instance Xml.XmlElement Data where
    toElement (Data values) = Xml.element xmlElementName [("type", "data"), ("version", "1")] [Right (show values)]
    toElement (Spectrum values) = Xml.element xmlElementName [("type", "spectrum"), ("version", "1")] [Right (show values)]
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
                        otherwise -> 
                            let 
                                dat = Data $ read $ head $ Xml.contentTexts e
                            in
                                case dim dat of
                                    1 -> data1 (values1 dat)
                                    2 -> Data3 (V.zip4 (xsi 0 dat) (xsi 1 dat) (ys dat) (ws dat))
                                    otherwise -> dat
                        
                "data2" -> Data2 $ read $ head $ Xml.contentTexts e
                "data3" -> Data3 $ read $ head $ Xml.contentTexts e
                "spectrum" ->
                    case version of
                        "0" -> Spectrum ([(read (Xml.attrValue e "offset") , read (Xml.attrValue e "step"))], map (\s -> read s) (Xml.contentTexts e)) 
                        otherwise -> 
                            let 
                                spec = Spectrum $ read $ head $ Xml.contentTexts e
                            in
                                 case dim spec of
                                    1 -> spectrum1 (values1 spec)
                                    otherwise -> spec
                                 
                "spectrum2" -> Spectrum2 $ read $ head $ Xml.contentTexts e

xmlElementName :: String
xmlElementName = "data"

xs :: Data -> [[Double]]
xs (Data ds) =
    let 
        (xss, _, _) = unzip3 ds
    in 
        xss
xs (Spectrum (offsetsAndSteps, values)) =
        map (\(offset, step) -> [offset + step * fromIntegral dx | dx <- [0 .. length values - 1]]) offsetsAndSteps
xs d@(Data2 _) = map (\x -> [x]) (V.toList (xs1 d))


xsi :: Int -> Data -> V.Vector Double
xsi i (Data ds) =
    let 
        (xss, _, _) = unzip3 ds
    in 
        V.fromList $ map (\xs -> xs !! i) xss
xsi i (Spectrum (offsetsAndSteps, values)) =
    let 
        (offset, step) = offsetsAndSteps !! i
    in  
        V.generate (length values) (\i -> offset + step * fromIntegral i)
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
xs1 (Data ds) =
    let 
        (xss, _, _) = unzip3 ds
    in 
        V.fromList $ map head xss
xs1 (Spectrum (((offset, step):_), values)) = 
    V.generate (length values) (\i -> offset + step * fromIntegral i)
xs1 dat = xsi 0 dat 

ys :: Data -> V.Vector Double
ys (Data ds) = 
    let 
        (_, y, _) = unzip3 ds
    in 
        V.fromList y
ys (Spectrum (_, values)) = fst $ V.unzip $ V.fromList $ values
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
ws (Data ds) = 
    let 
        (_, _, w) = unzip3 ds
    in 
        V.fromList $ w
ws (Spectrum (_, values)) = snd $ V.unzip $ V.fromList $ values
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
values (Data ds) = ds
values (Spectrum (_,  [])) = []
values (Spectrum (offsetsAndSteps, values)) = 
    [(map (\(offset, step) -> offset + step * fromIntegral i) offsetsAndSteps, fst (values !! i), snd (values !! i)) | i <- [0 .. length values - 1]]
values (Data2 ds) = map (\(x, y, w) -> ([x], y, w)) $ V.toList ds
values (Data3 ds) = map (\(x1, x2, y, w) -> ([x1, x2], y, w)) $ V.toList ds
values (Spectrum2 ((offset, step), values)) = 
    [let (y, w) = values V.! i in ([offset + step * fromIntegral i], y, w) | i <- [0 .. V.length values - 1]]

-- | Returns an array of pure measurements - (x, y, w) triples
values1 :: Data -> V.Vector (Double, Double, Double)
values1 (Data ds) = V.fromList $ map (\((x1:_), y, w) -> (x1, y, w)) ds
values1 (Spectrum (_,  [])) = V.empty
values1 (Spectrum (((offset, step):_), values)) = 
    V.generate (length values) (\i ->
        let 
            (y, w) = values !! i
        in
            (offset + step * fromIntegral i, y, w))
values1 (Data2 ds) = ds
values1 (Spectrum2 ((offset, step), values)) = 
    V.generate (V.length values) (\i ->
        let 
            (y, w) = values V.! i
        in
            (offset + step * fromIntegral i, y, w))

values2 :: Data -> V.Vector (Double, Double, Double, Double)
values2 (Data ds) = V.fromList $ map (\((x1:x2:_), y, w) -> (x1, x2, y, w)) ds
values2 (Spectrum (_,  [])) = V.empty
values2 (Spectrum ([(offset1, step1), (offset2, step2)], values)) = 
    V.fromList [(offset1 + step1 * fromIntegral i, offset2 + step2 * fromIntegral i, fst (values !! i), snd (values !! i)) | i <- [0 .. length values - 1]]
values2 (Data3 ds) = ds

-- | Returns an array of pure measurements - (x, y) pairs
xys :: Data -> [([Double], Double)]
xys (Data ds) = map (\(xs, y, w) -> (xs, y)) ds
xys (Spectrum (_,  [])) = []
xys (Spectrum (offsetsAndSteps, values)) = 
    [(map (\(offset, step) -> offset + step * fromIntegral i) offsetsAndSteps, fst (values !! i)) | i <- [0 .. length values - 1]]

-- | Returns an array of pure measurements - (x, y) pairs
xys1 :: Data -> V.Vector (Double, Double)
xys1 (Data ds) = V.fromList $ map (\((x1:_), y, w) -> (x1, y)) ds
xys1 (Spectrum (_,  [])) = V.empty
xys1 (Spectrum (((offset, step):_), values)) = 
    V.generate (length values) (\i -> (offset + step * fromIntegral i, fst $ values !! i))
xys1 (Data2 ds) = V.map (\(x, y, _) -> (x, y)) ds
xys1 (Spectrum2 ((offset, step), values)) = 
    V.generate (V.length values) (\i -> (offset + step * fromIntegral i, fst $ values V.! i))

-- | Returns an array of pure measurements - (x, y) pairs
xys2 :: Data -> V.Vector (Double, Double, Double)
xys2 (Data ds) = V.fromList $ map (\((x1:x2:_), y, _) -> (x1, x2, y)) ds
xys2 (Spectrum (_,  [])) = V.empty
xys2 (Spectrum ([(offset1, step1), (offset2, step2)], values)) = 
    V.fromList [(offset1 + step1 * fromIntegral i, offset2 + step2 * fromIntegral i, fst (values !! i)) | i <- [0 .. length values - 1]]
xys2 (Data3 ds) = V.map (\(x1, x2, y, _) -> (x1, x2, y) )ds

xAt :: Int -> Data -> [Double]
xAt i (Data ds) = 
    let 
        (x, _, _) = ds !! i
    in
        x
xAt i (Spectrum (offsetsAndSteps, _)) = map (\(offset, step) -> offset + step * fromIntegral i) offsetsAndSteps 
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
yAt i (Data ds) = 
    let 
        (_, y, _) = ds !! i
    in
        y
yAt i (Spectrum (_, values)) = fst (values !! i) 
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
wAt i (Data ds) = 
    let 
        (_, _, w) = ds !! i
    in
        w
wAt i (Spectrum (_, values)) = snd (values !! i)
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
valueAt i (Data ds) = ds !! i
valueAt i (Spectrum (offsetsAndSteps, values)) = 
    (map (\(offset, step) -> offset + step * fromIntegral i) offsetsAndSteps, fst (values !! i), snd (values !! i))
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
setY ys (Data ds)= 
    Data (zip3 xs (V.toList ys) ws) where
        (xs, _, ws) = unzip3 ds
setY ys (Spectrum (offsetAndSteps, ds))= 
    Spectrum (offsetAndSteps, zip (V.toList ys) ws) where
        (_, ws) = unzip ds
setY ys (Data2 ds)= 
    Data2 (V.zip3 xs ys ws) where
        (xs, _, ws) = V.unzip3 ds
setY ys (Data3 ds)= 
    Data3 (V.zip4 x1s x2s ys ws) where
        (x1s, x2s, _, ws) = V.unzip4 ds
setY ys (Spectrum2 (offsetAndStep, ds))= 
    Spectrum2 (offsetAndStep, V.zip ys ws) where
        (_, ws) = V.unzip ds

setW :: V.Vector Double -- ^ new values of ws
     -> Data 
     -> Data
setW ws (Data ds)= 
    Data (zip3 xs ys (V.toList ws)) where
        (xs, ys, _) = unzip3 ds
setW ws (Spectrum (offsetAndSteps, ds))= 
    Spectrum (offsetAndSteps, zip ys (V.toList ws)) where
        (ys, _) = unzip ds
setW ws (Data2 ds)= 
    Data2 (V.zip3 xs ys ws) where
        (xs, ys, _) = V.unzip3 ds
setW ws (Data3 ds)= 
    Data3 (V.zip4 x1s x2s ys ws) where
        (x1s, x2s, ys, _) = V.unzip4 ds
setW ws (Spectrum2 (offsetAndStep, ds))= 
    Spectrum2 (offsetAndStep, V.zip ys ws) where
        (ys, _) = V.unzip ds

getY :: [Double] -> Data -> Maybe Double
getY xs (Data ds) = 
    case find (\(xs1, _, _) -> all (\(x1, x) -> x1 > x) (zip xs1 xs)) ds of
        Just (_, y, _) -> Just y
        otherwise -> 
            let 
                (_, y, _) = last ds
            in 
                Just y
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
xMins d@(Data ms) = 
    minimumBy (\x1s x2s ->
        let
            x1x2s = zip x1s x2s
        in
            if all (\(x1, x2) -> x1 < x2) x1x2s then LT
            else if all (\(x1, x2) -> x1 == x2) x1x2s then EQ
            else GT
    ) $ xs d
xMins (Spectrum (offsetsAndSteps, _)) = 
    map (\(offset, _) -> offset) offsetsAndSteps
-- here we assume that the data is sorted
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
xMini i d@(Data ms) = V.minimum $ xsi i d
xMini i (Spectrum (offsetsAndSteps, _)) = 
    let
        (offset, _) = offsetsAndSteps !! i
    in
        offset
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
xMin1 (Data ms) = V.minimum $ xs1 (Data ms)
xMin1 (Spectrum (((offset, _):_), _)) = offset
xMin1 d = xMini 0 d 

-- | Returns the maximum x value for data
xMaxs :: Data -> [Double]
xMaxs d@(Data ms) = 
    maximumBy (\x1s x2s ->
        let
            x1x2s = zip x1s x2s
        in
            if all (\(x1, x2) -> x1 < x2) x1x2s then LT
            else if all (\(x1, x2) -> x1 == x2) x1x2s then EQ
            else GT
    ) $ xs d
xMaxs (Spectrum (offsetsAndSteps, vals)) = 
    map (\(offset, step) -> offset + (step * fromIntegral (length vals - 1))) offsetsAndSteps
-- here we assume that the data is sorted
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
xMaxi i d@(Data ms) = V.maximum $ xsi i d
xMaxi i (Spectrum (offsetsAndSteps, vals)) = 
    let
        (offset, step) = offsetsAndSteps !! i
    in
        offset + (step * fromIntegral (length vals - 1))
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
xMax1 (Data ms) = V.maximum $ xs1 (Data ms)
xMax1 (Spectrum (((offset, step):_), vals)) = offset + (step * fromIntegral (length vals - 1))
xMax1 d = xMaxi 0 d 

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

interpolatedData :: [[Double]] -> Data -> Data
interpolatedData xs1 dat = 
    Data $ zip3 xs1 (interpolatedValues xs1 dat) (replicate (length xs1) 1)


-- | Filters a subrange from data set from x1 (including) to x2 (excluding)
subSet1 :: (Double, Double) -> Data -> Data
subSet1 (x1, x2) d@(Data values) = 
    let 
        maxx = xMax1 d
        op = if x2 >= maxx then (<=) else (<)
    in Data (filter (\((x:_), _, _) -> x >= x1 && x `op` x2) values)
subSet1 (x1, x2) (Spectrum (((offset, step):_), values)) = 
    let 
        len = length values
        i1 = min (len - 1) $ max 0 $ ceiling ((x1 - offset) / step);
        i2 = min (len - 1) $ max 0 $ floor ((x2 - offset) / step);
    in 
        Spectrum ([(offset + fromIntegral i1 * step, step)], [values !! i | i <- [i1 .. i2]])
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
split1 splitPoint d@(Data values) = 
    let 
        maxx = xMax1 d
        op = if splitPoint >= maxx then (<=) else (<)
        (lefts, rights) = partitionEithers $ map (\((x:_), y, w) -> if x `op` splitPoint then Left ([x], y, w) else Right ([x], y, w)) values
    in (Data lefts, Data rights)
-- !!! Spectrum part is never tested !!!
split1 splitPoint (Spectrum (((offset, step):_), values)) = 
    let 
        len = length values
        iSplit = min (len - 1) $ max 0 $ ceiling ((splitPoint - offset) / step)
        (lefts, rights) = splitAt iSplit values 
    in 
        (Spectrum ([(offset, step)], lefts), Spectrum ([(offset + fromIntegral iSplit * step, step)], rights))
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
selectData1 (x1, x2) (y1, y2) inner d@(Data values) = 
    let xOp = if x2 == xMax1 d then (<=) else (<)
        yOp = if y2 == yMax d then (<=) else (<)
        op ((x:_), y, _)= x >= x1 && x `xOp` x2 && y >= y1 && y `yOp` y2
    in Data $
        filter (\v -> if inner then op v else not (op v)) values
selectData1 (x1, x2) (y1, y2) inner (Spectrum (((offset, step):_), values)) = 
    let 
        len = length values
        i1 = min (len - 1) $ max 0 $ ceiling ((x1 - offset) / step);
        i2 = min (len - 1) $ max 0 $ floor ((x2 - offset) / step);
    in 
        trace (show i1 ++ "::" ++ show i2)
            Spectrum ([(offset + fromIntegral i1 * step, step)], [values !! i | i <- [i1 .. i2]])
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
sampleData1 xs (Data d) = 
    Data $ filter (\((x:_), _, _) -> x `V.elem` xs) d
sampleData1 xs s@(Spectrum _) = 
    spectrum1 $ V.filter (\(x, _, _) -> x `V.elem` xs) (values1 s)
sampleData1 xs (Data2 d) = 
    Data2 $ V.filter (\(x, _, _) -> x `V.elem` xs) d
sampleData1 xs s@(Spectrum2 _) = 
    spectrum1 $ V.filter (\(x, _, _) -> x `V.elem` xs) (values1 s)

sampleData :: [[Double]] -> Data -> Data
sampleData xs d = 
    Data $ filter (\(x, _, _) -> x `elem` xs) (values d)

filterData :: (([Double], Double, Double) -> Bool) -> Data -> Data
filterData f d = Data (filter f (values d))

data1 :: V.Vector (Double, Double, Double) -> Data
data1 vals = Data2 vals

data1' :: V.Vector (Double, Double) -> Data
data1' vals = Data2 $ V.map (\(x, y) -> (x, y, 1)) vals

data2 :: [([Double], Double, Double)] -> Data
data2 vals@(([x], _, _):_) = Data2 $ V.fromList $ map (\([x], y, w) -> (x, y, w)) vals
data2 vals@(([x1, x2], _, _):_) = Data3 $ V.fromList $ map (\([x1, x2], y, w) -> (x1, x2, y, w)) vals
data2 vals = Data $ map (\(xs, y, w) -> (xs, y, w)) vals

data2' :: [([Double], Double)] -> Data
data2' vals@(([x], _):_) = Data2 $ V.fromList $ map (\([x], y) -> (x, y, 1)) vals
data2' vals@(([x1, x2], _):_) = Data3 $ V.fromList $ map (\([x1, x2], y) -> (x1, x2, y, 1)) vals
data2' vals = Data $ map (\(xs, y) -> (xs, y, 1)) vals

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


is2d :: Data -> Bool
is2d (Data (((_:[]), _, _):_)) = True
is2d (Spectrum ((_:[]), _)) = True
is2d (Data2 _) = True
is2d (Spectrum2 _) = True
is2d _ = False

is3d :: Data -> Bool
is3d (Data (((_:_:[]), _, _):_)) = True
is3d (Spectrum ((_:_:[]), _)) = True
is3d (Data3 _) = True
is3d _ = False

-- | return number of independent ordinal values this data set is defined on
dim :: Data -> Int
dim (Data []) = 1
dim (Data ((xs, _, _):_)) = length xs
dim (Spectrum (xs, _)) = length xs
dim (Data2 _) = 1
dim (Data3 _) = 2
dim (Spectrum2 _) = 1

isData :: Data -> Bool
isData (Data _) = True
isData (Data2 _) = True
isData (Data3 _) = True
isData _ = False

isSpectrum :: Data -> Bool
isSpectrum (Spectrum _) = True
isSpectrum (Spectrum2 _) = True
isSpectrum _ = False

toSpectrum :: Data -> Maybe Data
toSpectrum s@(Spectrum _) = Just s
toSpectrum s@(Spectrum2 _) = Just s
toSpectrum d@(Data2 _) = 
    let 
        xs = xs1 d
        diffs = V.zipWith (-) (V.tail xs) (V.init xs) 
        step = V.head diffs
    in
        if V.all (== step) diffs then Just (Spectrum2 ((V.head xs, step), V.zip (ys d) (ws d))) else Nothing
toSpectrum _ = Nothing 

--sort :: Data -> Data
--sort s@(Spectrum _) = s
--sort (Data vals) =
--    Data $ sortBy (\(xs1, _, _) (xs2, _, _) -> compare xs1 xs2) vals

dataLength :: Data -> Int
dataLength (Data ds) = length ds
dataLength (Spectrum (_, ds)) = length ds
dataLength (Data2 ds) = V.length ds
dataLength (Data3 ds) = V.length ds
dataLength (Spectrum2 (_, ds)) = V.length ds

--------------------------------------------------------------------------------
-- Functions only for 2D data/spectrum
--------------------------------------------------------------------------------

subtr :: Data -> Data -> Data
subtr (Data dat1) (Data dat2) = 
    let
        (xs, ys1, ws1) = unzip3 dat1
        (_, ys2, ws2) = unzip3 dat2
        ys = zipWith (-) ys1 ys2
        ws = zipWith (+) ws1 ws2
    in 
        Data $ zip3 xs ys ws
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

getTangent :: Data -> Data
getTangent d = 

    let d1 = V.fromList $ tangent $ V.toList $ values1 d where
        tangent ((x0, y0, w0):((x1, y1, w1):vals)) = 
            (x0, (y1 - y0) / xDiff, (w0 + w1) / xDiff):tangent ((x1, y1, w1):vals) where
                xDiff = (x1 - x0)
        tangent _ = []
    in
        case d of 
            Data _ -> Data2 d1
            Spectrum _ -> spectrum1 d1
            Data2 _ -> Data2 d1
            Spectrum2 _ -> spectrum1 d1

getMinima :: Data -> V.Vector (Double, Double)
getMinima d = fst $ getExtrema d

getMaxima :: Data -> V.Vector (Double, Double)
getMaxima d = snd $ getExtrema d

-- | Returns an array containing minima and maxima of given data set (currently supported for 2d data only)
getExtrema :: Data -> (V.Vector (Double, Double) {-minima-}, V.Vector (Double, Double) {-maxima-})
getExtrema d =
    let
        vals = V.toList $ xys1 d
        getExtrema' ((_, _):(_, _):[]) = (V.empty, V.empty)
        getExtrema' ((x0, y0):(x1, y1):(x2, y2):xys) =
            let
                (minima, maxima) = getExtrema' ((x1, y1):(x2, y2):xys)
            in
            if (y1 - y0) < 0 && (y1 - y2) < 0 then (V.cons (x1, y1) minima, maxima)
                else if (y1 - y0) > 0 && (y1 - y2) > 0 then (minima, V.cons (x1, y1) maxima)
                else (minima, maxima)
            
    in        
        getExtrema' vals

