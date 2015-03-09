
module Regression.Statistic (
    Statistic,
    statistic,
    getValues,
    getDefinition,
    varNames,
    dataFuncs,
    isValid
    ) where

import qualified Math.Function as F
import qualified Regression.Data as D
import qualified Regression.Spline as S
import qualified Regression.Functions as Fs
import qualified Regression.AnalyticDataWrapper as AD
import Data.List
import Data.Maybe
import System.Random
import Control.Concurrent
import Debug.Trace
import Utils.Num
import Utils.Misc
import Utils.Math as Math
import Utils.Concurrent

import qualified Data.Vector.Unboxed as V
newtype Statistic = Statistic (F.Function Double) deriving (Show, Read)

getYAt = "getyat"
getXAt = "getxat"
getY = "gety"
getWeight = "getweight"
getLength = "getlength"
getXMin = "getxmin"
getXMax = "getxmax"
getYMin = "getymin"
getYMax = "getymax"

dataFuncNames = [getYAt, getXAt, getY, getWeight, getLength, getXMin, getXMax, getYMin, getYMax]
analyticDataFuncNames = [getY, getXMin, getXMax]

getVarVals :: (RandomGen g) => [Either D.Data AD.AnalyticDataWrapper] -> g -> [F.Function Double] -> [Double] 
getVarVals ds g (startFn:endFn:stepFn:valueFn:[]) = 
    let
        mapOp opName =
            case maybeDataSetIndexAndName opName of
                Just (i, funcName) -> getFunc ds g i funcName
                Nothing -> (\_ -> 0) -- TODO: change this approach
        start = F.getValue [] (map mapOp (F.funcNames startFn)) g startFn
        end = F.getValue [] (map mapOp (F.funcNames endFn)) g endFn
        step = F.getValue [] (map mapOp (F.funcNames stepFn)) g stepFn
    in
        [F.getValue [i] (map mapOp (F.funcNames valueFn)) g valueFn | i <- [start, start + step .. end]]


getValues :: (RandomGen g) => [Either D.Data AD.AnalyticDataWrapper] 
    -> [[F.Function Double]] 
    -> g 
    -> (Double -> IO ()) 
    -> (ThreadId -> IO ())
    -> IO ()
    -> Statistic 
    -> IO D.Data
getValues ds [varValDef] g puFunc initializer finalizer statistic = do
    let
        xs = getVarVals ds g varValDef
    ys <- calcConcurrently (\x puFunc ->
            return $ getValue ds [x] (mkStdGen 1) statistic
        ) puFunc initializer finalizer xs
    let
        spec2d = D.data1 (V.fromList (zip3 xs ys (replicate (length xs) 1)))
    return spec2d
getValues ds [varValDef1, varValDef2] g puFunc initializer finalizer statistic = do
    let
        xs = getVarVals ds g varValDef1
        ys = getVarVals ds g varValDef2
    vals <- calcConcurrently (\(i, j) puFunc ->
            do
                y <- return $ getValue ds [xs !! i, ys !! j] (mkStdGen 1) statistic
                return ([xs !! i, ys !! j], y, 0)
        )  puFunc initializer finalizer [(i, j) | i <- [0 .. length xs - 1], j <- [0 .. length ys - 1]]

    let
        spec3d = D.data2 vals
    return spec3d

getFunc :: (RandomGen g) => [Either D.Data AD.AnalyticDataWrapper] -> g -> Int -> String -> ([Double] -> Double)
getFunc ds g i funcName =
    let 
        d = ds !! i
    in
        if funcName == getYAt then
            case d of
                Left d ->
                    (\[i] -> D.yAt (round i) d)
            else if funcName == getXAt then
                case d of
                    Left d ->
                        case D.dim d of
                            1 -> (\[i] -> head (D.xAt (round i) d))
                            otherwise -> (\[i, j] -> (D.xAt (round i) d) !! (round j))
            else if funcName == getXMin then
                case d of
                    Left d ->
                        (\[] -> D.xMin1 d)
                    Right ad ->
                        (\[] -> AD.xMin1 ad)
            else if funcName == getXMax then
                case d of
                    Left d ->
                        (\[] -> D.xMax1 d)
                    Right ad ->
                        (\[] -> AD.xMax1 ad)
            else if funcName == getYMin then
                case d of
                    Left d ->
                        (\[] -> D.yMin d)
            else if funcName == getYMax then
                case d of
                    Left d ->
                        (\[] -> D.yMax d)
            else if funcName == getY then
                case d of
                    Left d ->
                        (\xs -> let Just y = D.getY xs d in y)
                    Right ad ->
                        (\xs -> AD.getValue xs g ad)
            else if funcName == getWeight then
                case d of
                    Left d ->
                        (\[i] -> D.wAt (round i) d)
            else if funcName == getLength then
                case d of
                    Left d ->
                        (\[] -> fromIntegral $ D.dataLength d)
            else (\_ -> 0) -- Should never happen

getValue :: (RandomGen g) => [Either D.Data AD.AnalyticDataWrapper] -> [Double] -> g -> Statistic -> Double
getValue ds xs g (Statistic f) =
    let
        ops = map (\opName ->
            case maybeDataSetIndexAndName opName of
                Just (i, funcName) -> getFunc ds g i funcName
                Nothing -> (\_ -> 0) -- TODO: change this approach
            ) $ F.funcNames f 
    in
        F.getValue xs ops g f
        
statistic :: String -> Statistic
statistic def = Statistic (F.function def)

getDefinition :: Statistic -> String
getDefinition (Statistic f) = F.initialExpression f

varNames :: Statistic -> [String]
varNames (Statistic f) = F.varNames f

maybeDataSetIndex :: String -> String -> Maybe Int
maybeDataSetIndex func funcName =
    if funcName == func then Just 0
    else 
        let
            suffix = drop (length funcName) func
        in
            if funcName `isPrefixOf` func && isInteger suffix then Just (read suffix)
            else Nothing

maybeDataSetIndexAndName :: String -> Maybe (Int, String)
maybeDataSetIndexAndName func = 
    let
        meybeIndexAndName = find (\(i, _) -> isJust i) $ map (\f -> (maybeDataSetIndex func f, f)) dataFuncNames
    in
        case meybeIndexAndName of
            Just (Just i, f) -> Just (i, f)
            Nothing -> Nothing

-- | returns array of boolean values, where 
--   True means that function at given array index can be applied to all data types,
--   otherwise the function must be applied to discrete data (excluding analytic data)
dataFuncs :: Statistic -> [Bool]
dataFuncs (Statistic f) = 
    let
        dataSetIndexAndName func = 
            let
                Just (i, f) = maybeDataSetIndexAndName func
            in
                (i, f)
        indicesAndNames = map (dataSetIndexAndName) $ F.funcNames f
        indicesAndVals =
            map (\(i, funcName) ->
                if funcName `elem` analyticDataFuncNames then (i, True) else (i, False)
                ) $ (trace ("indicesAndNames: " ++ show indicesAndNames) indicesAndNames)
        retVal = sortBy (\(i1, _) (i2, _) -> compare i1 i2) $ nubBy (\(i1, val1) (i2, val2) -> i1 == i2) $ 
            nubBy (\(i1, val1) (i2, val2) -> i1 == i2 && not val2) $ 
            (trace ("indicesAndVals: " ++ show indicesAndVals) indicesAndVals)
    in
        snd $ unzip (trace ("retVal: " ++ show retVal) retVal)

-- | returns True in case this statistic is valid, False otherwise
isValid :: Statistic -> Bool
isValid (Statistic f) = (F.isValid f) && (not $ Nothing `elem` (map (maybeDataSetIndexAndName) (F.funcNames f)))
