module TSA.Data (
    ---------------
    isAnalytic,
    isContinuous,
    isDiscrete,
    is2d,
    dim,
    ---------------
    merge,
    applyToData,
    applyToData1,
    ---------------
    getDataType,
    getSubDataAt,
    dataRange,
    reshuffleData,
    createDataParams,
    createDataParams_,
    createSubDataParams,
    createSubDataParams_,
    subDataBinaryOp,
    subDataConstantOp
    ) where

import Debug.Trace

import qualified Regression.Data as D
import qualified Regression.AnalyticData as AD
import qualified Regression.AnalyticDataWrapper as ADW
import Regression.Functions as FS
import Regression.Spline as S
import Regression.Utils as U
import Math.Expression as E
import Math.Function as F


import TSA.CommonParams
import TSA.Params
import Utils.Misc
import Utils.List
import Utils.Concurrent

import Data.IORef
import Data.Either
import Data.Maybe
import Control.Concurrent.MVar
import Data.List
import Control.Applicative

import Control.Concurrent.SSem as SSem
import Control.Concurrent
--import Control.Monad hiding (join)

import qualified Data.Vector.Unboxed as V
import Control.Monad.IO.Class

import System.Random
import System.Random.MWC

isAnalytic :: DataParams -> Bool
isAnalytic dp =
    case subData (head (dataSet dp)) of 
             SD1 _ -> False
             otherwise -> True

isContinuous :: DataParams -> Bool
isContinuous = isAnalytic

isDiscrete :: DataParams -> Bool
isDiscrete dp =
    case subData (head (dataSet dp)) of 
             SD1 _ -> True
             otherwise -> False

is2d :: DataParams -> Bool
is2d dp =
    case subData (head (dataSet dp)) of 
             SD1 d -> D.is2d d
             SD2 s -> AD.is2d s
             SD3 f -> AD.is2d f
             SD4 rbf -> AD.is2d rbf

dim :: DataParams -> Int
dim dp =
    case subData (head (dataSet dp)) of 
             SD1 d -> D.dim d
             SD2 s -> AD.dim s
             SD3 f -> AD.dim f
             SD4 rbf -> AD.dim rbf

merge :: String -> DataParams -> DataParams -> DataParams
merge name dp1 dp2 = 
        DataParams {
            dataName = name, 
            dataDesc = name,
            dataSet = (dataSet dp1) ++ (dataSet dp2)
        }

applyToData :: (Int {- subdata no-} -> SubData -> (Double -> IO ()) -> IO [SubData]) 
        -> DataParams 
        -> [String] 
        -> TaskEnv
        -> IO [DataParams]
applyToData func dp names taskEnv = do
    let
        numSubSets = length $ dataSet dp
        puFunc = progressUpdateFunc taskEnv 
        mapOp (i, sdp) = do
            results <- func i (subData sdp) (\pct -> puFunc (((fromIntegral i + pct) / fromIntegral numSubSets)))
            puFunc $ fromIntegral (i + 1) / fromIntegral numSubSets
            return results
    resultsPerSubData <- mapM mapOp (zip [0, 1 ..] (dataSet dp))
    let
        results = resultsPerSubData 
    return $ zipWith (\name results -> DataParams {
        dataName = name, 
        dataDesc = "",
        dataSet = zipWith (\sdp result -> 
            SubDataParams {
                    subDataRange = subDataRange sdp,
                    subData = result 
                }) (dataSet dp) results
        }) names (transpose results)

applyToData1 :: (Int -> SubData -> (Double -> IO ()) -> IO (SubData)) 
        -> DataParams 
        -> String 
        -> TaskEnv
        -> IO DataParams
applyToData1 func dp name taskEnv = 
    applyToData (\i d pu -> func i d pu >>= \result -> return [result]) dp [name] taskEnv >>= \[result] -> return result

getDataType :: DataParams -> String
getDataType dp =
    case subData (head (dataSet dp)) of 
        SD1 d -> 
            if D.isData d then "Data" else "Spectrum"
        SD2 _ -> "Modulated spline"
        SD3 _ -> "Analytic function"
        SD4 _ -> "Radial basis functions"

getSubDataAt :: DataParams -> Int -> SubData
getSubDataAt dp i = subData (dataSet dp !! i)

createDataParams :: String -> String -> [SubDataParams] -> DataParams
createDataParams name desc dat =
    DataParams {
        dataName = name,
        dataDesc = desc, 
        dataSet = dat
    }

createDataParams_ :: String -> [SubDataParams] -> DataParams
createDataParams_ name = createDataParams name name

createSubDataParams :: ([Double], [Double]) 
    -> SubData 
    -> SubDataParams
createSubDataParams range dat =
    SubDataParams {
        subDataRange = range,
        subData = dat
    }

createSubDataParams_ :: SubData 
    -> SubDataParams
createSubDataParams_ dat = createSubDataParams (U.dataRange (unboxSubData dat)) dat

subDataBinaryOp :: (RandomGen g) => F.Function Double -> SubData -> SubData -> Bool -> g -> SubData
subDataBinaryOp op (SD1 d1) (SD1 d2) yOrx g = SD1 (U.dataToDataOp op d1 d2 yOrx g)
subDataBinaryOp op (SD1 d1) d2 yOrx g =
    let
        Right ad = unboxSubData d2
    in 
        SD1 (U.dataToADOp op d1 ad yOrx g)
subDataBinaryOp op (SD2 d1) (SD2 d2) _ _ = SD2 (F.binaryOp op d1 d2)
subDataBinaryOp op (SD3 d1) (SD3 d2) _ _ = SD3 (F.binaryOp op d1 d2)
subDataBinaryOp op (SD4 d1) (SD4 d2) _ _ = SD4 (F.binaryOp op d1 d2)
        

subDataConstantOp :: (RandomGen g) => F.Function Double -> SubData -> Double -> Bool -> g -> SubData
subDataConstantOp op (SD1 d) k yOrx g = SD1 (U.dataConstantOp op d k yOrx g)
subDataConstantOp op (SD2 d) k _ _ = SD2 (F.constantOp op d k)
subDataConstantOp op (SD3 d) k _ _ = SD3 (F.constantOp op d k)
subDataConstantOp op (SD4 d) k _ _ = SD4 (F.constantOp op d k)
