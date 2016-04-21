module TSA.Data (
    ---------------
    isAnalytic,
    isContinuous,
    isDiscrete,
    is2d,
    ---------------
    merge,
    applyToData,
    applyToData1,
    calculateWeights,
    ---------------
    getDataType,
    getSubDataAt,
    dataRange,
    reshuffleData,
    createDataParams,
    createDataParams_,
    createSubDataParams,
    createSubDataParams_,
    createSubDataParams__
    ) where

import Debug.Trace

import qualified Regression.Data as D
import qualified Regression.AnalyticData as AD
import qualified Regression.AnalyticDataWrapper as ADW
import Regression.Functions as FS
import Regression.Spline as S
import Regression.Utils as U
import Math.Expression as E


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

merge :: String -> DataParams -> DataParams -> DataParams
merge name dp1 dp2 = 
        DataParams {
            dataName = name, 
            dataDesc = name,
            dataSet = (dataSet dp1) ++ (dataSet dp2)
        }

applyToData :: (Int {- subdata no-} -> Maybe Int {- bootstrap no -} -> SubData -> (Double -> IO ()) -> IO [SubData]) 
        -> DataParams 
        -> [String] 
        -> TaskEnv
        -> IO [DataParams]
applyToData func dp names taskEnv = do
    let
        numSubSets = length $ dataSet dp
        numBootstrapSets = length $ subDataBootstrapSet $ head $ dataSet dp
        subSetNum = 1 + numBootstrapSets
        totalNum = numSubSets * (1 + numBootstrapSets)
        puFunc = progressUpdateFunc taskEnv 
        mapOp (i, sdp) = do
            results <- func i Nothing (subData sdp) (\pct -> puFunc ((fromIntegral (i * subSetNum) + pct) / fromIntegral totalNum))
            bootstrapResults <- calcConcurrently (\(j, d) puFunc -> 
                    func i (Just j) d (\pct -> puFunc ((fromIntegral (i * subSetNum) + pct * fromIntegral subSetNum) / fromIntegral totalNum))
                ) (progressUpdateFunc taskEnv) (taskInitializer taskEnv) (taskFinalizer taskEnv) (zip [0, 1 ..] (subDataBootstrapSet sdp))
            puFunc $ fromIntegral (i + 1) / fromIntegral numSubSets
            return (results, case transpose bootstrapResults of 
                [] -> replicate (length names) []
                xs -> xs
                )
    resultsPerSubData <- mapM mapOp (zip [0, 1 ..] (dataSet dp))
    let
        (results, bootstrapResults) = unzip resultsPerSubData 
    return $ zipWith3 (\name results bootstrapResults -> DataParams {
        dataName = name, 
        dataDesc = "",
        dataSet = zipWith3 (\sdp result bootstrapResults -> 
            SubDataParams {
                    subDataRange = subDataRange sdp,
                    subData = result, 
                    subDataBootstrapSet = bootstrapResults
                }) (dataSet dp) results bootstrapResults
        }) names (transpose results) (transpose bootstrapResults)

applyToData1 :: (Int -> Maybe Int -> SubData -> (Double -> IO ()) -> IO (SubData)) 
        -> DataParams 
        -> String 
        -> TaskEnv
        -> IO DataParams
applyToData1 func dp name taskEnv = 
    applyToData (\i j d pu -> func i j d pu >>= \result -> return [result]) dp [name] taskEnv >>= \[result] -> return result

calculateWeights :: DataParams -> Bool -> DataParams
calculateWeights dp dropBootstrapData = dp {dataSet = map mapOp (dataSet dp)} where
        mapOp sdp =
            case subData sdp of
                SD1 d ->
                    if length (subDataBootstrapSet sdp) > 0
                        then
                        let
                            vals = D.values1 d
                            bootstrap = (subDataBootstrapSet sdp)
                            bsData = map (\(SD1 sd) -> sd) bootstrap 
                            len = length bsData
                            vars = foldl1' (\ys1 ys2 -> V.zipWith (\y1 y2 -> y1 + y2) ys1 ys2) $ map (\bsd -> V.zipWith (\(_, yb, _) (_, y, _) -> (y - yb) * (y - yb) / fromIntegral len) (D.values1 bsd) vals) bsData
                            newValues = V.zipWith (\(x, y, _) var -> (x, y, 1 / var)) vals vars
                          in
                            sdp {subData = SD1 (D.data1 newValues), subDataBootstrapSet = if dropBootstrapData then [] else bootstrap}
                      else sdp
                otherwise -> sdp 

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
    -> [SubData]
    -> SubDataParams
createSubDataParams range dat bootstrapSet =
    SubDataParams {
        subDataRange = range,
        subData = dat,
        subDataBootstrapSet = bootstrapSet
    }

createSubDataParams_ :: SubData
    -> [SubData]
    -> SubDataParams
createSubDataParams_ dat = createSubDataParams (U.dataRange (unboxSubData dat)) dat

createSubDataParams__ :: SubData 
    -> SubDataParams
createSubDataParams__ dat = createSubDataParams (U.dataRange (unboxSubData dat)) dat []

