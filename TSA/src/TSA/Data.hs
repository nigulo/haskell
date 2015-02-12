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
import qualified Data.Map as M

import qualified Regression.Data as D
import qualified Regression.AnalyticData as AD
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
             Left _ -> False
             Right _ -> True

isContinuous :: DataParams -> Bool
isContinuous = isAnalytic

isDiscrete :: DataParams -> Bool
isDiscrete dp =
    case subData (head (dataSet dp)) of 
             Left _ -> True
             Right _ -> False

is2d :: DataParams -> Bool
is2d dp =
    case subData (head (dataSet dp)) of 
             Left d -> D.is2d d
             Right (Left s) -> AD.is2d s
             Right (Right f) -> AD.is2d f

merge :: String -> DataParams -> DataParams -> DataParams
merge name dp1 dp2 = 
        DataParams {
            dataName = name, 
            dataDesc = name,
            dataSet = (dataSet dp1) ++ (dataSet dp2)
        }

applyToData :: (Int {- subdata no-} -> Maybe Int {- bootstrap no -} -> Either D.Data (Either S.Spline FS.Functions) -> (Double -> IO ()) -> IO [Either D.Data (Either S.Spline FS.Functions)]) 
        -> DataParams 
        -> [String] 
        -> (Double -> IO ()) -- ^ progressUpdate func
        -> IO [DataParams]
applyToData func dp names puFunc = do
    let
        numSubSets = length $ dataSet dp
        numBootstrapSets = length $ subDataBootstrapSet $ head $ dataSet dp
        subSetNum = 1 + numBootstrapSets
        totalNum = numSubSets * (1 + numBootstrapSets) 
        mapOp (i, sdp) = do
            results <- func i Nothing (subData sdp) (\pct -> puFunc ((fromIntegral (i * subSetNum) + pct) / fromIntegral totalNum))
            bootstrapResults <- calcConcurrently (\(j, d) puFunc -> func i (Just j) d (\pct -> puFunc ((fromIntegral (i * subSetNum) + pct * fromIntegral subSetNum) / fromIntegral totalNum))) puFunc (zip [0, 1 ..] (subDataBootstrapSet sdp))
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

applyToData1 :: (Int -> Maybe Int -> Either D.Data (Either S.Spline FS.Functions) -> (Double -> IO ()) -> IO (Either D.Data (Either S.Spline FS.Functions))) 
        -> DataParams 
        -> String 
        -> (Double -> IO ()) -- ^ progressUpdate func
        -> IO DataParams
applyToData1 func dp name puFunc = applyToData (\i j d pu -> func i j d pu >>= \result -> return [result]) dp [name] puFunc >>= \[result] -> return result

calculateWeights :: DataParams -> Bool -> DataParams
calculateWeights dp dropBootstrapData = dp {dataSet = map mapOp (dataSet dp)} where
        mapOp sdp =
            case subData sdp of
                Left d ->
                    if length (subDataBootstrapSet sdp) > 0
                        then
                        let
                            vals = D.values1 d
                            bootstrap = (subDataBootstrapSet sdp)
                            bsData = lefts bootstrap 
                            len = length bsData
                            vars = foldl1' (\ys1 ys2 -> V.zipWith (\y1 y2 -> y1 + y2) ys1 ys2) $ map (\bsd -> V.zipWith (\(_, yb, _) (_, y, _) -> (y - yb) * (y - yb) / fromIntegral len) (D.values1 bsd) vals) bsData
                            newValues = V.zipWith (\(x, y, _) var -> (x, y, 1 / var)) vals vars
                          in
                            sdp {subData = Left (D.data1 newValues), subDataBootstrapSet = if dropBootstrapData then [] else bootstrap}
                      else sdp
                otherwise -> sdp 

getDataType :: DataParams -> String
getDataType dp =
    case subData (head (dataSet dp)) of 
        Left d -> 
            if D.isData d then "Data" else "Spectrum"
        Right (Left _) -> "Spline"
        Right (Right _) -> "Function"

getSubDataAt :: DataParams -> Int -> Either D.Data (Either S.Spline FS.Functions)
getSubDataAt dp i = subData (dataSet dp !! i)

createDataParams :: String -> String -> [SubDataParams] -> DataParams
createDataParams name desc dat =
    DataParams {
        dataName = name,
        dataDesc = desc, 
        dataSet = dat
    }

createDataParams_ :: String -> [SubDataParams] -> DataParams
createDataParams_ name dat = createDataParams name name dat

createSubDataParams :: ([Double], [Double]) 
    -> Either D.Data (Either S.Spline FS.Functions) 
    -> [Either D.Data (Either S.Spline FS.Functions)]
    -> SubDataParams
createSubDataParams range dat bootstrapSet =
    SubDataParams {
        subDataRange = range,
        subData = dat,
        subDataBootstrapSet = bootstrapSet
    }

createSubDataParams_ :: Either D.Data (Either S.Spline FS.Functions) 
    -> [Either D.Data (Either S.Spline FS.Functions)]
    -> SubDataParams
createSubDataParams_ dat bootstrapSet = createSubDataParams (U.dataRange dat) dat bootstrapSet

createSubDataParams__ :: Either D.Data (Either S.Spline FS.Functions) 
    -> SubDataParams
createSubDataParams__ dat = createSubDataParams (U.dataRange dat) dat []

