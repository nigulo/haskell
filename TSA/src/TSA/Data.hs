module TSA.Data (
    ---------------
    isAnalytic,
    isContinuous,
    isDiscrete,
    ---------------
    merge,
    applyToData,
    applyToData1,
    calculateWeights,
    ---------------
    getDataType,
    getSubDataAt,
    dataRange,
    reshuffleData
    ) where

import Debug.Trace
import qualified Data.Map as M

import Regression.Data as D
import Regression.AnalyticData as AD
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

merge :: String -> DataParams -> DataParams -> DataParams
merge name dp1 dp2 = 
        DataParams {
            dataName = name, 
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
    return $ zipWith4 (\sdp name results bootstrapResults -> DataParams {
        dataName = name, 
        dataSet = zipWith (\result bootstrapResults -> 
            SubDataParams {
                    subDataRange = subDataRange sdp,
                    subData = result, 
                    subDataBootstrapSet = bootstrapResults
                }) results bootstrapResults
        }) (dataSet dp) names (transpose results) (transpose (trace ("bootstrapResults=" ++ show (transpose bootstrapResults)) bootstrapResults))

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
                            sdp {subData = Left (data1 newValues), subDataBootstrapSet = if dropBootstrapData then [] else bootstrap}
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

