
module TSA.GUI.D2 (d2Dialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Debug.Trace

import Regression.Functions as FS
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils

import TSA.Params
import TSA.D2
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget

import Data.List
import Data.Maybe
import Utils.Misc
import Utils.Concurrent

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import System.IO
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

import System.Random
import System.Random.MWC

d2Dialog :: StateRef -> IO ()
d2Dialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = d2Params (params state)
        commonParams = d2CommonParams parms

    dialog <- dialogWithTitle state "Find period"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    dataSetCombo <- dataSetComboNew onlyData state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    periodStartAdjustment <- adjustmentNew (d2PeriodStart parms) 0 (2**52) 1 1 1
    periodStartSpin <- spinButtonNew periodStartAdjustment 1 10
    addWidget (Just "Period start: ") periodStartSpin dialog

    periodEndAdjustment <- adjustmentNew (d2PeriodEnd parms) 0 (2**52) 1 1 1
    periodEndSpin <- spinButtonNew periodEndAdjustment 1 10
    addWidget (Just "Period end: ") periodEndSpin dialog

    corrLenStartAdjustment <- adjustmentNew (d2CorrLenStart parms) 0 (2**52) 1 1 1
    corrLenStartSpin <- spinButtonNew corrLenStartAdjustment 1 10
    addWidget (Just "Correlation length start: ") corrLenStartSpin dialog

    corrLenEndAdjustment <- adjustmentNew (d2CorrLenEnd parms) 0 (2**52) 1 1 1
    corrLenEndSpin <- spinButtonNew corrLenEndAdjustment 1 10
    addWidget (Just "Correlation length end: ") corrLenEndSpin dialog
    
    methodCombo <- createComboBox ["Box", "Gauss"]
    comboBoxSetActive methodCombo (d2Method parms)
    addWidget (Just "Method: ") methodCombo dialog

    precisionAdjustment <- adjustmentNew (fromIntegral (d2Precision parms)) 1 (2**52) 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                Just selectedData <- getSelectedData dataSetCombo
                periodStart <- spinButtonGetValue periodStartSpin
                periodEnd <- spinButtonGetValue periodEndSpin
                corrLenStart <- spinButtonGetValue corrLenStartSpin
                corrLenEnd <- spinButtonGetValue corrLenEndSpin
                methodNo <- comboBoxGetActive methodCombo
                precision <- spinButtonGetValue precisionSpin
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {d2Params = D2Params {
                        d2Data = Just selectedData,
                        d2PeriodStart = periodStart,
                        d2PeriodEnd = periodEnd,
                        d2CorrLenStart = corrLenStart,
                        d2CorrLenEnd = corrLenEnd,
                        d2Method = methodNo,
                        d2Precision = round precision,
                        d2CommonParams = updateCommonParams name commonParams
                    }}
                
                forkIO $ d2 stateRef selectedData periodStart periodEnd corrLenStart corrLenEnd methodNo (round precision) name
                return ()
        else
            do
                widgetDestroy dialog

d2 :: StateRef -> DataParams -> Double -> Double -> Double -> Double -> Int -> Int -> String-> IO ()
d2 stateRef dataParams periodStart periodEnd minCorrLen maxCorrLen method precision name = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    dispersions <- calcDispersions dataParams periodStart periodEnd minCorrLen maxCorrLen method precision name False (taskEnv stateRef)
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
    if length dispersions > 0 
        then
            do
                mapM_ (\(corrLen, dp) -> do
                    modifyState stateRef $ addDataParams dp (Just (currentGraphTab, selectedGraph))
                    let 
                        sdp = head $ dataSet dp
                        (Left d) = subData sdp
                        vals = D.xys1 d
                        yMin = D.yMin d
                        yMax = D.yMax d
                        yRange = yMax - yMin
                        toStr (x, y) = show corrLen ++ " " ++ show x ++ " " ++ show ((y - yMin) / yRange) ++ "\n"
                        --toStr (x, y) = show corrLen ++ " " ++ show x ++ " " ++ show y ++ "\n"
                        str = concatMap (toStr) (V.toList vals)
                    handle <- openFile ("phasedisp.csv") AppendMode
                    hPutStr handle (str ++ "\n")
                    hClose handle
                    ) dispersions
        else 
            return ()
    progressUpdate stateRef 0
