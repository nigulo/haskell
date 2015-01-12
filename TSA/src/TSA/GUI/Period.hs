
module TSA.GUI.Period (findPeriodDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Debug.Trace

import Regression.Functions as FS
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils

import TSA.Params
import TSA.Period
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

findPeriodDialog :: StateRef -> IO ()
findPeriodDialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = findPeriodParams (params state)
        commonParams = findPeriodCommonParams parms

    dialog <- dialogWithTitle state "Find period"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    dataSetCombo <- dataSetComboNew onlyData state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    methodCombo <- createComboBox ["Least squares", "String length"]
    comboBoxSetActive methodCombo (findPeriodMethod parms)
    addWidget (Just "Method: ") methodCombo dialog

    periodStartAdjustment <- adjustmentNew (findPeriodStart parms) 0 (2**52) 1 1 1
    periodStartSpin <- spinButtonNew periodStartAdjustment 1 10
    addWidget (Just "Period start: ") periodStartSpin dialog

    periodEndAdjustment <- adjustmentNew (findPeriodEnd parms) 0 (2**52) 1 1 1
    periodEndSpin <- spinButtonNew periodEndAdjustment 1 10
    addWidget (Just "Period end: ") periodEndSpin dialog

    precisionAdjustment <- adjustmentNew (fromIntegral (findPeriodPrecision parms)) 1 (2**52) 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetText nameEntry
                Just selectedData <- getSelectedData dataSetCombo
                methodNo <- comboBoxGetActive methodCombo
                periodStart <- spinButtonGetValue periodStartSpin
                periodEnd <- spinButtonGetValue periodEndSpin
                precision <- spinButtonGetValue precisionSpin
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {findPeriodParams = FindPeriodParams {
                        findPeriodData = Just selectedData,
                        findPeriodMethod = methodNo,
                        findPeriodStart = periodStart,
                        findPeriodEnd = periodEnd,
                        findPeriodPrecision = round precision,
                        findPeriodCommonParams = updateCommonParams name commonParams
                    }}
                
                forkIO $ findPeriod stateRef selectedData periodStart periodEnd (round precision) methodNo name
                return ()
        else
            do
                widgetDestroy dialog

findPeriod :: StateRef -> DataParams -> Double -> Double -> Int -> Int -> String-> IO ()
findPeriod stateRef dataParams periodStart periodEnd precision method name = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    dispersions <- calcDispersions dataParams periodStart periodEnd precision method name False (progressUpdate stateRef) (appendLog stateRef)
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
