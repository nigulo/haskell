module TSA.GUI.SpecificPoints (findSpecificPointsDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.AnalyticData as AD
import Regression.Utils

import TSA.Params
import qualified TSA.SpecificPoints
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget

import Data.List
import Data.Maybe
import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample


findSpecificPointsDialog :: StateRef -> IO ()
findSpecificPointsDialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = specificPointsParams (params state)
        commonParams = specificPointsCommonParams parms

    dialog <- dialogWithTitle state "Find specific points"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    typeCombo <- createComboBox ["Extrema", "Zero-crossings"]
    typeCombo `comboBoxSetActive` (specificPointsType parms)
    addWidget (Just "Type: ") typeCombo dialog
    
    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    precisionAdjustment <- adjustmentNew (fromIntegral (specificPointsPrecision parms)) 1 (2**52) 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                spType <- comboBoxGetActive typeCombo
                precision <- spinButtonGetValue precisionSpin
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {specificPointsParams = SpecificPointsParams {
                        specificPointsData = Just selectedData,
                        specificPointsType = spType,
                        specificPointsPrecision = round precision,
                        specificPointsCommonParams = updateCommonParams name commonParams
                    }}
                
                
                forkIO $ findSpecificPoints stateRef selectedData (round precision) name spType
                return ()
        else
            do
                widgetDestroy dialog

findSpecificPoints :: StateRef -> DataParams -> Int -> String -> Int -> IO ()
findSpecificPoints stateRef dataParams precision name spType = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

    results <-
        case spType of
            0 -> do
                (minima, maxima) <- TSA.SpecificPoints.findExtrema dataParams precision name (progressUpdate stateRef)
                return [minima, maxima]
            1 -> do
                zc <- TSA.SpecificPoints.findZeroCrossings dataParams precision name (progressUpdate stateRef)
                return [zc]
    mapM_ (\dp -> modifyState stateRef $ addDataParams dp (Just (currentGraphTab, selectedGraph))) results
    progressUpdate stateRef 0

