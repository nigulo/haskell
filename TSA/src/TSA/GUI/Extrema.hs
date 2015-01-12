module TSA.GUI.Extrema (findExtremaDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.AnalyticData as AD
import Regression.Utils

import TSA.Params
import qualified TSA.Extrema
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


findExtremaDialog :: StateRef -> IO ()
findExtremaDialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = findExtremaParams (params state)
        commonParams = findExtremaCommonParams parms

    dialog <- dialogWithTitle state "Find extrema"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    precisionAdjustment <- adjustmentNew (fromIntegral (findExtremaPrecision parms)) 1 (2**52) 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetText nameEntry
                precision <- spinButtonGetValue precisionSpin
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {findExtremaParams = FindExtremaParams {
                        findExtremaData = Just selectedData,
                        findExtremaPrecision = round precision,
                        findExtremaCommonParams = updateCommonParams name commonParams
                    }}
                
                
                forkIO $ findExtrema stateRef selectedData (round precision) name
                return ()
        else
            do
                widgetDestroy dialog

findExtrema :: StateRef -> DataParams -> Int  -> String -> IO ()
findExtrema stateRef dataParams precision name = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

    (minima, maxima) <- TSA.Extrema.findExtrema dataParams precision name (progressUpdate stateRef)
    mapM_ (\dp -> modifyState stateRef $ addDataParams dp (Just (currentGraphTab, selectedGraph))) [minima, maxima]
    progressUpdate stateRef 0

