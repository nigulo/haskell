
module TSA.GUI.AnalyticSignal (analyticSignalDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import qualified Regression.Polynom as P
import Regression.AnalyticData as AD
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils
import Regression.FFT
import qualified Math.Function as F

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import qualified TSA.AnalyticSignal as AS

import GUI.Widget
import Utils.Misc

import Data.IORef
import Data.Maybe
import Data.Complex
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import Debug.Trace

import Math.Expression
import System.Random
import qualified Data.Vector.Unboxed as V

analyticSignalDialog :: StateRef -> IO ()
analyticSignalDialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = asParams (params state)
        amplitudeParams = asAmplitudeParams parms
        phaseParams = asPhaseParams parms
        frequencyParams = asFrequencyParams parms
    dialog <- dialogWithTitle state "Analytic signal"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    amplitudeEntry <- entryNew
    amplitudeEntry `entrySetText` (getNameWithNo amplitudeParams)
    addWidget (Just "Amplitude name: ") amplitudeEntry dialog

    phaseEntry <- entryNew
    phaseEntry `entrySetText` (getNameWithNo phaseParams)
    addWidget (Just "Phase name: ") phaseEntry dialog

    frequencyEntry <- entryNew
    frequencyEntry `entrySetText` (getNameWithNo frequencyParams)
    addWidget (Just "Frequency name: ") frequencyEntry dialog
    
    realCombo <- dataSetComboNew (\_ -> True) state
    addWidget (Just "Real signal: ") (getComboBox realCombo) dialog

    conjugatedCombo <- dataSetComboNew2 (\_ -> True) state False
    addWidget (Just "Conjugated signal: ") (getComboBox conjugatedCombo) dialog

    precisionAdjustment <- adjustmentNew 65536 1 1048576 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                amplitudeName <- entryGetString amplitudeEntry
                phaseName <- entryGetString phaseEntry
                frequencyName <- entryGetString frequencyEntry
                                
                Just realData <- getSelectedData realCombo
                conjugatedData <- getSelectedData conjugatedCombo
                precision <- spinButtonGetValue precisionSpin
                widgetDestroy dialog

                modifyStateParams stateRef $ \params -> params {asParams = AnalyticSignalParams {
                    asAmplitudeParams = updateCommonParams amplitudeName amplitudeParams,
                    asPhaseParams = updateCommonParams phaseName phaseParams,
                    asFrequencyParams = updateCommonParams frequencyName frequencyParams,
                    asRealData = Just realData,
                    asImagData = conjugatedData                    
                }}
                
                forkIO $ analyticSignal stateRef (round precision) (amplitudeName, phaseName, frequencyName)
                return ()
                
        else
            do
                widgetDestroy dialog

analyticSignal :: StateRef -> Int -> (String, String, String) -> IO ()
analyticSignal stateRef precision (amplitudeName, phaseName, frequencyName) = 
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms
            asParms = asParams (params state)
            dataParams = fromJust (asRealData asParms)
            
        AS.analyticSignal asParms precision (amplitudeName, phaseName, frequencyName, ((dataName dataParams) ++ "_conj")) (taskEnv stateRef) (DataUpdateFunc (\dat name update -> modifyState stateRef $ addOrUpdateData dat name (Just (currentGraphTab, selectedGraph)) update)) 
        return ()            
