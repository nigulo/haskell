
module TSA.GUI.Envelopes where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U
import qualified Math.Function as F

import TSA.Params
import TSA.Data
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget
import qualified TSA.Envelopes as E

import Utils.Misc

import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent
import System.Random

import Control.Applicative

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = envParams (params state)
        upperParams = envUpperParams parms
        lowerParams = envLowerParams parms
        meanParams = envMeanParams parms
        
    dialog <- dialogWithTitle state "Envelopes"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    label <- labelNew $ Just "Upper envelope"
    addWidget Nothing label dialog
    upperFitWidgets@(FitWidgets upperNameEntry upperRankSpin upperPeriod upperHarmonics _ _ _ _) <- addFitWidgets upperParams state dialog
    upperPeriod `widgetSetSensitivity` False

    label <- labelNew $ Just "Lower envelope"
    addWidget Nothing label dialog
    lowerFitWidgets@(FitWidgets lowerNameEntry lowerRankSpin lowerPeriod lowerHarmonics _ _ _ _) <- addFitWidgets lowerParams state dialog
    lowerPeriod `widgetSetSensitivity` False


    sep <- hSeparatorNew
    addWidget Nothing sep dialog
    precisionAdjustment <- adjustmentNew (envPrecision parms) 1 100000 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    extremaCombo <- createComboBox ["Strict", "Statistical"]
    case envExtrema parms of
        EnvExtremaStrict -> comboBoxSetActive extremaCombo 0 
        EnvExtremaStatistical -> comboBoxSetActive extremaCombo 1
    addWidget (Just "Extrema detection: ") extremaCombo dialog

    meanNameEntry <- entryNew
    meanNameEntry `entrySetText` (getNameWithNo meanParams)
    addWidget (Just "Mean envelope name: ") meanNameEntry dialog
    
    dataSetCombo <- dataSetComboNew dataAndSpectrum state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    let 
        toggleFitButton :: IO ()
        toggleFitButton = 
            do
                selectedData <- getSelectedData dataSetCombo
                upperName <- entryGetString upperNameEntry
                lowerName <- entryGetString lowerNameEntry
                meanName <- entryGetString meanNameEntry
                sensitivity <-
                    case selectedData of 
                        Just _ -> if length upperName <= 0 
                                      || length lowerName <= 0
                                      || length meanName <= 0
                                    then return False 
                                    else return True
                        Nothing -> return False
                fitButton `widgetSetSensitivity` sensitivity
                        
    on (getComboBox dataSetCombo) changed toggleFitButton
    --nameEntry `onButtonRelease` (\e -> toggleFitButton >> return False)
    --nameEntry `onKeyRelease` (\e -> toggleFitButton >> return False)
    on (castToEditable upperNameEntry) editableChanged toggleFitButton
    on (castToEditable lowerNameEntry) editableChanged toggleFitButton
    on (castToEditable meanNameEntry) editableChanged toggleFitButton
--    nameEntry `afterInsertAtCursor` (\s -> toggleFitButton)
--    nameEntry `afterPasteClipboard` (toggleFitButton)
--    nameEntry `afterCutClipboard` (toggleFitButton)
    
    
    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                newUpperParms <- getFitParams upperFitWidgets upperParams
                newLowerParms <- getFitParams lowerFitWidgets lowerParams

                precision <- spinButtonGetValue precisionSpin
                Just extrema <- comboBoxGetActiveString extremaCombo
                
                upperName <- entryGetString upperNameEntry
                lowerName <- entryGetString lowerNameEntry
                meanName <- entryGetString meanNameEntry
                
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog

                let
                    newEnvParams = EnvParams {
                            envUpperParams = newUpperParms,
                            envLowerParams = newLowerParms,
                            envPrecision = precision,
                            envExtrema = (if extrema == "Strict" then EnvExtremaStrict else EnvExtremaStatistical),
                            envMeanParams = updateCommonParams meanName meanParams,
                            envData = Just selectedData
                        } 

                modifyStateParams stateRef $ \params -> params {envParams = newEnvParams}
                runTask stateRef "Find envelopes" $ (envelopes stateRef (upperName, lowerName, meanName))
                return ()
        else
            do
                widgetDestroy dialog

envelopes :: StateRef -> (String, String, String) -> IO ()
envelopes stateRef (upperName, lowerName, meanName) =
    do
        state <- readMVar stateRef
        g <- getStdGen 
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            parms = envParams (params state)
        tEnv <- taskEnv stateRef
        
        E.envelopes parms (upperName, lowerName, meanName) tEnv (DataUpdateFunc (\dat name update -> modifyState stateRef $ addOrUpdateData dat name (Just (currentGraphTab, selectedGraph)) update))
        return ()