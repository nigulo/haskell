{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Bayes (linRegWithMLIIDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import qualified Regression.Bayes as B
import Regression.Data as D

import TSA.CommonParams
import TSA.RegressionParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log

import Utils.List

import GUI.Widget hiding (entryGetString)

import Control.Concurrent.MVar

linRegWithMLIIDialog :: StateRef -> IO ()
linRegWithMLIIDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = bayesLinRegParams (params state)
        commonParams = bayesLinRegCommonParams parms
    dialog <- dialogWithTitle state "Bayesian linear regression using MLII algorithm"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    algoCombo <- createComboBox ["ML-II"]
    comboBoxSetActive algoCombo (bayesLinRegAlgo parms)
    addWidgetToBox (Just "Algorithm: ") algoCombo contentBox

    methodCombo <- createComboBox ["RBF"]
    comboBoxSetActive methodCombo (bayesLinRegMethod parms)
    addWidgetToBox (Just "Method: ") methodCombo contentBox

    -----------------------------------------
    let
        rbfParams = head (bayesLinRegMethodParams parms)
    rbfNumCentresAdjustment <- Gtk.adjustmentNew (fromIntegral (rbfNumCentres rbfParams)) 1 (2**52) 1 1 1
    rbfNumCentresSpin <- Gtk.spinButtonNew (Just rbfNumCentresAdjustment) 1 0
    rbfNumCentresLabel <- addLabel "Num. centres: " contentBox
    Gtk.widgetSetHexpand rbfNumCentresSpin True
    Gtk.boxAppend contentBox rbfNumCentresSpin

    rbfNumLambdasAdjustment <- Gtk.adjustmentNew (fromIntegral (rbfNumLambdas rbfParams)) 1 (2**52) 1 1 1
    rbfNumLambdasSpin <- Gtk.spinButtonNew (Just rbfNumLambdasAdjustment) 1 0
    rbfNumLambdasLabel <- addLabel "Num. lambdas: " contentBox
    Gtk.widgetSetHexpand rbfNumLambdasSpin True
    Gtk.boxAppend contentBox rbfNumLambdasSpin
    -----------------------------------------

    dataSetCombo <- dataSetComboNew onlyData state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    let
        updateWidgets =
            do
                methodNo <- comboBoxGetActive methodCombo
                case methodNo of
                    0 -> do
                        Gtk.widgetSetVisible rbfNumCentresSpin True
                        Gtk.widgetSetVisible rbfNumCentresLabel True
                        Gtk.widgetSetVisible rbfNumLambdasSpin True
                        Gtk.widgetSetVisible rbfNumLambdasLabel True

        toggleFitButton =
            do
                selectedData <- getSelectedData dataSetCombo
                fitName <- entryGetString nameEntry
                sensitivity <-
                    case selectedData of
                        Just _ -> if length fitName <= 0 then return False
                                                    else return True
                        Nothing -> return False
                Gtk.widgetSetSensitive okButton sensitivity

    _ <- on (getComboBox dataSetCombo) #notify $ \_ -> toggleFitButton
    _ <- on nameEntry #changed toggleFitButton
    _ <- after dialog #realize updateWidgets
    _ <- on methodCombo #notify $ \_ -> updateWidgets

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $ do
        let
            getMethodParams 0 = do
                numCentres <- spinButtonGetValue rbfNumCentresSpin
                numLambdas <- spinButtonGetValue rbfNumLambdasSpin
                return RBFParams {
                    rbfNumCentres = round numCentres,
                    rbfNumLambdas = round numLambdas
                }

        name <- entryGetString nameEntry
        methodNo <- comboBoxGetActive methodCombo
        Just selectedData <- getSelectedData dataSetCombo
        Gtk.windowDestroy dialog
        methodParams <- getMethodParams methodNo

        modifyStateParams stateRef $ \params ->
            let
                bayesLinRegParms = bayesLinRegParams params
            in
                params {bayesLinRegParams = bayesLinRegParms {
                        bayesLinRegData = Just selectedData,
                        bayesLinRegAlgo = 0,
                        bayesLinRegMethod = methodNo,
                        bayesLinRegMethodParams = Utils.List.updateAt methodNo methodParams (bayesLinRegMethodParams bayesLinRegParms),
                        bayesLinRegCommonParams = updateCommonParams name commonParams
                    }}

        runTask stateRef "ML-II" $ linRegWithMLII stateRef methodNo name selectedData methodParams
        return ()

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowPresent dialog


linRegWithMLII :: StateRef -> Int -> String -> DataParams -> BayesLinRegMethodParams -> IO ()
linRegWithMLII stateRef method fitName dataParams (RBFParams numCentres numLambdas) = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        dat = dataSet dataParams
        opts = (50, 0.001)

        func i (SD1 dat) _ = do
            let
                minx = D.xMin1 dat
                maxx = D.xMax1 dat
                range = maxx - minx
                lambdaMin = range / 10
                lambdaMax = range
            (ad, varFunc) <- B.linRegWithMLII dat (B.MethodRBF numCentres [(minx, maxx)] [lambdaMin, lambdaMin + (lambdaMax - lambdaMin) / (fromIntegral numLambdas - 1) .. lambdaMax] opts)
            return $ SD4 ad
    result <- applyToData1 func dataParams fitName tEnv
    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))