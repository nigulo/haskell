{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Sample where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import qualified Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.AnalyticDataWrapper as ADW
import Regression.Data as D
import Regression.Utils as U
import qualified Math.Function as F
import qualified Math.Expression as E
import qualified Data.Vector.Unboxed as V

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

import Utils.Misc
import Utils.Concurrent

import Data.IORef
import qualified Data.Map as M
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random
import Debug.Trace

sampleDialog :: StateRef -> IO ()
sampleDialog stateRef = do
    state <- readMVar stateRef

    let
        parms = sampleParams (params state)
        commonParams = sampleCommonParams parms

    g <- newStdGen
    (currentGraphTab, _) <- getCurrentGraphTab state

    win <- dialogWithTitle state "Sample data set"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidgetToBox (Just "Data to sample: ") (getComboBox dataSetCombo) contentBox

    dataSetCombo2 <- dataSetComboNew2 dataAndSpectrum state False
    addWidgetToBox (Just "Sample with: ") (getComboBox dataSetCombo2) contentBox

    countAdjustment <- Gtk.adjustmentNew (fromIntegral (sampleCount parms)) 1 1000000 1 1 1
    countSpin <- Gtk.spinButtonNew (Just countAdjustment) 1 0
    addWidgetToBox (Just "Count: ") countSpin contentBox

    randomnessAdjustment <- Gtk.adjustmentNew (fromIntegral (sampleRandomness parms)) 0 100 1 1 1
    randomnessSpin <- Gtk.spinButtonNew (Just randomnessAdjustment) 1 0
    addWidgetToBox (Just "Randomness: ") randomnessSpin contentBox

    dataTypeCombo <- createComboBox ["Data", "Spectrum"]
    if (sampleType parms) then comboBoxSetActive dataTypeCombo 0 else comboBoxSetActive dataTypeCombo 1
    addWidgetToBox (Just "Type: ") dataTypeCombo contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy win

    _ <- Gtk.onButtonClicked okButton $ do
        name <- entryGetString nameEntry

        Just selectedData <- getSelectedData dataSetCombo
        selectedData2 <- getSelectedData dataSetCombo2
        count <- spinButtonGetValue countSpin
        randomness <- spinButtonGetValue randomnessSpin
        dataType <- comboBoxGetActive dataTypeCombo
        Gtk.windowDestroy win

        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms
            (xMins, xMaxs) = unzip $ map (\sdp ->
                    case unboxSubData $ subData sdp of
                        Left d -> (D.xMins d, D.xMaxs d)
                        Right ad -> (ADW.xMins ad, ADW.xMaxs ad)
                ) (dataSet selectedData)

            xs =
                case selectedData2 of -- only unsegmented data
                    Just dat ->
                        let
                            SD1 d = subData $ head $ dataSet dat
                        in
                            filter (\xs -> and (zipWith (>=) xs (head xMins)) && and (zipWith (<=) xs (head xMaxs))) (D.xs d)
                    Nothing ->
                        let
                            getXs xMin xMax =
                                let
                                    avgStep = (xMax - xMin) / count
                                in
                                    map (\(i, r) -> xMin + avgStep * ((fromIntegral i)  + r * randomness / 100)) (zip [0, 1 ..] (take (round count) (randomRs (0, 1) g)))
                            xMin = minimum xMins
                            xMax = maximum xMaxs
                        in
                            sequence $ zipWith (\xMin xMax -> getXs xMin xMax) xMin xMax
        samples <- calcConcurrently_ (\d -> return (U.getValues xs d g)) (map (\sdp -> unboxSubData (subData sdp)) (dataSet selectedData))
        let
            dataCreateFunc sample = if dataType == 0
                then
                    case head sample of
                        ((x1:x2:_), y) -> D.data2' . V.fromList . map (\((x1:x2:_), y) -> (x1, x2, y)) $ sample
                        _ -> D.data1' . V.fromList . map (\((x:_), y) -> (x, y)) $ sample
                else
                    D.spectrum1' . V.fromList . map (\((x:_), y) -> (x, y)) $ sample
            subDataParams = map (\sample -> createSubDataParams_ (SD1 (dataCreateFunc sample))) samples

        modifyState stateRef $ addDataParams (createDataParams_ name subDataParams) (Just (currentGraphTab, selectedGraph))

        modifyStateParams stateRef $ \params -> params {sampleParams = SampleParams {
                sampleCommonParams = updateCommonParams name commonParams,
                sampleCount = round count,
                sampleRandomness = round randomness,
                sampleType = if dataType == 0 then True else False
            }}
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win
