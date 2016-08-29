module TSA.GUI.Sample where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox
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

    dialog <- dialogWithTitle state "Sample data set"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    vBox <- castToBox <$> dialogGetContentArea dialog
    
    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog
    
    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidget (Just "Data to sample: ") (getComboBox dataSetCombo) dialog

    dataSetCombo2 <- dataSetComboNew2 dataAndSpectrum state False
    addWidget (Just "Sample with: ") (getComboBox dataSetCombo2) dialog

    countAdjustment <- adjustmentNew (fromIntegral (sampleCount parms)) 1 1000000 1 1 1
    countSpin <- spinButtonNew countAdjustment 1 0
    addWidget (Just "Count: ") countSpin dialog

    randomnessAdjustment <- adjustmentNew (fromIntegral (sampleRandomness parms)) 0 100 1 1 1
    randomnessSpin <- spinButtonNew randomnessAdjustment 1 0
    addWidget (Just "Randomness: ") randomnessSpin dialog

    dataTypeCombo <- createComboBox ["Data", "Spectrum"]
    if (sampleType parms) then comboBoxSetActive dataTypeCombo 0 else comboBoxSetActive dataTypeCombo 1 
    addWidget (Just "Type: ") dataTypeCombo dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                
                Just selectedData <- getSelectedData dataSetCombo
                selectedData2 <- getSelectedData dataSetCombo2
                count <- spinButtonGetValue countSpin
                randomness <- spinButtonGetValue randomnessSpin
                dataType <- comboBoxGetActive dataTypeCombo
                widgetDestroy dialog

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
                                    --D.xs d
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
                                otherwise -> D.data1' . V.fromList . map (\((x:_), y) -> (x, y)) $ sample 
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
        else
            do
                widgetDestroy dialog

    
