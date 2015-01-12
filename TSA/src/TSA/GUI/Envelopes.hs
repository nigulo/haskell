
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
import GUI.Widget

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
                upperName <- entryGetText upperNameEntry
                lowerName <- entryGetText lowerNameEntry
                meanName <- entryGetText meanNameEntry
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
                Just extrema <- comboBoxGetActiveText extremaCombo
                
                meanName <- entryGetText meanNameEntry
                
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog

                modifyStateParams stateRef $ \params -> params {envParams = EnvParams {
                        envUpperParams = newUpperParms,
                        envLowerParams = newLowerParms,
                        envPrecision = precision,
                        envExtrema = (if extrema == "Strict" then EnvExtremaStrict else EnvExtremaStatistical),
                        envMeanParams = updateCommonParams meanName meanParams,
                        envData = Just selectedData
                    }}
                forkIO (envelopes stateRef)
                return ()
        else
            do
                widgetDestroy dialog

envelopes :: StateRef -> IO ()
envelopes stateRef =
    do
        state <- readMVar stateRef
        g <- getStdGen 
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            parms = envParams (params state)
            upperParams = envUpperParams parms
            lowerParams = envLowerParams parms
            Just dataParms = envData parms
            sdp = head $ dataSet $ dataParms
            Left dat = subData sdp
            
        upperSpline <- fitWithSpline_ 
            (fitPolynomRank upperParams) 
            (splineNumNodes (fitSplineParams upperParams)) 
            dat
            False
            2
            (progressUpdate stateRef)
        let
            upperSdev = U.stdev dat (Right (Left upperSpline))

            --lowerSpline = fitWithSpline3_ 
            --    (fitPolynomRank upperParms) 
            --    (fitNumKnots upperParms) 
            --    dat;
            --lowerSdev = R.stdev dat lowerSpline;

            strictExtremaDetection = case envExtrema parms of
                EnvExtremaStrict -> True
                _ -> False
                
        upperEnv <- envelope 
            True 
            (fitPolynomRank upperParams) 
            (splineNumNodes (fitSplineParams upperParams))
            (upperSdev / envPrecision parms) 
            strictExtremaDetection
            dat
            (\_ -> return ())
            (\spline -> 
                    modifyState stateRef $ addOrUpdateData (Right (Left spline)) ((commonName . fitCommonParams) upperParams) (Just (currentGraphTab, selectedGraph)) True
               --modifyState stateRef $ \state -> 
               --    let dataParms = findDataByName ((commonName . fitCommonParams) upperParams) state
               --    in case dataParms of 
               --        Just dataParms -> updateData (dataParms {dataSet = [SubDataParams {subData = Right (Left spline), subDataBootstrapSet = []}]}) state
               --        Nothing -> addSpline spline ((commonName . fitCommonParams) upperParams) (Just (currentGraphTab, selectedGraph)) state
               )
            (\dat -> 
                    modifyState stateRef $ addOrUpdateData (Left dat) (((commonName . fitCommonParams) upperParams) ++ "_") (Just (currentGraphTab, selectedGraph)) True
                --modifyState stateRef $ \state -> 
                --    let dataParms = findDataByName (((commonName . fitCommonParams) upperParams) ++ "_") state
                --    in case dataParms of 
                --        Just dataParms -> updateData (dataParms {dataSet = [SubDataParams {subData = Left dat, subDataBootstrapSet = []}]}) state
                --        Nothing -> addDiscreteData dat (((commonName . fitCommonParams) upperParams) ++ "_") (Just (currentGraphTab, selectedGraph)) state
                )
            
        lowerEnv <- envelope 
            False 
            (fitPolynomRank lowerParams) 
            (splineNumNodes (fitSplineParams lowerParams)) 
            (upperSdev / envPrecision parms) 
            strictExtremaDetection
            dat
            (\_ -> return ())
            (\spline -> 
                    modifyState stateRef $ addOrUpdateData (Right (Left spline)) ((commonName . fitCommonParams) lowerParams) (Just (currentGraphTab, selectedGraph)) True
                --modifyState stateRef $ \state -> 
                --    let dataParms = findDataByName ((commonName . fitCommonParams) lowerParams) state
                --    in case dataParms of 
                --        Just dataParms -> updateData (dataParms {dataSet = [SubDataParams {subData = Right (Left spline), subDataBootstrapSet = []}]}) state
                --        Nothing -> addSpline spline ((commonName . fitCommonParams) lowerParams) (Just (currentGraphTab, selectedGraph)) state
                )
            (\dat -> 
                    modifyState stateRef $ addOrUpdateData (Left dat) (((commonName . fitCommonParams) lowerParams) ++ "_") (Just (currentGraphTab, selectedGraph)) True
                --modifyState stateRef $ \state -> 
                --    let dataParms = findDataByName (((commonName . fitCommonParams) lowerParams) ++ "_") state
                --    in case dataParms of 
                --        Just dataParms -> updateData (dataParms {dataSet = [SubDataParams {subData = Left dat, subDataBootstrapSet = []}]}) state
                --        Nothing -> addDiscreteData dat (((commonName . fitCommonParams) lowerParams) ++ "_") (Just (currentGraphTab, selectedGraph)) state
                )
            
        --modifyState stateRef $ addSpline lowerEnv ((commonName . fitCommonParams) lowerParams) (Just (currentGraphTab, selectedGraph))
        --modifyState stateRef $ addSpline upperEnv ((commonName . fitCommonParams) upperParams) (Just (currentGraphTab, selectedGraph))
        
        let envMean = (upperEnv `S.add` lowerEnv) `S.divide` 2
        modifyState stateRef $ addSpline envMean ((commonName . envMeanParams) parms) (Just (currentGraphTab, selectedGraph))
        let dataOrSpline = U.binaryOp (F.subtr) (Left dat) (Right (Left envMean)) True g
        case dataOrSpline of
            Left d -> modifyState stateRef $ addDiscreteData d ((dataName dataParms) ++ "_r") (Just (currentGraphTab, selectedGraph))
            Right (Left s) -> modifyState stateRef $ addSpline s ((dataName dataParms) ++ "_r") (Just (currentGraphTab, selectedGraph))
        
        --let envMean = U.constantOp (U.binaryOp (Left (U.splineToSpectrum upperEnv 1024)) (Left (U.splineToSpectrum lowerEnv 1024)) (+) True g) 0.5 (*) True
                    
        --modifyMVar_ stateRef $ \state -> return $ addData (envMean) (envMeanName parms) (Just (currentGraphTab, selectedGraph)) state

        
        