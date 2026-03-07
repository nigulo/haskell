{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.LeastSquares  (paramsDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T
--import qualified Data.Map as Map
--import Data.IORef
--import Control.Concurrent
import Debug.Trace

import Math.Function as F
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U
import Regression.AnalyticData as AD
import Regression.Bootstrap as B

import TSA.CommonParams
import TSA.RegressionParams
import TSA.Params
import TSA.LeastSquares
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log

import GUI.Widget

import Utils.Misc
import Utils.Xml

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression
import System.Random

import qualified Data.Vector.Unboxed as V
import Statistics.Test.KolmogorovSmirnov
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Types
import qualified Statistics.Sample as Sample

import qualified Utils.Xml as Xml

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = lsqParams (params state)
        fitParams = lsqFitParams parms
        commonParams = fitCommonParams fitParams
    win <- dialogWithTitle state "Least squares fit"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8

    fitWidgets@(FitWidgets nameEntry rankSpin periodSpin harmonicsSpin typeCombo _ _ _) <- addFitWidgets fitParams state contentBox

    dataSetCombo <- dataSetComboNew dataAndSpectrum state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    bootstrapCountAdjustment <- Gtk.adjustmentNew (fromIntegral (lsqBootstrapCount parms)) 0 1000 1 1 1
    bootstrapCountSpin <- Gtk.spinButtonNew (Just bootstrapCountAdjustment) 1 0
    addWidgetToBox (Just "Bootstrap count: ") bootstrapCountSpin contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    Gtk.onButtonClicked cancelButton $ do
        Gtk.windowDestroy win

    Gtk.onButtonClicked okButton $ do
        Just selectedData <- getSelectedData dataSetCombo
        bootstrapCount <- spinButtonGetValue bootstrapCountSpin

        newFitParams <- getFitParams fitWidgets fitParams

        name <- entryGetString nameEntry

        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {lsqParams = LsqParams {
            lsqFitParams = newFitParams,
            lsqBootstrapCount = round bootstrapCount
        }}
        runTask stateRef "Least squares fit" $ fit stateRef selectedData name
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win


fit :: StateRef -> DataParams -> String -> IO ()
fit stateRef dataParams fitName = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

        lsqParms = lsqParams (params state)
        bootstrapCount = lsqBootstrapCount lsqParms

        func i (SD1 dat) puFunc = do
            spline <- fitData (lsqFitParams lsqParms) dat tEnv
            g <- getStdGen
            let
                SD1 diff = subDataBinaryOp (F.subtr) (SD1 dat) (SD2 spline) True g
                SD1 squareDiff = subDataBinaryOp (F.subtr) (SD1 diff) (SD1 diff) True g
                diffVals = D.values1 diff
                datVals = D.values1 dat
                datMean = Sample.meanWeighted (V.map (\(x, y, w) -> (y, w)) datVals)
                datVar = V.sum $ V.map (\(x, y, w) -> (y - datMean) ^ 2) datVals
                diffVar = V.sum $ V.map (\(x, y, w) -> y * y) diffVals
                n = fromIntegral $ D.dataLength dat
                bic = n * Prelude.log (diffVar / (n - 1)) + degFreedom * Prelude.log n
                r2 = 1 - diffVar / datVar
                degFreedom = fromIntegral $ getDegreesOfFreedom (lsqFitParams lsqParms)
                redChiSquared = (V.sum $ V.map (\(x, y, w) -> y * y * w) diffVals) / (n - degFreedom - 1)
                diffSample = D.ys diff
                normal = normalFromSample diffSample
                --dist = D.data1 $ V.map (\(x, y) -> (x, y, 1)) (cumulProbDist_ diffSample)
            appendLog stateRef ("Results for " ++ fitName ++ " " ++ show i ++ ":")
            appendLog stateRef ("stdev residuals = " ++ (show (stdDev normal)))
            appendLog stateRef ("num parameters = " ++ (show degFreedom))
            appendLog stateRef ("chi-squared = " ++ (show redChiSquared))
            appendLog stateRef ("R^2 = " ++ show r2)
            appendLog stateRef ("BIC = " ++ show bic)
            --appendLog stateRef ("KS statistic D = " ++ (show (kolmogorovSmirnovD normal diffSample)))
            --modifyState stateRef $ addData (Left dist) (fitName ++ "_residueDist") (Just (currentGraphTab, selectedGraph))
            return $ SD2 spline
    fitDataParams <- applyToData1 func dataParams fitName tEnv
    modifyState stateRef $ (addDataParams fitDataParams (Just (currentGraphTab, selectedGraph)))

    if bootstrapCount > 0
        then do
            Xml.renderToFile (Xml.toDocument (lsqFitParams lsqParms)) ("fitdata")
            mapM_ (\(i, fitParams, dataParams)  ->
                    do
                        let
                            SD2 spline = subData fitParams
                            SD1 dat = subData dataParams
                        Xml.renderToFile (Xml.toDocument spline) ("spline" ++ show i)
                        Xml.renderToFile (Xml.toDocument dat) ("data" ++ show i)
                ) (zip3 [1, 2 ..] (dataSet fitDataParams) (dataSet dataParams))

{-
            let
                numSubData = length $ dataSet dataParams
            subDataParams <- mapM (\(i, fitParams, dataParams)  ->
                    do
                        let
                            Right (Left spline) = subData fitParams
                            Left dat = subData dataParams
                        Xml.renderToFile (Xml.toDocument spline) ("spline" ++ show i)
                        Xml.renderToFile (Xml.toDocument dat) ("data" ++ show i)
                        bsSplines <- B.bootstrapSplines bootstrapCount (fitData (lsqFitParams lsqParms)) spline dat (\pct -> (progressUpdateFunc tEnv) (pct * fromIntegral i / fromIntegral numSubData))
                        --mapM (\(j, bsSpline) ->
                        --        modifyState stateRef $ addDataParams (DataParams {
                        --            dataName = fitName ++ "_b" ++ show j,
                        --            dataSet = [
                        --                SubDataParams {
                        --                    subData = Right (Left bsSpline),
                        --                    subDataBootstrapSet = []
                        --                }
                        --            ]
                        --        }) (Just (currentGraphTab, selectedGraph))
                        --    ) (zip [1 ..] bsSplines)
                        let
                            (upperSpline, lowerSpline) = upperLowerSplines spline bsSplines
                        return fitParams {subDataBootstrapSet = map (\spline -> Right (Left spline)) bsSplines}
                ) (zip3 [1, 2 ..] (dataSet fitDataParams) (dataSet dataParams))
            modifyMVar_ stateRef $ \state ->
                do
                let
                    dataParams = getDataByName fitName state
                return $ updateData (dataParams {dataSet = subDataParams}) state
-}
            (progressUpdateFunc tEnv) 1
        else do
            (progressUpdateFunc tEnv) 1


getDegreesOfFreedom :: FitParams -> Int
getDegreesOfFreedom fitParams =
        let
                rank = fitPolynomRank fitParams
                numHarmonics = fitNumHarmonics fitParams

        in
        case fitType fitParams of
                FitTypeSpline ->
                        let
                                numNodes = splineNumNodes (fitSplineParams fitParams)
                        in
                                (3 * (rank + 1)) * numNodes  * numHarmonics
                FitTypeHarmonic ->
                        let
                                numModulators = harmonicCount (fitHarmonicParams fitParams)
                     in
                                (2 + 4 * numModulators) * numHarmonics + 2 * numModulators + 1
