{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module TSA.GUI.Graph (
    drawGraph,
    printGraph,
    settingsDialog,
    dataDialog,
    TSA.GUI.Graph.onKeyDown,
    TSA.GUI.Graph.onMouseMove,
    TSA.GUI.Graph.onMouseButton,
    TSA.GUI.Graph.onMouseScroll,
    updateGuiChanged,
    updateGraphSettings,
    addGraphTab,
    setNotebookEvents

    ) where

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Cairo (Context)
import GI.Cairo.Render.Connector (renderWithContext)
import Data.GI.Base
import qualified Data.Text as T

import Debug.Trace
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

import Regression.Data as D
import Regression.Spline as S
import Regression.Functions as F
import Regression.AnalyticDataWrapper as ADW
import qualified Regression.Utils as U

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Data
import TSA.Data
import qualified TSA.GUI.Gnu as Gnu
import Utils.Misc
import Utils.Math
import Utils.List
import GUI.Plot as Plot
import GUI.Widget hiding (entryGetString)

import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.List
import Data.Bits
import Data.Word
import Data.Int (Int32)
import Data.Array
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import System.Random
import System.CPUTime

import Control.Monad.IO.Class
import Control.Applicative

type TimeStamp = Word32

data SymbolSettings = SymbolSettings {
    symbolSettingsVBox :: Gtk.Box,
    symbolSettingsCombo :: Gtk.DropDown,
    symbolSettingsSizeSpin :: Gtk.SpinButton
}

data LineSettings = LineSettings {
    lineSettingsVBox :: Gtk.Box,
    lineSettingsDashSpin1 :: Gtk.SpinButton,
    lineSettingsDashSpin2 :: Gtk.SpinButton,
    lineSettingsWidthSpin :: Gtk.SpinButton
}

pointTypes = Gnu.gnuPointTypes
dataSymbolList = Gnu.gnuPointTypeNames

toMouseButton :: Word32 -> MouseButton
toMouseButton 1 = LeftButton
toMouseButton 2 = MiddleButton
toMouseButton 3 = RightButton
toMouseButton _ = OtherButton

toClick :: Int32 -> Click
toClick 2 = DoubleClick
toClick 3 = TripleClick
toClick _ = SingleClick

toModifiers :: Word32 -> [Modifier]
toModifiers m = concat [
    [Shift | m .&. 1 /= 0],
    [Control | m .&. 4 /= 0],
    [Alt | m .&. 8 /= 0]]

toGuiModifier :: Gdk.ModifierType -> [Modifier]
toGuiModifier Gdk.ModifierTypeShiftMask = [Shift]
toGuiModifier Gdk.ModifierTypeControlMask = [Control]
toGuiModifier Gdk.ModifierTypeAltMask = [Alt]
toGuiModifier _ = []

toScrollDirection :: Double -> ScrollDirection
toScrollDirection dy = if dy < 0 then ScrollUp else ScrollDown

settingsDialog :: StateRef -> IO ()
settingsDialog stateRef = do

    state <- readMVar stateRef
    (currentTabIndex, graph) <- getCurrentGraphTab state

    let
        graphTabParms = (graphTabs state) !! currentTabIndex
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms
        dParams = graphData graphParms
        prevRows = fromIntegral (graphTabRows graphTabParms)
        prevCols = fromIntegral (graphTabCols graphTabParms)

    dialog <- dialogWithTitle state "Graph settings"
    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (graphTabName graphTabParms)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    separator1 <- Gtk.separatorNew Gtk.OrientationHorizontal
    addWidgetToBox Nothing separator1 contentBox

    automaticCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive automaticCheck (graphAreaAutomatic graphParms)
    addWidgetToBox (Just "Automatic") automaticCheck contentBox

    leftAdjustment <- Gtk.adjustmentNew (plotAreaLeft ga) (-2**52) (2**52) 1 1 10
    leftSpin <- Gtk.spinButtonNew (Just leftAdjustment) 1 10
    addWidgetToBox (Just "Left: ") leftSpin contentBox

    rightAdjustment <- Gtk.adjustmentNew (plotAreaRight ga) (-2**52) (2**52) 1 1 10
    rightSpin <- Gtk.spinButtonNew (Just rightAdjustment) 1 10
    addWidgetToBox (Just "Right: ") rightSpin contentBox

    bottomAdjustment <- Gtk.adjustmentNew (plotAreaBottom ga) (-2**52) (2**52) 1 1 10
    bottomSpin <- Gtk.spinButtonNew (Just bottomAdjustment) 1 10
    addWidgetToBox (Just "Bottom: ") bottomSpin contentBox

    topAdjustment <- Gtk.adjustmentNew (plotAreaTop ga) (-2**52) (2**52) 1 1 10
    topSpin <- Gtk.spinButtonNew (Just topAdjustment) 1 10
    addWidgetToBox (Just "Top: ") topSpin contentBox

    separator2 <- Gtk.separatorNew Gtk.OrientationHorizontal
    addWidgetToBox Nothing separator2 contentBox

    widthAdjustment <- Gtk.adjustmentNew (graphWidth graphParms) 0.01 1 0.01 0.1 0.1
    widthSpin <- Gtk.spinButtonNew (Just widthAdjustment) 0.01 10
    addWidgetToBox (Just "Width: ") widthSpin contentBox

    heightAdjustment <- Gtk.adjustmentNew (graphHeight graphParms) 0.01 1 0.01 0.1 0.1
    heightSpin <- Gtk.spinButtonNew (Just heightAdjustment) 0.01 10
    addWidgetToBox (Just "Height: ") heightSpin contentBox

    separator3 <- Gtk.separatorNew Gtk.OrientationHorizontal
    addWidgetToBox Nothing separator3 contentBox

    periodAdjustment <- Gtk.adjustmentNew (graphPeriod graphParms) 0 (2**52) 1 1 1
    periodSpin <- Gtk.spinButtonNew (Just periodAdjustment) 1 10
    addWidgetToBox (Just "Period: ") periodSpin contentBox

    offsetAdjustment <- Gtk.adjustmentNew (graphOffset graphParms) (-2**52) (2**52) 1 1 10
    offsetSpin <- Gtk.spinButtonNew (Just offsetAdjustment) 1 10
    addWidgetToBox (Just "Offset: ") offsetSpin contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $
        do
            name <- entryGetString nameEntry
            automatic <- Gtk.checkButtonGetActive automaticCheck
            left <- spinButtonGetValue leftSpin
            right <- spinButtonGetValue rightSpin
            bottom <- spinButtonGetValue bottomSpin
            top <- spinButtonGetValue topSpin
            width <- spinButtonGetValue widthSpin
            height <- spinButtonGetValue heightSpin
            period <- spinButtonGetValue periodSpin
            offset <- spinButtonGetValue offsetSpin

            maybeTabLabel <- Gtk.notebookGetTabLabel (getGraphTabs state) graph
            case maybeTabLabel of
                Just tabLabel -> do
                    maybeLabel <- Gtk.widgetGetFirstChild tabLabel
                    case maybeLabel of
                        Just lbl -> do
                            label <- unsafeCastTo Gtk.Label lbl
                            Gtk.labelSetText label (T.pack name)
                        Nothing -> return ()
                Nothing -> return ()

            modifyMVar_ stateRef $ \state ->
               return $ state {
                    graphTabs = updateAt currentTabIndex (
                        graphTabParms {
                            graphTabName = name,
                            graphTabGraphs = updateAt selectedGraph (
                            graphParms {
                                graphAreaAutomatic = automatic,
                                graphArea = PlotArea {
                                    plotAreaLeft = left,
                                    plotAreaRight = right,
                                    plotAreaBottom = bottom,
                                    plotAreaTop = top,
                                    plotAreaBack = 0,
                                    plotAreaFront = 0
                                },
                                graphPeriod = period,
                                graphOffset = offset,
                                graphWidth = width,
                                graphHeight = height
                                }
                            ) (graphTabGraphs graphTabParms)
                        }
                    ) $ graphTabs state
                }
            Gtk.windowDestroy dialog

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowPresent dialog

dataDialog :: StateRef -> IO ()
dataDialog stateRef = do
    notebook <- Gtk.notebookNew
    Gtk.notebookSetScrollable notebook True

    state <- readMVar stateRef
    (currentTabIndex, graph) <- getCurrentGraphTab state

    let

        createDataParamWidgets dataPage =
            do
                descEntry <- Gtk.entryNew
                addWidgetToBox (Just "Description: ") descEntry dataPage

                color <- Gtk.colorButtonNew
                rgba0 <- Gdk.new Gdk.RGBA [#red := 0.0, #green := 0.0, #blue := 0.0, #alpha := 1.0]
                Gtk.colorChooserSetRgba color rgba0

                addWidgetToBox (Just "Color:") color dataPage

                symbolVBox <- Gtk.boxNew Gtk.OrientationVertical 0

                symbolCombo <- createComboBox dataSymbolList
                comboBoxSetActive symbolCombo 0
                addWidgetToBox (Just "Symbol: ") symbolCombo symbolVBox

                symbolSizeAdjustment <- Gtk.adjustmentNew 1 0 100000 1 1 1
                symbolSizeSpin <- Gtk.spinButtonNew (Just symbolSizeAdjustment) 1 2
                addWidgetToBox (Just "Symbol size: ") symbolSizeSpin symbolVBox

                addWidgetToBox Nothing symbolVBox dataPage

                let
                    symbolSettings = SymbolSettings {
                        symbolSettingsVBox = symbolVBox,
                        symbolSettingsCombo = symbolCombo,
                        symbolSettingsSizeSpin = symbolSizeSpin
                    }

                lineVBox <- Gtk.boxNew Gtk.OrientationVertical 0

                lineDashAdjustment1 <- Gtk.adjustmentNew 1 0 100000 1 1 1
                lineDashSpin1 <- Gtk.spinButtonNew (Just lineDashAdjustment1) 1 0
                lineDashAdjustment2 <- Gtk.adjustmentNew 0 0 100000 1 1 1
                lineDashSpin2 <- Gtk.spinButtonNew (Just lineDashAdjustment2) 1 0
                lineDashBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
                Gtk.boxAppend lineDashBox lineDashSpin1
                Gtk.boxAppend lineDashBox lineDashSpin2
                addWidgetToBox (Just "Line type: ") lineDashBox lineVBox

                lineWidthAdjustment <- Gtk.adjustmentNew 1 0 100000 1 1 1
                lineWidthSpin <- Gtk.spinButtonNew (Just lineWidthAdjustment) 1 2
                addWidgetToBox (Just "Line width: ") lineWidthSpin lineVBox

                addWidgetToBox Nothing lineVBox dataPage

                let
                    lineSettings = LineSettings {
                        lineSettingsVBox = lineVBox,
                        lineSettingsDashSpin1 = lineDashSpin1,
                        lineSettingsDashSpin2 = lineDashSpin2,
                        lineSettingsWidthSpin = lineWidthSpin
                    }

                errorBars <- Gtk.checkButtonNew
                Gtk.checkButtonSetActive errorBars False
                addWidgetToBox (Just "Show error bars") errorBars dataPage

                return (descEntry, color, symbolSettings, lineSettings, errorBars)

        toggleSettings (color, symbolSettings, lineSettings, errorBars) dataParams =
            do
                let

                    setSymbolSettingsEnabled enabled = do
                        Gtk.widgetSetSensitive (symbolSettingsCombo symbolSettings) enabled
                        Gtk.widgetSetSensitive (symbolSettingsSizeSpin symbolSettings) enabled
                    setLineSettingsEnabled enabled = do
                        Gtk.widgetSetSensitive (lineSettingsDashSpin1 lineSettings) enabled
                        Gtk.widgetSetSensitive (lineSettingsDashSpin2 lineSettings) enabled
                        Gtk.widgetSetSensitive (lineSettingsWidthSpin lineSettings) enabled
                    setColorSettingsEnabled enabled = do
                        Gtk.widgetSetSensitive color enabled
                    setErrorBarsEnabled enabled = do
                        Gtk.widgetSetSensitive errorBars enabled
                case unboxSubData $ subData (head (dataSet dataParams)) of
                    Left dat ->
                        if D.is3d dat
                            then
                                do
                                    setSymbolSettingsEnabled False
                                    setLineSettingsEnabled False
                                    setColorSettingsEnabled False
                                    setErrorBarsEnabled False
                            else
                                do
                                    setColorSettingsEnabled True
                                    setLineSettingsEnabled True
                                    if D.isSpectrum dat
                                        then
                                            setSymbolSettingsEnabled False
                                        else
                                            setSymbolSettingsEnabled True
                                    setErrorBarsEnabled True
                    Right ad ->
                        do
                            setSymbolSettingsEnabled False
                            setErrorBarsEnabled False
                            if ADW.is3d ad
                                then
                                    do
                                        setLineSettingsEnabled False
                                        setColorSettingsEnabled False
                                else
                                    do
                                        setLineSettingsEnabled True
                                        setColorSettingsEnabled True

        createDataPage (pages) i =
            do
                state <- readMVar stateRef
                let
                    graphTabParms = (graphTabs state) !! currentTabIndex
                    selectedGraph = graphTabSelection graphTabParms
                    graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
                    ga = graphArea graphParms
                    dParams = graphData graphParms
                    dp = dParams !! i
                    dataParams = getDataByName (graphDataParamsName dp) state

                dataPage <- Gtk.boxNew Gtk.OrientationVertical 0

                typeLabel <- Gtk.labelNew (Just (T.pack (getDataType dataParams)))

                addWidgetToBox (Just "Type:") typeLabel dataPage

                (desc, color, symbolSettings, lineSettings, errorBars) <- createDataParamWidgets dataPage
                let
                    (r, g, b) = graphDataParamsColor dp
                    Just selectedPointType = elemIndex (graphDataParamsPointType dp) pointTypes
                rgba <- Gdk.new Gdk.RGBA [#red := fromIntegral r / 65535, #green := fromIntegral g / 65535, #blue := fromIntegral b / 65535, #alpha := 1.0]
                Gtk.colorChooserSetRgba color rgba

                comboBoxSetActive (symbolSettingsCombo symbolSettings) selectedPointType
                Gtk.spinButtonSetValue (symbolSettingsSizeSpin symbolSettings) (graphDataParamsPointSize dp)

                Gtk.spinButtonSetValue (lineSettingsDashSpin1 lineSettings) (head (graphDataParamsLineDash dp))
                Gtk.spinButtonSetValue (lineSettingsDashSpin2 lineSettings) ((graphDataParamsLineDash dp) !! 1)

                Gtk.spinButtonSetValue (lineSettingsWidthSpin lineSettings) (graphDataParamsLineWidth dp)

                Gtk.checkButtonSetActive errorBars (graphDataParamsErrorBars dp)
                entrySetText desc (graphDataParamsDesc dp)
                toggleSettings (color, symbolSettings, lineSettings, errorBars) dataParams

                return (pages ++ [(dataPage, graphDataParamsName dp, desc, typeLabel, color, symbolSettings, lineSettings, errorBars)])

    let
        graphTabParms = (graphTabs state) !! currentTabIndex
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms
        dParams = graphData graphParms
        prevRows = fromIntegral (graphTabRows graphTabParms)
        prevCols = fromIntegral (graphTabCols graphTabParms)

    dialog <- dialogWithTitle state "Graph data"
    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    dataPages <- foldM (createDataPage) [] [0 .. length dParams - 1]
    pagesRef <- newIORef dataPages


    let

        getDataParamValues desc color symbolSettings lineSettings errorBars =
            do
                descText <- entryGetString desc
                rgba <- Gtk.colorChooserGetRgba color
                r <- Gdk.get rgba #red
                g <- Gdk.get rgba #green
                b <- Gdk.get rgba #blue
                newSymbol <- comboBoxGetActive (symbolSettingsCombo symbolSettings)
                newSymbolSize <- spinButtonGetValue (symbolSettingsSizeSpin symbolSettings)
                newLineType1 <- spinButtonGetValue (lineSettingsDashSpin1 lineSettings)
                newLineType2 <- spinButtonGetValue (lineSettingsDashSpin2 lineSettings)
                newLineWidth <- spinButtonGetValue (lineSettingsWidthSpin lineSettings)
                newErrorBars <- Gtk.checkButtonGetActive errorBars
                return  (descText, (round (r * 65535), round (g * 65535), round (b * 65535)), newSymbol, newSymbolSize, [newLineType1, newLineType2], newLineWidth, newErrorBars)

        newGraphDataParams name (desc, (r, g, b), newSymbol, newSymbolSize, [newLineType1, newLineType2], newLineWidth, newErrorBars) =
            GraphDataParams {
                graphDataParamsName = name,
                graphDataParamsDesc = desc,
                graphDataParamsColor = (r, g, b),
                graphDataParamsPointType = pointTypes !! newSymbol,
                graphDataParamsPointSize = newSymbolSize,
                graphDataParamsLineDash = [newLineType1, newLineType2],
                graphDataParamsLineWidth = newLineWidth,
                graphDataParamsErrorBars = newErrorBars
            }


        addNewData pageRef =
            do
                (dataPage, dataSetCombo, desc, color, symbolSettings, lineSettings, errorBars) <- readIORef pageRef
                maybeSelectedData <- getSelectedData dataSetCombo

                case maybeSelectedData of
                    Just selectedData ->
                        do
                            modifyMVar_ stateRef $ \state ->
                                        do
                                            newDataParamsValues <- getDataParamValues desc color symbolSettings lineSettings errorBars
                                            let
                                                graphTabParms = (graphTabs state) !! currentTabIndex
                                                selectedGraph = graphTabSelection graphTabParms
                                                graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
                                            return $
                                                state {
                                                    graphTabs = updateAt currentTabIndex (
                                                        graphTabParms {
                                                            graphTabGraphs = updateAt selectedGraph (graphParms {
                                                            graphData = (graphData graphParms) ++ [newGraphDataParams (dataName selectedData) newDataParamsValues]
                                                        }) (graphTabGraphs graphTabParms)} ) (graphTabs state)}

                            pages <- readIORef pagesRef
                            let
                                newPageIndex = length pages
                            [newPage@(page, name, _, _, _, _, _, _)] <- createDataPage [] newPageIndex

                            modifyIORef pagesRef (\pages -> pages ++ [newPage])
                            label <- labelWithButton (Just (T.pack name)) "list-remove" (removeData page)
                            _ <- Gtk.notebookInsertPageMenu notebook page (Just label) (Just label) (fromIntegral newPageIndex)
                            return ()

                    Nothing -> return ()

        newDataPage =
            do
                dataPage <- Gtk.boxNew Gtk.OrientationVertical 0

                dataSetCombo <- dataSetComboNew (\_ -> True) state
                addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) dataPage

                (desc, color, symbolSettings, lineSettings, errorBars) <- createDataParamWidgets dataPage

                hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
                addButton <- Gtk.buttonNewWithLabel "Add"
                Gtk.boxAppend hBox addButton
                addWidgetToBox Nothing hBox dataPage

                let

                    onDataSetChanged :: IO ()
                    onDataSetChanged =
                        do
                            Just selectedData <- getSelectedData dataSetCombo
                            entrySetText desc (dataDesc selectedData)
                            toggleSettings (color, symbolSettings, lineSettings, errorBars) selectedData

                _ <- on (getComboBox dataSetCombo) #notify $ \_ -> onDataSetChanged
                onDataSetChanged

                let
                    page = (dataPage, dataSetCombo, desc, color, symbolSettings, lineSettings, errorBars)

                pageRef <- newIORef page
                _ <- Gtk.onButtonClicked addButton $ addNewData pageRef

                return page

        removeData page =
            do
                pageIndex <- Gtk.notebookPageNum notebook page
                pages <- readIORef pagesRef
                let dataPage@(page, name, _, _, _, _, _, _) = pages !! (fromIntegral pageIndex)
                Gtk.notebookRemovePage notebook pageIndex
                modifyIORef pagesRef (deleteBy (\(_, name1, _, _, _, _, _, _) (_, name2, _, _, _, _, _, _) -> name1 == name2) dataPage)
                modifyMVar_ stateRef $ \state -> return $ removeDataByNameFromTab currentTabIndex name state

    let
        mapOp (page, name, _, _, _, _, _, _) =
            do
                pageIndex <- Gtk.notebookGetNPages notebook
                label <- labelWithButton (Just (T.pack name)) "list-remove" (removeData page)
                _ <- Gtk.notebookAppendPageMenu notebook page (Just label) (Just label)
                return ()

    mapM_ mapOp dataPages

    (newPage, _, _, _, _, _, _) <- newDataPage
    newPageLabel <- labelWithImage (Just "Add data") (Just "list-add")
    _ <- Gtk.notebookAppendPageMenu notebook newPage (Just newPageLabel) (Just newPageLabel)

    Gtk.boxAppend contentBox notebook

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $
        do
            pages <- readIORef pagesRef

            modifyMVar_ stateRef $ \state ->
                foldM (\state i ->
                        do
                            let
                                dataPage@(_, name, desc, _, color, symbolSettings, lineSettings, errorBars) = pages !! i
                            newDataParamsValues <- getDataParamValues desc color symbolSettings lineSettings errorBars
                            let
                                graphTabParms = (graphTabs state) !! currentTabIndex
                                selectedGraph = graphTabSelection graphTabParms
                                graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
                            return $
                                state {
                                    graphTabs = updateAt currentTabIndex (
                                        graphTabParms {
                                            graphTabGraphs = updateAt selectedGraph (
                                            graphParms {
                                                graphData = (graphData graphParms) ++ [newGraphDataParams name newDataParamsValues]}
                                            ) (graphTabGraphs graphTabParms)} ) (graphTabs state)}
                                    )
                                state {
                                        graphTabs = updateAt currentTabIndex (
                                            graphTabParms {
                                                graphTabGraphs = updateAt selectedGraph (
                                                graphParms {
                                                    graphData = []}
                                                ) (graphTabGraphs graphTabParms)
                                            }
                                        ) $ graphTabs state
                                }
                                [0 .. length pages - 1]

            Gtk.windowDestroy dialog

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowSetDefaultSize dialog 800 600
    Gtk.windowPresent dialog

onKeyDown :: StateRef -> String -> IO Bool
onKeyDown stateRef keyName =
    do
        state <- readMVar stateRef
        g <- getStdGen
        (currentTab, canvas) <- getCurrentGraphTab state
        w <- Gtk.widgetGetAllocatedWidth canvas
        h <- Gtk.widgetGetAllocatedHeight canvas

        let
            graphTabParms = (graphTabs state) !! currentTab
            selectedGraph = graphTabSelection graphTabParms
            graphParms = (graphTabGraphs graphTabParms) !! selectedGraph

        case graphPeriod graphParms of
            0 ->
                do
                    let
                         plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (fromIntegral w, fromIntegral h) g

                    Plot.onKeyDown keyName canvas plotSettings (\plotSettings ->
                        do
                            modifyMVar_ stateRef $ \state -> return $
                                let
                                    newState = updateGraphSettings state currentTab selectedGraph plotSettings
                                    graphTabParms = (graphTabs newState) !! currentTab
                                    selectedGraph = graphTabSelection graphTabParms
                                    graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
                                    tool = graphTabTool graphTabParms
                                    areaAutomatic =
                                        case keyName of
                                            "a" -> True
                                            otherwise -> graphAreaAutomatic graphParms
                                in
                                    updateGuiChanged True $
                                    newState {graphTabs = updateAt currentTab (graphTabParms {
                                        graphTabGraphs = updateAt selectedGraph (graphParms {graphAreaAutomatic = areaAutomatic}) (graphTabGraphs graphTabParms),
                                        graphTabTool = case keyName of
                                            "1" -> PlotToolSelect
                                            "2" -> PlotToolSegment
                                            otherwise -> tool
                                        }) (graphTabs state)}
                            Gtk.widgetQueueDraw canvas
                            --drawGraph stateRef Nothing
                        )
            otherwise -> return True


onMouseMove :: StateRef -> (Double, Double) -> [Modifier] -> IO Bool
onMouseMove stateRef (x, y) modifiers = do
    state <- readMVar stateRef
    g <- getStdGen
    (currentTab, canvas) <- getCurrentGraphTab state
    w <- Gtk.widgetGetAllocatedWidth canvas
    h <- Gtk.widgetGetAllocatedHeight canvas
    let
        graphTabParms = (graphTabs state) !! currentTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        grphArea = getGraphArea state currentTab selectedGraph g
        period = graphPeriod graphParms
        graphSel = graphSelection graphParms
        scrArea = getScreenArea graphTabParms selectedGraph (fromIntegral w, fromIntegral h)
        (x1, y1) = toGraphCoords scrArea (toPhaseView grphArea period) (x, y)
        updatePlotSettings =
            do
                let
                    plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (fromIntegral w, fromIntegral h) g

                Plot.onMouseMove (x, y) modifiers plotSettings (\plotSettings ->
                    do
                        modifyMVar_ stateRef $ \state -> return $
                            updateGuiChanged True (updateGraphSettings state currentTab selectedGraph plotSettings)
                        Gtk.widgetQueueDraw canvas
                        --drawGraph stateRef Nothing
                    )

    updatePlotSettings
    modifyMVar_ stateRef $ \state -> return $ setStatusBarText ((show x1) ++ ", " ++ (show y1)) state

    return True

onMouseButton :: StateRef -> MouseButton -> [Modifier] -> Click -> (Double, Double) -> TimeStamp -> IO Bool
onMouseButton stateRef button modifiers click (x, y) timestamp = do

    ----------------------------------------------------------------------------
    -- Change selected graph
    state <- readMVar stateRef
    (currentTab, canvas) <- getCurrentGraphTab state
    w <- Gtk.widgetGetAllocatedWidth canvas
    h <- Gtk.widgetGetAllocatedHeight canvas

    let
        graphTabParms = (graphTabs state) !! currentTab

        maybeNewSelectedGraph =
            findIndex (\(i, params) ->
                let
                    scrArea = getScreenArea graphTabParms i (fromIntegral w, fromIntegral h)
                    left = screenAreaLeft scrArea
                    top = screenAreaTop scrArea
                    right = screenAreaRight scrArea
                    bottom = screenAreaBottom scrArea
                in
                    x >= left && x < right && y >= top && y < bottom
            ) (assocs $ listArray (0, length (graphTabGraphs graphTabParms) - 1) (graphTabGraphs graphTabParms))

    case maybeNewSelectedGraph of
        Just newSelectedGraph ->
            case button of
                LeftButton -> do
                    modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True $
                        state {graphTabs = updateAt currentTab (graphTabParms {graphTabSelection = newSelectedGraph}) (graphTabs state)}
                    Gtk.widgetQueueDraw canvas
                    --drawGraph stateRef Nothing
                --RightButton -> do
                --        modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True $
                --            state {graphTabs = updateAt currentTab (graphTabParms {graphTabSelection = newSelectedGraph}) (graphTabs state)}
                --        Gtk.widgetQueueDraw canvas
                --        --drawGraph stateRef Nothing
                otherwise -> return ()
        otherwise -> return ()
    ----------------------------------------------------------------------------

    state <- readMVar stateRef
    (currentTab, canvas) <- getCurrentGraphTab state
    w <- Gtk.widgetGetAllocatedWidth canvas
    h <- Gtk.widgetGetAllocatedHeight canvas
    g <- getStdGen

    let
        graphTabParms = (graphTabs state) !! currentTab
        selectedGraph = graphTabSelection graphTabParms
        selectedRow = selectedGraph `quot` (graphTabCols graphTabParms)
        selectedCol = selectedGraph `rem` (graphTabCols graphTabParms)

        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        grphArea = getGraphArea state currentTab selectedGraph g
        period = graphPeriod graphParms
        graphSel = graphSelection graphParms
        scrArea = getScreenArea graphTabParms selectedGraph (fromIntegral w, fromIntegral h)
        (x1, y1) = toGraphCoords scrArea (toPhaseView grphArea period) (x, y)
    case button of
        LeftButton ->
            do
                let
                    plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (fromIntegral w, fromIntegral h) g
                Plot.onMouseButton button modifiers click (x, y) plotSettings (\plotSettings ->
                    do
                        modifyMVar_ stateRef $ \state -> return $
                            updateGuiChanged True (updateGraphSettings state currentTab selectedGraph plotSettings)
                        Gtk.widgetQueueDraw canvas
                        --drawGraph stateRef Nothing
                    )
                return ()

        RightButton ->
            case click of
                ReleaseClick ->
                    do
                        menu <- Gio.menuNew

                        settingsMenuItem <- Gio.menuItemNew (Just "Settings...") (Just "graph.settings")
                        Gio.menuAppendItem menu settingsMenuItem

                        gnuSettingsMenuItem <- Gio.menuItemNew (Just "Gnuplot settings...") (Just "graph.gnuSettings")
                        Gio.menuAppendItem menu gnuSettingsMenuItem

                        separatorMenuItem <- createMenuSeparator
                        Gio.menuAppendItem menu separatorMenuItem

                        --------------------------------------------------------
                        insertRowSection <- Gio.menuNew
                        insertRowBeforeMenuItem <- Gio.menuItemNew (Just "Before") (Just "graph.insertRowBefore")
                        Gio.menuAppendItem insertRowSection insertRowBeforeMenuItem
                        insertRowAfterMenuItem <- Gio.menuItemNew (Just "After") (Just "graph.insertRowAfter")
                        Gio.menuAppendItem insertRowSection insertRowAfterMenuItem
                        insertRowMenuItem <- Gio.menuItemNewSection (Just "Insert row") insertRowSection
                        Gio.menuAppendItem menu insertRowMenuItem
                        --------------------------------------------------------
                        insertColSection <- Gio.menuNew
                        insertColBeforeMenuItem <- Gio.menuItemNew (Just "Before") (Just "graph.insertColBefore")
                        Gio.menuAppendItem insertColSection insertColBeforeMenuItem
                        insertColAfterMenuItem <- Gio.menuItemNew (Just "After") (Just "graph.insertColAfter")
                        Gio.menuAppendItem insertColSection insertColAfterMenuItem
                        insertColMenuItem <- Gio.menuItemNewSection (Just "Insert column") insertColSection
                        Gio.menuAppendItem menu insertColMenuItem
                        --------------------------------------------------------

                        actionGroup <- Gio.simpleActionGroupNew
                        let
                            addAction name callback = do
                                action <- Gio.simpleActionNew name Nothing
                                _ <- Gio.onSimpleActionActivate action $ \_ -> callback
                                Gio.actionMapAddAction actionGroup action
                        addAction "settings" $ settingsDialog stateRef
                        addAction "gnuSettings" $ Gnu.paramsDialog stateRef
                        addAction "insertRowBefore" $ modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow, selectedCol) 1 0 state
                        addAction "insertRowAfter" $ modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow + 1, selectedCol) 1 0 state
                        addAction "insertColBefore" $ modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow, selectedCol) 0 1 state
                        addAction "insertColAfter" $ modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow, selectedCol + 1) 0 1 state
                        Gtk.widgetInsertActionGroup canvas "graph" (Just actionGroup)

                        popover <- Gtk.popoverMenuNewFromModel (Just menu)
                        Gtk.widgetSetParent popover canvas
                        _ <- Gtk.onPopoverClosed popover $ Gtk.widgetUnparent popover
                        Gtk.popoverPopup popover

                otherwise -> return ()
        otherwise -> return ()
    return True

onMouseScroll :: StateRef -> (Double, Double) -> ScrollDirection -> IO Bool
onMouseScroll stateRef (x, y) direction =
    do
        state <- readMVar stateRef
        (currentTab, canvas) <- getCurrentGraphTab state
        w <- Gtk.widgetGetAllocatedWidth canvas
        h <- Gtk.widgetGetAllocatedHeight canvas
        g <- getStdGen

        let
            graphTabParms = (graphTabs state) !! currentTab
            selectedGraph = graphTabSelection graphTabParms
            graphParms = (graphTabGraphs graphTabParms) !! selectedGraph

            grphArea = graphArea $ graphParms
            left = plotAreaLeft grphArea
            right = plotAreaRight grphArea
            bottom = plotAreaBottom grphArea
            top = plotAreaTop grphArea

            plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (fromIntegral w, fromIntegral h) g

        Plot.onMouseScroll (x, y) direction plotSettings (\plotSettings ->
            do
                modifyMVar_ stateRef $ \state -> return $
                    updateGuiChanged True (updateGraphSettings state currentTab selectedGraph plotSettings)

                Gtk.widgetQueueDraw canvas
                --drawGraph stateRef Nothing
            )

--------------------------------------------------------------------------------

drawGraph :: StateRef -> Maybe String -> IO ()
drawGraph stateRef maybeFileName = do
    randomGen <- newStdGen
    state <- readMVar stateRef
    let
        Just guiParms = guiParams state
    if guiChanged guiParms then
        do
            (currentTab, c) <- getCurrentGraphTab state
            w <- Gtk.widgetGetAllocatedWidth c
            h <- Gtk.widgetGetAllocatedHeight c
            let
                graphTabParms = (graphTabs state) !! currentTab
                selectedGraph = graphTabSelection graphTabParms
                rows = graphTabRows graphTabParms
                cols = graphTabCols graphTabParms
                graphs = graphTabGraphs graphTabParms

            settings <-
                mapM (\(currentGraph, row, col, graphParms) ->
                    do
                        let
                            grphArea = getGraphArea state currentTab currentGraph randomGen
                            scrArea = getScreenArea graphTabParms currentGraph (fromIntegral w, fromIntegral h)

                            plotSettings = getPlotSettings state currentTab currentGraph selectedGraph (fromIntegral w, fromIntegral h) randomGen
                            period = graphPeriod graphParms

                        dataSettings <-
                            mapM (\gdp ->
                                do
                                    let
                                        dp = getDataByName (graphDataParamsName gdp) state
                                        ds = getPlotData (toPhaseView grphArea period) gdp dp period (fromIntegral w, fromIntegral h) randomGen

                                    return ds
                            ) $ graphData graphParms

                        modifyMVar_ stateRef $ \state -> return $
                            state {graphTabs = updateAt currentTab (graphTabParms {graphTabGraphs = updateAt currentGraph (graphParms {graphArea = grphArea}) (graphTabGraphs graphTabParms)}) (graphTabs state)}
                        return (plotSettings, concat dataSettings)


                    ) (map (\i -> (i, i `quot` cols, i `rem` cols, graphs !! i)) [0 .. length graphs - 1])
            plot c settings maybeFileName
            modifyMVar_ stateRef $ \state -> return $ updateGuiChanged False state
    else
        return ()

canvasDrawFunc :: StateRef -> Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ()
canvasDrawFunc stateRef _drawingArea context width height = do
    state <- readMVar stateRef
    randomGen <- newStdGen
    (currentTab, _) <- getCurrentGraphTab state
    let
        w = fromIntegral width
        h = fromIntegral height
        graphTabParms = (graphTabs state) !! currentTab
        selectedGraph = graphTabSelection graphTabParms
        cols = graphTabCols graphTabParms
        graphs = graphTabGraphs graphTabParms

    settings <-
        mapM (\(currentGraph, row, col, graphParms) ->
            do
                let
                    grphArea = getGraphArea state currentTab currentGraph randomGen
                    scrArea = getScreenArea graphTabParms currentGraph (w, h)
                    plotSettings = getPlotSettings state currentTab currentGraph selectedGraph (w, h) randomGen
                    period = graphPeriod graphParms
                dataSettings <-
                    mapM (\gdp ->
                        do
                            let
                                dp = getDataByName (graphDataParamsName gdp) state
                                ds = getPlotData (toPhaseView grphArea period) gdp dp period (fromIntegral w, fromIntegral h) randomGen
                            return ds
                    ) $ graphData graphParms
                modifyMVar_ stateRef $ \state -> return $
                    state {graphTabs = updateAt currentTab (graphTabParms {graphTabGraphs = updateAt currentGraph (graphParms {graphArea = grphArea}) (graphTabGraphs graphTabParms)}) (graphTabs state)}
                return (plotSettings, concat dataSettings)
            ) (map (\i -> (i, i `quot` cols, i `rem` cols, graphs !! i)) [0 .. length graphs - 1])
    renderWithContext (Plot.renderPlot settings) context

updateGuiChanged :: Bool -> State -> State
updateGuiChanged changed state =
    let
        Just guiParms = guiParams state
    in
        state {guiParams = Just guiParms {guiChanged = changed}}

printGraph :: StateRef -> IO ()
printGraph stateRef = do
    state <- readMVar stateRef
    g <- getStdGen
    (_, c) <- getCurrentGraphTab state

    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog "Save graph"
    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter "*.PDF"
    Gtk.fileFilterAddPattern fileFilter "*.pdf"
    Gtk.fileFilterSetName fileFilter (Just "PDF files")

    filters <- Gio.listStoreNew =<< glibType @Gtk.FileFilter
    Gio.listStoreAppend filters fileFilter
    Gtk.fileDialogSetFilters dialog (Just filters)
    Gtk.fileDialogSetDefaultFilter dialog (Just fileFilter)

    Gtk.fileDialogSave dialog (Just (getWindow state)) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
        file <- Gtk.fileDialogSaveFinish dialog result
        maybePath <- Gio.fileGetPath file
        case maybePath of
            Just path -> do
                modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
                drawGraph stateRef (Just path)
            Nothing -> return ()

getPlotData :: (RandomGen g) => PlotArea -> GraphDataParams -> DataParams -> Double -> (Double, Double) -> g -> [PlotData]
getPlotData graphArea graphDataParams dataParams period (w, h) randomGen =
    let
            get1dData d =
                V.fromList $ map (\vec -> ((V.head vec, 0, 0), (V.head vec, fromIntegral (V.length vec), 0))) $ groupVector $ D.ys d
            get2dData d =
                V.map (\(x, y, weight) -> ((toPhase x period, 0), (y, if graphDataParamsErrorBars graphDataParams && weight > 0 then sqrt (1 / weight) else 0))) $ D.values1 d
            get3dData = xys2
            sample2dData d =
                let
                    (xAreaLeft, xAreaRight) = (plotAreaLeft graphArea, plotAreaRight graphArea)
                    xLeft = max xAreaLeft (ADW.xMin1 d)
                    xRight = min xAreaRight (ADW.xMax1 d)
                    xStep = (xAreaRight - xAreaLeft) / w
                    xs = if xStep > 0 && xLeft <= xRight then [xLeft, xLeft + xStep .. xRight] else []
                in
                    V.fromList $ zipWith (\x y -> ((x, 0), (y, 0))) xs (ADW.getValues (map (:[]) xs) randomGen d)
            sample3dData d =
                let
                    (xLeft, xRight) = (plotAreaLeft graphArea, plotAreaRight graphArea)
                    xStep = (xRight - xLeft) / w / 10
                    yStep = (plotAreaTop graphArea - plotAreaBottom graphArea) / h / 10
                    xs = [[x1, x2] | x1 <- [xLeft, xLeft + xStep .. xRight], x2 <- [plotAreaBottom graphArea, plotAreaBottom graphArea + yStep .. plotAreaTop graphArea]]
                in
                    D.xys2 (U.sampleAnalyticData d [xLeft, plotAreaBottom graphArea] [xRight, plotAreaTop graphArea] [100, 75] randomGen)
            lineAttributes =
                PlotLineAttributes {
                    plotLineDash = graphDataParamsLineDash graphDataParams,
                    plotLineWidth = graphDataParamsLineWidth graphDataParams,
                    plotLineColor = getRGBA $ graphDataParamsColor graphDataParams
                }
            mapOp sdp =
                case unboxSubData $ subData sdp of
                    Left d ->
                        case D.dim d of
                            0 ->
                                PlotVectors {
                                    plotVectors = get1dData d,
                                    plotVectorLineAttributes = lineAttributes,
                                    plotVectorStartStyle = 0,
                                    plotVectorEndStyle = 0
                                }
                            1 -> if D.isData d
                                -- 2D data
                                then
                                    PlotData {
                                        plotDataValues = get2dData d,
                                        plotDataLineAttributes = case graphDataParamsLineWidth graphDataParams of
                                            0 -> Nothing
                                            otherwise -> Just lineAttributes,
                                        plotDataPointAttributes =
                                            case graphDataParamsPointSize graphDataParams of
                                                0 -> Nothing
                                                otherwise ->
                                                    Just $ PlotPointAttributes {
                                                        plotPointType = graphDataParamsPointType graphDataParams,
                                                        plotPointSize = graphDataParamsPointSize graphDataParams,
                                                        plotPointColor = getRGBA $ graphDataParamsColor graphDataParams
                                                    }
                                    }
                                -- 2D spectrum
                                else
                                    PlotData {
                                        plotDataValues = get2dData d,
                                        plotDataLineAttributes = Just lineAttributes,
                                        plotDataPointAttributes = Nothing
                                    }
                            2 ->
                                -- 3D data or spectrum
                                PlotData3d {
                                    plotDataValues3d = get3dData d
                                }
                    Right ad ->
                        if ADW.is2d ad
                        then
                            PlotData {
                                plotDataValues = sample2dData ad,
                                plotDataLineAttributes = Just lineAttributes,
                                plotDataPointAttributes = Nothing
                            }
                        else
                            PlotData3d {
                                plotDataValues3d = sample3dData ad
                            }
    in
        map mapOp (dataSet dataParams)
--------------------------------------------------------------------------------

getGraphArea :: RandomGen g => State -> Int -> Int -> g -> PlotArea
getGraphArea state tabIndex graphIndex g =
    let
        graphTabParms = (graphTabs state) !! tabIndex
        graphParms = (graphTabGraphs graphTabParms) !! graphIndex
        dataParms = map (\gdp -> getDataByName (graphDataParamsName gdp) state) (graphData graphParms)
    in
        if graphAreaAutomatic graphParms && length (graphData graphParms) > 0
            then
                let
                    sample (Left d) = d
                    sample (Right ad) =
                        D.filterData (\(_, y, _) -> not (isNaN y) && not (isInfinite y)) $
                            if ADW.is3d ad
                                then U.sampleAnalyticData_ ad [100, 100] g
                                else U.sampleAnalyticData_ ad [1000] g

                    f xyz minOrMax =
                        let
                            vals = concat $
                                map (\dp -> map (\sdp ->
                                    let
                                        d = sample $ unboxSubData (subData sdp)
                                    in
                                        case xyz of
                                            0 ->
                                                if minOrMax
                                                    then
                                                        case D.dim d of
                                                            0 -> D.yMin d
                                                            otherwise -> D.xMin1 d
                                                    else
                                                        case D.dim d of
                                                            0 -> D.yMax d
                                                            otherwise -> D.xMax1 d
                                            1 ->
                                                if minOrMax
                                                    then
                                                        case D.dim d of
                                                            0 -> 0
                                                            1 -> D.yMin d
                                                            2 -> D.xMini 1 d
                                                    else
                                                        case D.dim d of
                                                            0 -> 1
                                                            1 -> D.yMax d
                                                            2 -> D.xMaxi 1 d
                                            2 ->
                                                if  D.is3d d
                                                    then
                                                        if minOrMax
                                                            then
                                                                D.yMin d
                                                            else
                                                                D.yMax d
                                                    else 0
                                    ) (dataSet dp)) dataParms
                        in
                            (if minOrMax then minimum else maximum) vals

                    (xLeft, xRight, yBottom, yTop, zBack, zFront) =
                        case graphPeriod graphParms of
                            0 -> (f 0 True, f 0 False, f 1 True, f 1 False, f 2 True, f 2 False)
                            otherwise -> (0, 1, f 1 True, f 1 False, f 2 True, f 2 False)

                    xSpace = if xRight == xLeft then 1 else (xRight - xLeft) / 20
                    ySpace = if yTop == yBottom then 1 else (yTop - yBottom) / 20

                in
                    PlotArea {
                        plotAreaLeft = xLeft - xSpace,
                        plotAreaRight = xRight + xSpace,
                        plotAreaTop = yTop + ySpace,
                        plotAreaBottom = yBottom - ySpace,
                        plotAreaBack = zBack - (zFront - zBack) / 10,
                        plotAreaFront = zFront + (zFront - zBack) / 10
                        }
            else
                graphArea graphParms


getScreenArea :: GraphTabParams -> Int -> (Int, Int) -> ScreenArea
getScreenArea graphTab selectedGraph (w, h) =
    let
        (left, top, right, bottom) = getNormalizedScreenArea graphTab selectedGraph
    in
        ScreenArea {
            screenAreaLeft = fromIntegral w * left,
            screenAreaTop = fromIntegral h * top,
            screenAreaRight = fromIntegral w * right,
            screenAreaBottom = fromIntegral h * bottom,
            screenAreaBack = 0,
            screenAreaFront = 1
        }

getRed :: (Word16, Word16, Word16) -> Double
getRed (r, g, b) = (fromIntegral r) / 65535

getGreen :: (Word16, Word16, Word16) -> Double
getGreen (r, g, b) = (fromIntegral g) / 65535

getBlue :: (Word16, Word16, Word16) -> Double
getBlue (r, g, b) = (fromIntegral b) / 65535

getRGBA :: (Word16, Word16, Word16) -> (Double, Double, Double, Double)
getRGBA c = (getRed c, getGreen c, getBlue c, 1)

toPhaseView :: PlotArea -> Double -> PlotArea
toPhaseView grphArea period =
    case period of
        0 -> grphArea
        otherwise -> grphArea {
            plotAreaLeft = 0,
            plotAreaRight = 1
        }

toPhase :: Double -> Double -> Double
toPhase x period =
    case period of
        0 -> x
        otherwise -> snd (properFraction (x / period))

getPlotSettings :: RandomGen r => State -> Int -> Int -> Int -> (Int, Int) -> r -> PlotSettings
getPlotSettings state currentTab currentGraph selectedGraph (w, h) randomGen =
    let
        graphTabParms = (graphTabs state) !! currentTab
        grphArea = getGraphArea state currentTab currentGraph randomGen
        scrArea = getScreenArea graphTabParms currentGraph (w, h)
        graphParms = (graphTabGraphs graphTabParms) !! currentGraph

    in
        PlotSettings {
            plotArea = grphArea,
            screenArea = scrArea,
            plotMinorXUnit = graphMinorXUnit graphParms,
            plotMinorYUnit = graphMinorYUnit graphParms,
            plotMajorXUnit = graphMajorXUnit graphParms,
            plotMajorYUnit = graphMajorYUnit graphParms,
            mousePos = guiMousePos (fromJust (guiParams state)),
            plotSelection =
                Just PlotSelection {
                    plotSelectionRectangle = fmap (\sel ->
                        Plot.Rectangle {
                            rectangleLeft = graphSelectionLeft sel,
                            rectangleRight = graphSelectionRight sel,
                            rectangleBottom = graphSelectionBottom sel,
                            rectangleTop = graphSelectionTop sel
                        }
                    ) (graphSelection graphParms),
                    plotSelectionLineAttributes = PlotLineAttributes {
                        plotLineDash = [5, 5],
                        plotLineWidth = 1,
                        plotLineColor = (0.5, 0.5, 0.5, 0.75)
                    }
                },
            plotSegments = Just PlotSegments {
                plotSegmentsData = graphSegments graphParms,
                plotSegmentsLineAttributes = PlotLineAttributes {
                    plotLineDash = [5, 5],
                    plotLineWidth = 1,
                    plotLineColor = (0.5, 0.5, 0.5, 0.75)
                }
            },
            plotTool = graphTabTool graphTabParms,
            plotBackground =
                if currentGraph /= selectedGraph
                    then
                            (0.9, 0.9, 0.9)
                    else
                            (1, 1, 1)
        }

updateGraphSettings :: State -> Int -> Int -> PlotSettings -> State
updateGraphSettings state currentTab currentGraph plotSettings =
        let
            graphTabParms = (graphTabs state) !! currentTab
            graphParms = (graphTabGraphs graphTabParms) !! currentGraph
            newSelection =
                case plotSelection plotSettings of
                    Just sel ->
                        fmap (\rect ->
                            GraphSelection {
                                graphSelectionLeft = rectangleLeft rect,
                                graphSelectionRight = rectangleRight rect,
                                graphSelectionBottom = rectangleBottom rect,
                                graphSelectionTop = rectangleTop rect
                            }
                        ) (plotSelectionRectangle sel)
                    otherwise -> Nothing
            newSegments =
                case plotSegments plotSettings of
                    Just segments -> plotSegmentsData segments
                    otherwise -> []
            newArea = plotArea plotSettings

        in
            state {
                guiParams = fmap (\guiParms -> guiParms {guiMousePos = mousePos plotSettings}) (guiParams state),
                graphTabs = updateAt currentTab (graphTabParms {
                    graphTabGraphs = updateAt currentGraph (graphParms {
                        graphSelection = newSelection,
                        graphArea = newArea,
                        graphAreaAutomatic = False,
                        graphSegments = newSegments
                    }) (graphTabGraphs graphTabParms)
                }) (graphTabs state)
            }

createMenuSeparator :: IO Gio.MenuItem
createMenuSeparator = do
    section <- Gio.menuNew
    item <- Gio.menuItemNewSection Nothing section
    return item

setNotebookEvents :: StateRef -> IO ()
setNotebookEvents stateRef =
    do
        state <- readMVar stateRef
        let
            notebook = getGraphTabs state

        _ <- on notebook #switchPage $ \page pageNum ->
            do
                modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
                numPages <- Gtk.notebookGetNPages notebook
                if numPages > 1 && pageNum == fromIntegral (numPages - 1)
                    then
                        do
                                addGraphTab stateRef Nothing
                                _ <- Gtk.notebookPrevPage notebook
                                return ()
                    else return ()

        numPages <- Gtk.notebookGetNPages notebook

        let
            mapOp i =
                do
                    Just page <- Gtk.notebookGetNthPage notebook i

                    -- Mouse button press
                    clickController <- Gtk.gestureClickNew
                    Gtk.gestureSingleSetButton clickController 0
                    _ <- Gtk.onGestureClickPressed clickController $ \nPress x y -> liftIO $ do
                        buttonNo <- Gtk.gestureSingleGetCurrentButton clickController
                        maybeEvent <- Gtk.eventControllerGetCurrentEvent clickController
                        modifiers <- case maybeEvent of
                            Just event -> concatMap toGuiModifier <$> Gdk.eventGetModifierState event
                            Nothing -> return []
                        _ <- TSA.GUI.Graph.onMouseButton stateRef (toMouseButton buttonNo) modifiers (toClick nPress) (x, y) 0
                        return ()
                    Gtk.widgetAddController page clickController

                    -- Mouse button release
                    releaseController <- Gtk.gestureClickNew
                    Gtk.gestureSingleSetButton releaseController 0
                    _ <- Gtk.onGestureClickReleased releaseController $ \nPress x y -> liftIO $ do
                        buttonNo <- Gtk.gestureSingleGetCurrentButton releaseController
                        maybeEvent <- Gtk.eventControllerGetCurrentEvent releaseController
                        modifiers <- case maybeEvent of
                            Just event -> concatMap toGuiModifier <$> Gdk.eventGetModifierState event
                            Nothing -> return []
                        _ <- TSA.GUI.Graph.onMouseButton stateRef (toMouseButton buttonNo) modifiers ReleaseClick (x, y) 0
                        return ()
                    Gtk.widgetAddController page releaseController

                    -- Mouse motion
                    motionController <- Gtk.eventControllerMotionNew
                    _ <- Gtk.onEventControllerMotionMotion motionController $ \x y -> liftIO $ do
                        maybeEvent <- Gtk.eventControllerGetCurrentEvent motionController
                        modifiers <- case maybeEvent of
                            Just event -> concatMap toGuiModifier <$> Gdk.eventGetModifierState event
                            Nothing -> return []
                        _ <- TSA.GUI.Graph.onMouseMove stateRef (x, y) modifiers
                        return ()
                    Gtk.widgetAddController page motionController

                    -- Mouse scroll
                    scrollController <- Gtk.eventControllerScrollNew [Gtk.EventControllerScrollFlagsVertical, Gtk.EventControllerScrollFlagsDiscrete]
                    _ <- Gtk.onEventControllerScrollScroll scrollController $ \dx dy -> liftIO $ do
                        let direction = if dy < 0 then ScrollUp else ScrollDown
                        maybeEvent <- Gtk.eventControllerGetCurrentEvent scrollController
                        (x, y) <- case maybeEvent of
                            Just event -> do
                                (_, ex, ey) <- Gdk.eventGetPosition event
                                return (ex, ey)
                            Nothing -> return (0, 0)
                        _ <- TSA.GUI.Graph.onMouseScroll stateRef (x, y) direction
                        return True
                    Gtk.widgetAddController page scrollController
        mapM_ mapOp [0 .. numPages - 2]

addGraphTab :: StateRef -> Maybe String -> IO Int
addGraphTab stateRef maybeGraphName =
    do
        state <- readMVar stateRef
        let
            notebook = getGraphTabs state
        pageIndex <- Gtk.notebookGetNPages notebook
        page <- Gtk.drawingAreaNew
        Gtk.drawingAreaSetDrawFunc page (Just (canvasDrawFunc stateRef))
        let
            removeTab page =
                do
                    numPages <- Gtk.notebookGetNPages notebook
                    if numPages > 2
                        then
                            do
                                pageIndex <- Gtk.notebookPageNum notebook page
                                if pageIndex == numPages - 2
                                    then
                                        do
                                            _ <- Gtk.notebookPrevPage notebook
                                            return ()
                                    else
                                        return ()
                                _ <- Gtk.notebookRemovePage notebook pageIndex
                                modifyMVar_ stateRef $ \state -> return $ state {graphTabs = (removeAt (fromIntegral pageIndex) (graphTabs state))}
                        else
                            return ()
            tabName =
                case maybeGraphName of
                    Nothing -> "Graph " ++ show pageIndex
                    Just graphName -> graphName
        label <- labelWithButton (Just (T.pack tabName)) "list-remove" (removeTab page)

        case maybeGraphName of
            Nothing ->
                modifyMVar_ stateRef $ \state -> return $ state {graphTabs = (insertAt (fromIntegral pageIndex - 1) (newGraphTab tabName) (graphTabs state))}
            Just _ ->
                return ()

        _ <- Gtk.notebookInsertPageMenu notebook page (Just label) (Just label) (pageIndex - 1)
        _ <- Gtk.notebookSetCurrentPage notebook (pageIndex - 1)
        setNotebookEvents stateRef
        return (fromIntegral pageIndex - 1)