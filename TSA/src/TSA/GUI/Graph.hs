
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

import Graphics.UI.Gtk hiding (addWidget, Plus, Cross, Circle)
import Graphics.Rendering.Cairo

import Debug.Trace
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

import Regression.Data as D
import Regression.Spline as S
import Regression.Functions as F
import Regression.AnalyticData as AD
import qualified Regression.Utils as U

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
import GUI.Widget

import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.List
import Data.Word
import Data.Array
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import System.Random
import System.CPUTime

import Control.Monad.IO.Class
import Control.Applicative

data SymbolSettings = SymbolSettings {
    symbolSettingsVBox :: VBox,
    symbolSettingsCombo :: ComboBox,
    symbolSettingsSizeSpin :: SpinButton
} 

data LineSettings = LineSettings {
    lineSettingsVBox :: VBox,
    lineSettingsDashSpin1 :: SpinButton,
    lineSettingsDashSpin2 :: SpinButton,
    lineSettingsWidthSpin :: SpinButton
} 

pointTypes = Gnu.gnuPointTypes
dataSymbolList = Gnu.gnuPointTypeNames

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
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2

    nameEntry <- entryNew
    nameEntry `entrySetText` (graphTabName graphTabParms)
    addWidgetToBox (Just "Name: ") nameEntry PackNatural vBox

    separator1 <- hSeparatorNew
    addWidgetToBox Nothing separator1 PackNatural vBox
    
    automaticCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button (graphAreaAutomatic graphParms) >> return button
    addWidgetToBox (Just "Automatic") automaticCheck PackNatural vBox 

    leftAdjustment <- adjustmentNew (plotAreaLeft ga) (-2**52) (2**52) 1 1 10
    leftSpin <- spinButtonNew leftAdjustment 1 10
    addWidgetToBox (Just "Left: ") leftSpin PackNatural vBox

    rightAdjustment <- adjustmentNew (plotAreaRight ga) (-2**52) (2**52) 1 1 10
    rightSpin <- spinButtonNew rightAdjustment 1 10
    addWidgetToBox (Just "Right: ") rightSpin PackNatural vBox
    
    bottomAdjustment <- adjustmentNew (plotAreaBottom ga) (-2**52) (2**52) 1 1 10
    bottomSpin <- spinButtonNew bottomAdjustment 1 10
    addWidgetToBox (Just "Bottom: ") bottomSpin PackNatural vBox

    topAdjustment <- adjustmentNew (plotAreaTop ga) (-2**52) (2**52) 1 1 10
    topSpin <- spinButtonNew topAdjustment 1 10
    addWidgetToBox (Just "Top: ") topSpin PackNatural vBox

    separator2 <- hSeparatorNew
    addWidgetToBox Nothing separator2 PackNatural vBox

    widthAdjustment <- adjustmentNew (graphWidth graphParms) 0.01 1 0.01 0.1 0.1
    widthSpin <- spinButtonNew widthAdjustment 0.01 10
    addWidgetToBox (Just "Width: ") widthSpin PackNatural vBox

    heightAdjustment <- adjustmentNew (graphHeight graphParms) 0.01 1 0.01 0.1 0.1
    heightSpin <- spinButtonNew heightAdjustment 0.01 10
    addWidgetToBox (Just "Height: ") heightSpin PackNatural vBox
    
    separator3 <- hSeparatorNew
    addWidgetToBox Nothing separator3 PackNatural vBox

    periodAdjustment <- adjustmentNew (graphPeriod graphParms) 0 (2**52) 1 1 1
    periodSpin <- spinButtonNew periodAdjustment 1 10
    addWidgetToBox (Just "Period: ") periodSpin PackNatural vBox

    offsetAdjustment <- adjustmentNew (graphOffset graphParms) (-2**52) (2**52) 1 1 10
    offsetSpin <- spinButtonNew offsetAdjustment 1 10
    addWidgetToBox (Just "Offset: ") offsetSpin PackNatural vBox

    let
        onClose response =
            do
                if response == ResponseOk
                    then 
                        do

                            name <- entryGetString nameEntry
                            automatic <- toggleButtonGetActive automaticCheck
                            left <- spinButtonGetValue leftSpin
                            right <- spinButtonGetValue rightSpin
                            bottom <- spinButtonGetValue bottomSpin
                            top <- spinButtonGetValue topSpin
                            width <- spinButtonGetValue widthSpin
                            height <- spinButtonGetValue heightSpin
                            period <- spinButtonGetValue periodSpin
                            offset <- spinButtonGetValue offsetSpin

                            Just tabLabel <- notebookGetTabLabel (getGraphTabs state) graph
                            children <- containerGetChildren (castToContainer tabLabel)
                            labelSetText (castToLabel $ head children) name

                            --notebookSetTabLabelText (getGraphTabs state) graph name

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
                    else
                        return ()
                
        

    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Ok" ResponseOk

    widgetShowAll dialog
    response <- dialogRun dialog 

    newState <- onClose response

    widgetDestroy dialog
    return newState

dataDialog :: StateRef -> IO ()
dataDialog stateRef = do
    notebook <- notebookNew
    notebookSetScrollable notebook True
    
    state <- readMVar stateRef
    (currentTabIndex, graph) <- getCurrentGraphTab state

    let
        
        createDataParamWidgets dataPage =
            do
                color <- colorButtonNew 
                color `colorButtonSetColor` (Color 0 0 0)
        
                addWidgetToBox (Just "Color:") color PackNatural dataPage
        
                symbolVBox <- vBoxNew True 0
        
                symbolCombo <- createComboBox dataSymbolList
                comboBoxSetActive symbolCombo 0
                addWidgetToBox (Just "Symbol: ") symbolCombo PackNatural symbolVBox
        
                symbolSizeAdjustment <- adjustmentNew 1 0 100000 1 1 1
                symbolSizeSpin <- spinButtonNew symbolSizeAdjustment 1 2
                addWidgetToBox (Just "Symbol size: ") symbolSizeSpin PackNatural symbolVBox

                addWidgetToBox Nothing symbolVBox PackNatural dataPage
                
                let 
                    symbolSettings = SymbolSettings {
                        symbolSettingsVBox = symbolVBox, 
                        symbolSettingsCombo = symbolCombo,
                        symbolSettingsSizeSpin = symbolSizeSpin
                    }

                lineVBox <- vBoxNew True 0
    
                lineDashAdjustment1 <- adjustmentNew 1 0 100000 1 1 1
                lineDashSpin1 <- spinButtonNew lineDashAdjustment1 1 0
                lineDashAdjustment2 <- adjustmentNew 0 0 100000 1 1 1
                lineDashSpin2 <- spinButtonNew lineDashAdjustment2 1 0
                lineDashBox <- hBoxNew True 0
                boxPackStart lineDashBox lineDashSpin1 PackNatural 2
                boxPackStart lineDashBox lineDashSpin2 PackNatural 2
                addWidgetToBox (Just "Line type: ") lineDashBox PackNatural lineVBox
    
                lineWidthAdjustment <- adjustmentNew 1 0 100000 1 1 1
                lineWidthSpin <- spinButtonNew lineWidthAdjustment 1 2
                addWidgetToBox (Just "Line width: ") lineWidthSpin PackNatural lineVBox

                addWidgetToBox Nothing lineVBox PackNatural dataPage
                
                let 
                    lineSettings = LineSettings {
                        lineSettingsVBox = lineVBox,
                        lineSettingsDashSpin1 = lineDashSpin1,
                        lineSettingsDashSpin2 = lineDashSpin2,
                        lineSettingsWidthSpin = lineWidthSpin
                    } 
                
                errorBars <- checkButtonNew >>= \button -> toggleButtonSetActive button False >> return button
                addWidgetToBox (Just "Show error bars") errorBars PackNatural dataPage 
                
                return (color, symbolSettings, lineSettings, errorBars)
        
        toggleSettings (color, symbolSettings, lineSettings, errorBars) dataParams =
            do
                let
                
                    setSymbolSettingsEnabled enabled = do
                        widgetSetSensitive (symbolSettingsCombo symbolSettings) enabled
                        widgetSetSensitive (symbolSettingsSizeSpin symbolSettings) enabled
                    setLineSettingsEnabled enabled = do
                        widgetSetSensitive (lineSettingsDashSpin1 lineSettings) enabled
                        widgetSetSensitive (lineSettingsDashSpin2 lineSettings) enabled
                        widgetSetSensitive (lineSettingsWidthSpin lineSettings) enabled
                    setColorSettingsEnabled enabled = do
                        widgetSetSensitive color enabled
                    setErrorBarsEnabled enabled = do
                        widgetSetSensitive errorBars enabled
                case subData (head (dataSet dataParams)) of
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
                    Right (Left spline) -> do
                        setSymbolSettingsEnabled False
                        setLineSettingsEnabled True
                        setColorSettingsEnabled True
                        setErrorBarsEnabled False
                    Right (Right ad) ->
                        do 
                            setSymbolSettingsEnabled False
                            setErrorBarsEnabled False
                            if AD.is3d ad
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
                
                typeLbl <- labelNew (Just "Type: ")
                typeLabel <- labelNew $ Just $ getDataType dataParams 
        
                dataPage <- vBoxNew True 0
                addWidgetToBox (Just "Type:") typeLabel PackNatural dataPage
                
                (color, symbolSettings, lineSettings, errorBars) <- createDataParamWidgets dataPage
                let 
                    (r, g, b) = graphDataParamsColor dp
                    Just selectedPointType = elemIndex (graphDataParamsPointType dp) pointTypes 
                color `colorButtonSetColor` (Color r g b)
        
                comboBoxSetActive (symbolSettingsCombo symbolSettings) selectedPointType
                spinButtonSetValue (symbolSettingsSizeSpin symbolSettings) (graphDataParamsPointSize dp)

                spinButtonSetValue (lineSettingsDashSpin1 lineSettings) (head (graphDataParamsLineDash dp))
                spinButtonSetValue (lineSettingsDashSpin2 lineSettings) ((graphDataParamsLineDash dp) !! 1)

                spinButtonSetValue (lineSettingsWidthSpin lineSettings) (graphDataParamsLineWidth dp)
        
                toggleButtonSetActive errorBars (graphDataParamsErrorBars dp)
                toggleSettings (color, symbolSettings, lineSettings, errorBars) dataParams
                
        
                return (pages ++ [(dataPage, graphDataParamsName dp, typeLabel, color, symbolSettings, lineSettings, errorBars)])
            
    let
        graphTabParms = (graphTabs state) !! currentTabIndex
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms
        dParams = graphData graphParms
        prevRows = fromIntegral (graphTabRows graphTabParms)
        prevCols = fromIntegral (graphTabCols graphTabParms)

    dialog <- dialogWithTitle state "Graph data"
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2

    dataPages <- foldM (createDataPage) [] [0 .. length dParams - 1] 
    pagesRef <- newIORef dataPages
    

    let
    
        getDataParamValues color symbolSettings lineSettings errorBars =
            do
                Color r g b <- colorButtonGetColor color  
                newSymbol <- comboBoxGetActive (symbolSettingsCombo symbolSettings)
                newSymbolSize <- spinButtonGetValue (symbolSettingsSizeSpin symbolSettings)
                newLineType1 <- spinButtonGetValue (lineSettingsDashSpin1 lineSettings)
                newLineType2 <- spinButtonGetValue (lineSettingsDashSpin2 lineSettings)
                newLineWidth <- spinButtonGetValue (lineSettingsWidthSpin lineSettings)
                newErrorBars <- toggleButtonGetActive errorBars
                return  ((r, g, b), newSymbol, newSymbolSize, [newLineType1, newLineType2], newLineWidth, newErrorBars)
                
        newGraphDataParams name ((r, g, b), newSymbol, newSymbolSize, [newLineType1, newLineType2], newLineWidth, newErrorBars) =
            GraphDataParams {
                graphDataParamsName = name,
                graphDataParamsColor = (r, g, b), 
                graphDataParamsPointType = pointTypes !! newSymbol,
                graphDataParamsPointSize = newSymbolSize,
                graphDataParamsLineDash = [newLineType1, newLineType2],
                graphDataParamsLineWidth = newLineWidth,
                graphDataParamsErrorBars = newErrorBars
            }
        
    
        addNewData pageRef = 
            do
                (dataPage, dataSetCombo, color, symbolSettings, lineSettings, errorBars) <- readIORef pageRef
                maybeSelectedData <- getSelectedData dataSetCombo
                
                case maybeSelectedData of
                    Just selectedData ->
                        do
                            modifyMVar_ stateRef $ \state ->
                                        do
                                            newDataParamsValues <- getDataParamValues color symbolSettings lineSettings errorBars
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
                            [newPage@(page, name, _, _, _, _, _)] <- createDataPage [] newPageIndex
            
                            modifyIORef pagesRef (\pages -> pages ++ [newPage])
                            label <- labelWithButton (Just name) stockRemove (removeData page)
                            notebookInsertPageMenu notebook page label label newPageIndex
                            widgetShowAll label
                            widgetShowAll notebook
                    
                    Nothing -> return ()
                
        newDataPage =
            do
                dataPage <- vBoxNew True 0
    
                dataSetCombo <- dataSetComboNew (\_ -> True) state
                addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) PackNatural dataPage
                
                (color, symbolSettings, lineSettings, errorBars) <- createDataParamWidgets dataPage

                hBox <- hBoxNew False 0
                addButton <- buttonNewWithLabel "Add"
                boxPackEnd hBox addButton PackNatural 2
                addWidgetToBox Nothing hBox PackNatural dataPage

                let
                
                    onDataSetChanged :: IO ()
                    onDataSetChanged = 
                        do
                            Just selectedData <- getSelectedData dataSetCombo
                            toggleSettings (color, symbolSettings, lineSettings, errorBars) selectedData
                                    
                on (getComboBox dataSetCombo) changed onDataSetChanged
                onDataSetChanged

                let
                    page = (dataPage, dataSetCombo, color, symbolSettings, lineSettings, errorBars)

                pageRef <- newIORef page
                on addButton buttonReleaseEvent $ liftIO $ addNewData pageRef >> return True

                return page
                
        removeData page = 
            do
                Just pageIndex <- notebookPageNum notebook page
                pages <- readIORef pagesRef
                let dataPage@(page, name, _, _, _, _, _) = pages !! pageIndex
                notebookRemovePage notebook pageIndex
                modifyIORef pagesRef (deleteBy (\(_, name1, _, _, _, _, _) (_, name2, _, _, _, _, _) -> name1 == name2) dataPage)
                modifyMVar_ stateRef $ \state -> return $ removeDataByNameFromTab currentTabIndex name state

    let
        mapOp (page, name, _, _, _, _, _) =
            do
                pageIndex <- notebookGetNPages notebook
                label <- labelWithButton (Just name) stockRemove (removeData page)
                notebookAppendPageMenu notebook page label label
                widgetShowAll label

    mapM_ mapOp dataPages
    
    (newPage, _, _, _, _, _) <- newDataPage
    newPageLabel <- labelWithImage (Just "Add data") (Just stockAdd)
    notebookAppendPageMenu notebook newPage newPageLabel newPageLabel
    widgetShowAll newPageLabel
    
    boxPackStart vBox notebook PackGrow 2
    
    let
        onClose response =
            do
                if response == ResponseOk
                    then 
                        do
                            pages <- readIORef pagesRef

                            modifyMVar_ stateRef $ \state ->
                                foldM (\state i -> 
                                        do
                                            let 
                                                dataPage@(_, name, _, color, symbolSettings, lineSettings, errorBars) = pages !! i
                                            newDataParamsValues <- getDataParamValues color symbolSettings lineSettings errorBars
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


                    else
                        return ()
                
        

    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Ok" ResponseOk

    widgetShowAll dialog
    response <- dialogRun dialog 

    newState <- onClose response

    widgetDestroy dialog
    return newState

onKeyDown :: StateRef -> String -> IO Bool
onKeyDown stateRef keyName =
    do
        state <- readMVar stateRef
        g <- getStdGen
        (currentTab, canvas) <- getCurrentGraphTab state
        w <- widgetGetAllocatedWidth canvas
        h <- widgetGetAllocatedHeight canvas
        
        let 
            graphTabParms = (graphTabs state) !! currentTab
            selectedGraph = graphTabSelection graphTabParms
            graphParms = (graphTabGraphs graphTabParms) !! selectedGraph 
                
        case graphPeriod graphParms of
            0 ->
                do
                    let
                         plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (w, h) g
     
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
                            widgetQueueDraw canvas
                            --drawGraph stateRef Nothing
                        )
            otherwise -> return True


onMouseMove :: StateRef -> (Double, Double) -> [Modifier] -> IO Bool
onMouseMove stateRef (x, y) modifiers = do
    state <- readMVar stateRef
    g <- getStdGen
    (currentTab, canvas) <- getCurrentGraphTab state
    w <- widgetGetAllocatedWidth canvas
    h <- widgetGetAllocatedHeight canvas
    let 
        graphTabParms = (graphTabs state) !! currentTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph 
        grphArea = getGraphArea state currentTab selectedGraph g
        period = graphPeriod graphParms
        graphSel = graphSelection graphParms
        scrArea = getScreenArea graphTabParms selectedGraph (w, h)
        (x1, y1) = toGraphCoords scrArea (toPhaseView grphArea period) (x, y)
        updatePlotSettings =
            do
                let
                    plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (w, h) g

                Plot.onMouseMove (x, y) modifiers plotSettings (\plotSettings ->
                    do
                        modifyMVar_ stateRef $ \state -> return $
                            updateGuiChanged True (updateGraphSettings state currentTab selectedGraph plotSettings)
                        widgetQueueDraw canvas
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
    w <- widgetGetAllocatedWidth canvas
    h <- widgetGetAllocatedHeight canvas

    let
        graphTabParms = (graphTabs state) !! currentTab

        maybeNewSelectedGraph = 
            findIndex (\(i, params) -> 
                let 
                    scrArea = getScreenArea graphTabParms i (w, h) 
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
                    widgetQueueDraw canvas
                    --drawGraph stateRef Nothing
                --RightButton -> do
                --        modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True $
                --            state {graphTabs = updateAt currentTab (graphTabParms {graphTabSelection = newSelectedGraph}) (graphTabs state)}
                --        widgetQueueDraw canvas
                --        --drawGraph stateRef Nothing
                otherwise -> return ()    
        otherwise -> return ()    
    ----------------------------------------------------------------------------

    state <- readMVar stateRef
    (currentTab, canvas) <- getCurrentGraphTab state
    w <- widgetGetAllocatedWidth canvas
    h <- widgetGetAllocatedHeight canvas
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
        scrArea = getScreenArea graphTabParms selectedGraph (w, h) 
        (x1, y1) = toGraphCoords scrArea (toPhaseView grphArea period) (x, y)
    case button of 
        LeftButton ->
            do
                let
                    plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (w, h) g
                Plot.onMouseButton button modifiers click (x, y) plotSettings (\plotSettings ->
                    do
                        modifyMVar_ stateRef $ \state -> return $
                            updateGuiChanged True (updateGraphSettings state currentTab selectedGraph plotSettings)
                        widgetQueueDraw canvas
                        --drawGraph stateRef Nothing
                    )
                return ()
        
        RightButton -> 
            case click of
                ReleaseClick ->
                    do
                        menu <- menuNew
                        settingsMenuItem <- menuItemNewWithLabel "Settings..."
                        on settingsMenuItem menuItemActivated (settingsDialog stateRef)
                        menuShellAppend menu settingsMenuItem

                        gnuSettingsMenuItem <- menuItemNewWithLabel "Gnuplot settings..."
                        on gnuSettingsMenuItem menuItemActivated (Gnu.paramsDialog stateRef)
                        menuShellAppend menu gnuSettingsMenuItem
                        
                        separator <- separatorMenuItemNew
                        menuShellAppend menu separator

                        --------------------------------------------------------
                        insertRowMenuItem <- menuItemNewWithLabel "Insert row"
                        menuInsertRow <- menuNew
                        menuItemSetSubmenu insertRowMenuItem menuInsertRow

                        insertRowBeforeMenuItem <- menuItemNewWithLabel "Before"
                        on insertRowBeforeMenuItem menuItemActivated  
                            (modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow, selectedCol) 1 0 state)
                        menuShellAppend menuInsertRow insertRowBeforeMenuItem

                        insertRowAfterMenuItem <- menuItemNewWithLabel "After"
                        on insertRowAfterMenuItem menuItemActivated 
                            (modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow + 1, selectedCol) 1 0 state)
                        menuShellAppend menuInsertRow insertRowAfterMenuItem

                        menuShellAppend menu insertRowMenuItem
                        --------------------------------------------------------
                        insertColMenuItem <- menuItemNewWithLabel "Insert column"
                        menuInsertCol <- menuNew
                        menuItemSetSubmenu insertColMenuItem menuInsertCol

                        insertColBeforeMenuItem <- menuItemNewWithLabel "Before"
                        on insertColBeforeMenuItem menuItemActivated 
                            (modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow, selectedCol) 0 1 state)
                        menuShellAppend menuInsertCol insertColBeforeMenuItem

                        insertColAfterMenuItem <- menuItemNewWithLabel "After"
                        on insertColAfterMenuItem menuItemActivated
                            (modifyMVar_ stateRef $ \state -> return $ insertGraphRowsCols currentTab (selectedRow, selectedCol + 1) 0 1 state)
                        menuShellAppend menuInsertCol insertColAfterMenuItem

                        menuShellAppend menu insertColMenuItem
                        --------------------------------------------------------

                        menuPopup menu (Just (button, timestamp))
                        widgetShowAll menu
                            
                otherwise -> return ()
        otherwise -> return ()
    return True


onMouseScroll :: StateRef -> (Double, Double) -> ScrollDirection -> IO Bool
onMouseScroll stateRef (x, y) direction = 
    do
        state <- readMVar stateRef
        (currentTab, canvas) <- getCurrentGraphTab state
        w <- widgetGetAllocatedWidth canvas
        h <- widgetGetAllocatedHeight canvas
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

            plotSettings = getPlotSettings state currentTab selectedGraph selectedGraph (w, h) g
            
        Plot.onMouseScroll (x, y) direction plotSettings (\plotSettings ->
            do
                modifyMVar_ stateRef $ \state -> return $
                    updateGuiChanged True (updateGraphSettings state currentTab selectedGraph plotSettings)
                        
                widgetQueueDraw canvas
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
            w <- widgetGetAllocatedWidth c
            h <- widgetGetAllocatedHeight c
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
            plot c settings maybeFileName
            modifyMVar_ stateRef $ \state -> return $ updateGuiChanged False state
    else 
        return ()

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
    
    dialog <- fileChooserDialogNew (Just "Save graph") (Just (getWindow state)) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*.PDF"
    fileFilter `fileFilterAddPattern` "*.pdf"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter
    widgetShowAll dialog
    response <- dialogRun dialog
    file <- fileChooserGetFilename (castToFileChooser dialog)
    if response == ResponseAccept && (file /= Nothing)
        then
            do
                widgetDestroy dialog
                modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
                drawGraph stateRef file
        else 
            widgetDestroy dialog

getPlotData :: (RandomGen g) => PlotArea -> GraphDataParams -> DataParams -> Double -> (Double, Double) -> g -> [PlotData]
getPlotData graphArea graphDataParams dataParams period (w, h) randomGen =
    let
            get2dData d =
                V.map (\(x, y, weight) -> ((toPhase x period, 0), (y, if graphDataParamsErrorBars graphDataParams && weight > 0 then sqrt (1 / weight) else 0))) $ values1 d
            get3dData = xys2
            sample2dData d = 
                let
                    xs = [xLeft, xLeft + xStep .. xRight] where
                        (xLeft, xRight) = (plotAreaLeft graphArea, plotAreaRight graphArea)
                        xStep = (xRight - xLeft) / w
                    --errors = foldr1 (\ys1 ys2 -> zipWith (\y1 y2 -> y1 + y1) ys1 ys2) $ map (AD.getValues (map (\x -> [x]) xs) randomGen) bsData
                in
                    V.fromList $ zipWith (\x y -> ((x, 0), (y, 0))) xs (AD.getValues (map (\x -> [x]) xs) randomGen d)
            sample3dData d = 
                let
                    (xLeft, xRight) = (plotAreaLeft graphArea, plotAreaRight graphArea)
                    xStep = (xRight - xLeft) / w / 10 
                    yStep = (plotAreaTop graphArea - plotAreaBottom graphArea) / h / 10 
                    xs = [[x1, x2] | x1 <- [xLeft, xLeft + xStep .. xRight], x2 <- [plotAreaBottom graphArea, plotAreaBottom graphArea + yStep .. plotAreaTop graphArea]]
                in
                    D.xys2 (U.sampleAnalyticData d [xLeft, plotAreaBottom graphArea] [xRight, plotAreaTop graphArea] [100, 75] randomGen)
            lineAttributes =
                case graphDataParamsLineWidth graphDataParams of
                    0 -> Nothing
                    otherwise ->
                        Just $ PlotLineAttributes {
                            plotLineDash = graphDataParamsLineDash graphDataParams,
                            plotLineWidth = graphDataParamsLineWidth graphDataParams,
                            plotLineColor = getRGBA $ graphDataParamsColor graphDataParams
                        }
            mapOp sdp = 
                case subData sdp of
                    Left d ->
                        case D.dim d of
                            1 -> if D.isData d
                                -- 2D data
                                then
                                    PlotData {
                                        plotDataValues = get2dData d,
                                        plotDataLineAttributes = lineAttributes,
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
                                        plotDataLineAttributes = lineAttributes,
                                        plotDataPointAttributes = Nothing
                                    }                 
                            2 -> 
                                -- 3D data or spectrum
                                PlotData3d {
                                    plotDataValues3d = get3dData d
                                }
                    Right (Left s) -> 
                            PlotData {
                                plotDataValues = sample2dData s,
                                plotDataLineAttributes = lineAttributes,
                                plotDataPointAttributes = Nothing
                            }                 
                    Right (Right f) ->
                        if AD.is2d f 
                        then 
                            PlotData {
                                plotDataValues = sample2dData f,
                                plotDataLineAttributes = lineAttributes,
                                plotDataPointAttributes = Nothing
                            }                 
                        else
                            PlotData3d {
                                plotDataValues3d = sample3dData f
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
                    sample d =
                        D.filterData (\(_, y, _) -> not (isNaN y) && not (isInfinite y)) $
                            if AD.is3d d
                                then U.sampleAnalyticData_ d [100, 100] g
                                else U.sampleAnalyticData_ d [1000] g
                    
                    getData (Left d) = d
                    getData (Right s) = either sample sample s 
                        
                    f xyz minOrMax = 
                        let 
                            vals = concat $
                                map (\dp -> map (\sdp ->
                                    let
                                        d = getData (subData sdp)
                                    in
                                        case xyz of
                                            0 ->
                                                if minOrMax
                                                    then
                                                        D.xMin1 d
                                                    else
                                                        D.xMax1 d
                                            1 -> 
                                                if minOrMax
                                                    then
                                                        if D.is2d d then D.yMin d else D.xMini 1 d  
                                                    else
                                                        if D.is2d d then D.yMax d else D.xMaxi 1 d  
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

setNotebookEvents :: StateRef -> IO ()
setNotebookEvents stateRef =
    do
        state <- readMVar stateRef
        let
            notebook = getGraphTabs state
            
        on notebook switchPage $
            \i ->
                do 
                    modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
                    numPages <- notebookGetNPages notebook
                    if numPages > 1 && i == numPages - 1 
                        then 
                            do
                                    addGraphTab stateRef Nothing
                                    notebookPrevPage notebook
                                    return ()
                        else return ()
            
        numPages <- notebookGetNPages notebook
    
        let    
            mapOp i = 
                do    
                    Just page <- notebookGetNthPage notebook i
                    widgetAddEvents page [ButtonMotionMask, ButtonPressMask, ButtonReleaseMask]
                    on page draw $ liftIO (drawGraph stateRef Nothing)
                    on page buttonPressEvent $
                        do 
                            button <- eventButton
                            modifiers <- eventModifier
                            click <- eventClick
                            (x, y) <- eventCoordinates
                            timestamp <- eventTime
                            liftIO $ TSA.GUI.Graph.onMouseButton stateRef button modifiers click (x, y) timestamp
                    on page buttonReleaseEvent $ 
                        do 
                            button <- eventButton
                            modifiers <- eventModifier
                            click <- eventClick
                            (x, y) <- eventCoordinates
                            timestamp <- eventTime
                            liftIO $ TSA.GUI.Graph.onMouseButton stateRef button modifiers click (x, y) timestamp
                    on page motionNotifyEvent $ 
                        do 
                            (x, y) <- eventCoordinates
                            modifiers <- eventModifier
                            liftIO $ TSA.GUI.Graph.onMouseMove stateRef (x, y) modifiers
                    on page scrollEvent $
                        do
                            (x, y) <- eventCoordinates
                            direction <- eventScrollDirection
                            liftIO $ TSA.GUI.Graph.onMouseScroll stateRef (x, y) direction
        mapM_ mapOp [0 .. numPages - 2] 

addGraphTab :: StateRef -> Maybe String -> IO Int
addGraphTab stateRef maybeGraphName =
    do
        state <- readMVar stateRef
        let
            notebook = getGraphTabs state
        pageIndex <- notebookGetNPages notebook
        page <- drawingAreaNew
        widgetModifyBg page StateNormal (Color 65535 65535 65535)
        let
            removeTab page =
                do
                    numPages <- notebookGetNPages notebook
                    if numPages > 2 
                        then
                            do
                                Just pageIndex <- notebookPageNum notebook page
                                if pageIndex == numPages - 2
                                    then
                                        notebookPrevPage notebook
                                    else
                                        return ()
                                notebookRemovePage notebook pageIndex
                                modifyMVar_ stateRef $ \state -> return $ state {graphTabs = (removeAt pageIndex (graphTabs state))}
                        else 
                            return ()
            tabName = 
                case maybeGraphName of
                    Nothing -> "Graph " ++ show pageIndex
                    Just graphName -> graphName
        label <- labelWithButton (Just tabName) stockRemove (removeTab page)
        
        case maybeGraphName of
            Nothing ->
                modifyMVar_ stateRef $ \state -> return $ state {graphTabs = (insertAt (pageIndex - 1) (newGraphTab tabName) (graphTabs state))}
            Just _ -> 
                return ()
        
        notebookInsertPageMenu notebook page label label (pageIndex - 1)
        notebookSetCurrentPage notebook (pageIndex - 1)
        setNotebookEvents stateRef
        widgetShowAll label
        widgetShowAll notebook
        return (pageIndex - 1)


