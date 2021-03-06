
module Main (Main.main) where

import Graphics.UI.Gtk
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class
import Debug.Trace
import System.CPUTime
import System.IO.Error
import System.Directory
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip
import Codec.Binary.UTF8.String

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.IO as IO
import TSA.GUI.Gnu as Gnu
import TSA.GUI.Graph as Graph
import TSA.GUI.Interpolate as Interpolate
import TSA.GUI.LeastSquares as Lsq
import TSA.GUI.Envelopes as Env
import TSA.GUI.FFT as FFT
import TSA.GUI.Modify
import TSA.GUI.Selection
import TSA.GUI.Build
import TSA.GUI.AnalyticSignal
import TSA.GUI.LocalPhase
import TSA.GUI.D2
import TSA.GUI.Period
import TSA.GUI.SpecificPoints
import TSA.GUI.Attractor
import TSA.GUI.Function
import TSA.GUI.Statistic
import TSA.GUI.Correlation
import TSA.GUI.Sample
import TSA.GUI.Common
import TSA.GUI.Preferences
import TSA.GUI.Log
import TSA.GUI.TaskManager
import TSA.GUI.Markers
import TSA.GUI.Bayes

import GUI.Widget

import Utils.Misc

import Data.Maybe
--import Text.XML.HXT.Arrow
import qualified Utils.Xml as Xml
--test

title = "Time Series Analysis" 

main = do
    --initGUI
    unsafeInitGUIForThreadedRTS
    
    uiDef <- readFile "ui.xml"
    win <- windowNew

    icon <- pixbufNewFromFile "tsa.bmp"
    win `set` [windowTitle := "Untitled - " ++ title, windowIcon := Just icon]
    
    notebook <- notebookNew
    progressBar <- progressBarNew
    
    let
        statusBarText = "Welcome!"
    statusBar <- statusbarNew
    contextId <- statusbarGetContextId statusBar "Info"
    messageId <- statusbarPush statusBar contextId statusBarText

    stateRef <- newMVar $ (newState newParams) {
        guiParams = 
            Just GuiParams {
                guiWindow = win,
                guiGraphTabs = notebook,
                guiProgressBar = (progressBar, 0),
                guiStatusBar = (statusBar, statusBarText, contextId, messageId),
                guiMousePos = Nothing,
                guiLog = Nothing,
                guiChanged = True,
                guiFileName = ""
            }
        }
    
    win `on` objectDestroy $ liftIO (quit stateRef)

    label <- labelWithImage (Just "New graph") (Just stockAdd)
    hBox <- hBoxNew False 0
    notebookAppendPageMenu notebook hBox label label
    widgetShowAll label
    
    addGraphTab stateRef Nothing
    notebookSetCurrentPage notebook 0
    
    setNotebookEvents stateRef
    
    -- Create the menus
    fileAct <- actionNew "FileAction" "File" Nothing Nothing
    
    newAct <- actionNew "NewAction" "New"
          (Just "New project")
          (Just stockNew)
    on newAct actionActivated (newProject stateRef)

    loadAct <- actionNew "LoadAction" "Load..."
          (Just "Load project")
          (Just stockOpen)
    on loadAct actionActivated (loadDialog stateRef)

    saveAct <- actionNew "SaveAction" "Save"
          (Just "Save project")
          (Just stockSave)
    on saveAct actionActivated (saveDialog stateRef)

    saveAsAct <- actionNew "SaveAsAction" "Save as..."
          (Just "Save project as")
          (Just stockSaveAs)
    on saveAsAct actionActivated (saveAsDialog stateRef)

    ----------------------------------------------------------------------------

    importAct <- actionNew "ImportAction" "Import from ISDA..."
          (Just "Import spectrum or data from ISDA")
          (Just stockOpen)
    on importAct actionActivated (IO.importData stateRef)

    exportAct <- actionNew "ExportAction" "Export to ISDA..."
          (Just "Export spectrum or data to ISDA")
          (Just stockSave)
    on exportAct actionActivated (readMVar stateRef >>= \state -> IO.exportData state)

    ----------------------------------------------------------------------------

    exitAct <- actionNew "ExitAction" "Exit"
          (Just "Exit from TSA")
          (Just stockQuit)
    on exitAct actionActivated (quit stateRef)

    ----------------------------------------------------------------------------

    dataAct <- actionNew "DataAction" "Data" Nothing Nothing

    dataSetsAct <- actionNew "DataSetsAction" "Data sets... " Nothing Nothing
    on dataSetsAct actionActivated (TSA.GUI.Data.infoDialog stateRef)

    newDataAct <- actionNew "NewDataAction" "New..."
          (Just "Create new data")
          (Just stockOpen)
    on newDataAct actionActivated (IO.newDataDialog stateRef)

    loadDataAct <- actionNew "LoadDataAction" "Load..."
          (Just "Load data from text file")
          (Just stockOpen)
    on loadDataAct actionActivated (IO.loadDataDialog stateRef)


    functionAct <- actionNew "FunctionAction" "Function... " Nothing Nothing
    on functionAct actionActivated (functionDialog stateRef)

    statisticAct <- actionNew "StatisticAction" "Statistics... " Nothing Nothing
    on statisticAct actionActivated (statisticDialog stateRef)

    modifyAct <- actionNew "ModifyAction" "Modify... " Nothing Nothing
    on modifyAct actionActivated (modifyDialog stateRef)

    buildAct <- actionNew "BuildAction" "Build... " Nothing Nothing
    on buildAct actionActivated (buildDialog stateRef)

    sampleAct <- actionNew "SampleAction" "Sample... " Nothing Nothing
    on sampleAct actionActivated (sampleDialog stateRef)

    selectionAct <- actionNew "SelectionAction" "Selection... " Nothing Nothing
    on selectionAct actionActivated (selectionDialog stateRef)

    ----------------------------------------------------------------------------
    transformAct <- actionNew "TransformAction" "Transform" Nothing Nothing

    interpAct <- actionNew "InterpolateAction" "Interpolate... " Nothing Nothing
    on interpAct actionActivated (Interpolate.paramsDialog stateRef)

    envelopeAct <- actionNew "EnvelopeAction" "Envelopes... " Nothing Nothing
    on envelopeAct actionActivated (Env.paramsDialog stateRef)
    
    fftAct <- actionNew "FftAction" "FFT... " Nothing Nothing
    on fftAct actionActivated (FFT.paramsDialog stateRef)

    asAct <- actionNew "AsAction" "Analytic signal... " Nothing Nothing
    on asAct actionActivated (analyticSignalDialog stateRef)

    localPhaseAct <- actionNew "LocalPhaseAction" "Local phase... " Nothing Nothing
    on localPhaseAct actionActivated (localPhaseDialog stateRef)

    ----------------------------------------------------------------------------
    regressionAct <- actionNew "RegressionAction" "Regression" Nothing Nothing

    lsqAct <- actionNew "LsqAction" "Least squares regression... " Nothing Nothing
    on lsqAct actionActivated (Lsq.paramsDialog stateRef)

    bayesLinRegAct <- actionNew "BayesLinRegAction" "Bayesian linear regression... " Nothing Nothing
    on bayesLinRegAct actionActivated (TSA.GUI.Bayes.linRegWithMLIIDialog stateRef)

    ----------------------------------------------------------------------------
    analyzeAct <- actionNew "AnalyzeAction" "Analyze" Nothing Nothing

    findPeriodAct <- actionNew "FindPeriodAction" "Find period... " Nothing Nothing
    on findPeriodAct actionActivated (findPeriodDialog stateRef)

    d2Act <- actionNew "D2Action" "D2... " Nothing Nothing
    on d2Act actionActivated (d2Dialog stateRef)

    findSpecificPointsAct <- actionNew "FindSpecificPointsAction" "Find specific points... " Nothing Nothing
    on findSpecificPointsAct actionActivated (findSpecificPointsDialog stateRef)

    attractorAct <- actionNew "AttractorAction" "Find attractor... " Nothing Nothing
    on attractorAct actionActivated (attractorDialog stateRef)

    correlationAct <- actionNew "CorrelationAction" "Find correlation... " Nothing Nothing
    on correlationAct actionActivated (correlationDialog stateRef)

    ----------------------------------------------------------------------------

    graphAct <- actionNew "GraphAction" "Graph" Nothing Nothing

    graphDataAct <- actionNew "GraphDataAction" "Data... " Nothing (Just stockProperties)
    on graphDataAct actionActivated (Graph.dataDialog stateRef)

    graphSettingsAct <- actionNew "GraphSettingsAction" "Settings... " Nothing (Just stockProperties)
    on graphSettingsAct actionActivated (Graph.settingsDialog stateRef)

    graphMarkersAct <- actionNew "GraphMarkersAction" "Markers... " Nothing (Just stockProperties)
    on graphMarkersAct actionActivated (TSA.GUI.Markers.showMarkers stateRef)

    ----------------------------------------------------------------------------

    saveGnuplotAct <- actionNew "SaveGnuplotAction" "Save graph as gnuplot..."
          (Just "Save graph as gnuplot")
          (Just stockSave)
    on saveGnuplotAct actionActivated (Gnu.plotDialog stateRef)

    previewGnuplotAct <- actionNew "PreviewGnuplotAction" "Preview graph as gnuplot... " Nothing (Just stockPrintPreview)
    on previewGnuplotAct actionActivated (Gnu.previewDialog stateRef)

    gnuplotSettingsAct <- actionNew "GnuplotSettingsAction" "Gnuplot settings... " Nothing (Just stockProperties)
    on gnuplotSettingsAct actionActivated (Gnu.paramsDialog stateRef)
    
    ----------------------------------------------------------------------------

    savePDFAct <- actionNew "SavePDFAction" "Save graph as PDF..."
          (Just "Save graph as PDF")
          (Just stockPrint)
    on savePDFAct actionActivated (printGraph stateRef)
    
    ----------------------------------------------------------------------------
    windowAct <- actionNew "WindowAction" "Window" Nothing Nothing

    preferencesAct <- actionNew "PreferencesAction" "Preferences..." Nothing (Just stockPreferences)
    on preferencesAct actionActivated (TSA.GUI.Preferences.preferencesDialog stateRef)

    showLogAct <- actionNew "ShowLogAction" "Show log..." Nothing (Just stockInfo)
    on showLogAct actionActivated (TSA.GUI.Log.showLog stateRef)

    taskManagerAct <- actionNew "TaskManagerAction" "Task manager..." Nothing (Just stockInfo)
    on taskManagerAct actionActivated (TSA.GUI.TaskManager.taskManagerDialog stateRef)
    
    ----------------------------------------------------------------------------
    on win keyPressEvent $
        do
            keyName <- eventKeyName 
            liftIO $ Graph.onKeyDown stateRef (glibToString keyName)
    
    {-
    let
        redrawFunc = 
            do
                win <-  eventWindow
                liftIO $ modifyMVar_ stateRef $ \state -> return $ updateGuiChanged False state
                liftIO $ do
                    state <- readMVar stateRef
                    (currentTab, canvas) <- getCurrentGraphTab state
                    widgetQueueDraw canvas
                return True
    
    on win exposeEvent redrawFunc
    on win focusOutEvent redrawFunc
    -}
    
    ----------------------------------------------------------------------------
    standardGroup <- actionGroupNew "standard"
    mapM_ (actionGroupAddAction standardGroup) [fileAct, dataAct, transformAct, regressionAct, analyzeAct,  graphAct, windowAct]
    mapM_ (actionGroupAddAction standardGroup)
      [newAct,
       loadAct,
       saveAct,
       saveAsAct,
       importAct,
       exportAct,
       exitAct,
       ----------- 
       dataSetsAct, 
       loadDataAct, 
       newDataAct, 
       functionAct,
       statisticAct,
       modifyAct,
       buildAct,
       sampleAct,
       selectionAct,
       ----------- 
       interpAct,
       envelopeAct, 
       fftAct,
       asAct,
       localPhaseAct,
       ----------- 
       lsqAct,
       bayesLinRegAct,
       ----------- 
       findPeriodAct,
       d2Act,
       findSpecificPointsAct,
       attractorAct,
       correlationAct,
       ----------- 
       graphDataAct,
       graphSettingsAct,
       graphMarkersAct,
       gnuplotSettingsAct,
       previewGnuplotAct,
       saveGnuplotAct,
       savePDFAct,
       -----------
       preferencesAct,
       showLogAct,
       taskManagerAct
       ]
    
    ui <- uiManagerNew 
    uiManagerAddUiFromString ui uiDef
    
    ui `uiManagerGetAction` "/ui"
    
    uiManagerInsertActionGroup ui standardGroup 0
    
    (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
    
    vBox <- vBoxNew False 0
    set vBox [boxHomogeneous := False]
    boxPackStart vBox menuBar PackNatural 0
    boxPackStart vBox notebook PackGrow 0
    
    boxPackStart vBox progressBar PackNatural 0
    boxPackStart vBox statusBar PackNatural 0
    widgetHide progressBar
    
    containerAdd win vBox

    _ <- timeoutAdd (
        do
            state <- readMVar stateRef
            -- Update progressBar and statusBar
            let 
                (pb, percent) = getProgressBar state
                (sb, text, contextId, messageId) = getStatusBar state
            oldPercent <- progressBarGetFraction pb 
            if percent /= oldPercent 
                then
                    pb `progressBarSetFraction` percent
                else
                    return ()
            if percent == 0 
                then
                    do 
                        widgetHide pb
                        widgetShow sb
                else
                    do
                        widgetHide sb
                        widgetShow pb
                    
            statusbarRemove sb contextId messageId
            newMessageId <- statusbarPush sb contextId text
            modifyMVar_ stateRef $ \state -> return $ state {guiParams = Just ((fromJust (guiParams state)) {guiStatusBar = (sb, text, contextId, newMessageId)})}
            return True
        ) 200

    _ <- timeoutAdd (
        do
            modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
            return True
        ) 1000

    numCapabilities <- getNumCapabilities
    putStrLn $ "numCapabilities: " ++ (show numCapabilities)
    exists <- doesFileExist "current.stsz"
    if exists then
        loadState "current.stsz" stateRef
    else
        loadState "current.sts" stateRef


    windowResize win 640 480
    widgetShowAll win
    mainGUI

quit stateRef =
    do
        state <- readMVar stateRef
        let
            settings = settingsParams state
        if settingsSaveChangesOnExit settings 
            then 
                saveDialog stateRef 
            else 
                return ()
        saveState (if settingsSaveInZippedFormat settings then "current.stsz" else "current.sts") state
        mainQuit


--------------------------------------------------------------------------------
-- Saving and loading state
--------------------------------------------------------------------------------


loadDialog :: StateRef -> IO ()
loadDialog stateRef = do
    oldState <- readMVar stateRef
    dialog <- fileChooserDialogNew (Just "Load") (Just (getWindow oldState)) FileChooserActionOpen [("Cancel", ResponseCancel), ("Open", ResponseAccept)]

    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*.tsa"
    fileFilter `fileFilterAddPattern` "*.tsaz"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter

    widgetShowAll dialog
    response <- dialogRun dialog
    file <- fileChooserGetFilename (castToFileChooser dialog)
    
    if response == ResponseAccept && (file /= Nothing)
        then
            do
                let
                    Just fileName = file 
                widgetDestroy dialog
                loadState fileName stateRef
                modifyMVar_ stateRef $ \state -> do 
                    let
                        gp = fromJust (guiParams state)
                    (guiWindow gp) `set` [windowTitle := fileName ++ " - " ++ title]
                    return $ state {
                            guiParams = Just (gp {guiFileName = fileName})
                        }

        else
            do
                widgetDestroy dialog

newProject :: StateRef -> IO ()
newProject stateRef = do
    saveDialog stateRef
    state <- readMVar stateRef
    let
        Just gp = guiParams state
    numTabs <- notebookGetNPages (guiGraphTabs gp) 
    mapM_ (\_ -> notebookRemovePage (guiGraphTabs gp) 0) [0 .. numTabs - 2]
    modifyMVar_ stateRef $ \state -> do
        let
            Just gp = guiParams state
        (guiWindow gp) `set` [windowTitle := "Untitled - " ++ title]
        return $ (newState newParams) {
            guiParams = Just $ gp {
                guiMousePos = Nothing,
                guiFileName = ""
                }
            }
    addGraphTab stateRef Nothing
    notebookSetCurrentPage (guiGraphTabs gp) 0

loadState :: String -> StateRef -> IO ()
loadState fileName stateRef = 
    do
        exists <- doesFileExist fileName
        if exists
            then
                do
                    sState <- B.readFile fileName
                    let
                        isZipped f = last (map Char.toLower f) == 'z'
                        decompressFunc bs = if isZipped fileName then decompress bs else bs
                    TSA.GUI.State.readState stateRef (decode (B.unpack (decompressFunc sState)))
                
                    state <- readMVar stateRef
                    let
                        notebook = getGraphTabs state
                        gts = graphTabs state
                        settings = settingsParams state
                    
                    numPages <- notebookGetNPages notebook
                    mapM_ (\i -> notebookRemovePage notebook 0) [0 .. numPages - 2]
                    mapM_ (\i -> addGraphTab stateRef (Just (graphTabName (gts !! i)))) [0 .. length gts - 1]
                    notebookSetCurrentPage notebook (settingsActiveTab settings)
                    widgetShowAll notebook
                    modifyMVar_ stateRef $ \state -> 
                        return $ state {
                                settingsParams = settings {settingsSaveInZippedFormat = isZipped fileName}
                            }
                    
            else return ()

saveDialog :: StateRef -> IO ()
saveDialog stateRef = do
    state <- readMVar stateRef
    case guiFileName (fromJust (guiParams state)) of
        "" -> saveAsDialog stateRef
        fileName -> 
            saveState fileName state

saveAsDialog :: StateRef -> IO ()
saveAsDialog stateRef = do
    state <- readMVar stateRef
    dialog <- fileChooserDialogNew (Just "Save") (Just (getWindow state)) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    
    fileFilter <- fileFilterNew
    let
        settings = settingsParams state
        suffix = if settingsSaveInZippedFormat settings then ".tsaz" else ".tsa"
    fileFilter `fileFilterAddPattern` ("*" ++ suffix)

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter
    
    widgetShowAll dialog
    response <- dialogRun dialog
    file <- fileChooserGetFilename (castToFileChooser dialog)
    
    if response == ResponseAccept && (file /= Nothing)
        then
            do
                widgetDestroy dialog
                let 
                    Just f = file
                    fileName = if suffix `List.isSuffixOf` (map Char.toLower f) then f else f ++ suffix
                saveState fileName state
                modifyMVar_ stateRef $ \state -> do
                    let
                        gp = fromJust (guiParams state)
                    (guiWindow gp) `set` [windowTitle := fileName ++ " - " ++ title]
                    return $ state {guiParams = Just (gp {guiFileName = fileName})}
        else 
            widgetDestroy dialog

saveState :: String -> State -> IO ()
saveState fileName state = 
    do
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            settings = settingsParams state
            newState = state {settingsParams = settings {settingsActiveTab = currentGraphTab}}
            byteStr = B.pack (encode (Xml.render (Xml.toDocument newState)))
        if settingsSaveInZippedFormat settings 
            then
                B.writeFile fileName (compress byteStr)
            else
                B.writeFile fileName byteStr

            
