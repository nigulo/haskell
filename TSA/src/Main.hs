{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (Main.main) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import Data.GI.Base

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad (when, forM_)
import Debug.Trace
import System.CPUTime
import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.IO.Error
import System.Directory
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip
import Codec.Binary.UTF8.String
import Data.Int (Int32)
import Data.Word (Word32)

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
import qualified Utils.Xml as Xml

title :: Text
title = "Time Series Analysis"

main :: IO ()
main = do
    -- Set up UTF-8 encoding for Windows console (mirrors GUI/src/Main.hs;
    -- prevents hPutChar "cannot encode" crashes on non-ASCII stderr output,
    -- e.g. haskell-gi's disowned-pointer warning callstack bullets '•')
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    app <- Gtk.applicationNew (Just "org.tsa.app") []
    _ <- Gio.onApplicationActivate app (activate app)
    _ <- Gio.applicationRun app Nothing
    return ()

activate :: Gtk.Application -> IO ()
activate app = do
    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just ("Untitled - " <> title))
    Gtk.windowSetDefaultSize win 640 480

    notebook <- Gtk.notebookNew
    progressBar <- Gtk.progressBarNew

    statusBar <- Gtk.labelNew (Just "Welcome!")
    Gtk.widgetSetHalign statusBar Gtk.AlignStart

    stateRef <- newMVar $ (newState newParams) {
        guiParams =
            Just GuiParams {
                guiWindow = win,
                guiGraphTabs = notebook,
                guiProgressBar = (progressBar, 0),
                guiStatusBar = statusBar,
                guiMousePos = Nothing,
                guiLog = Nothing,
                guiChanged = True,
                guiFileName = ""
            }
        }

    _ <- Gtk.onWindowCloseRequest win $ do
        liftIO $ quit stateRef
        return False

    -- Create menu bar using GMenu
    menuBar <- createMenuBar app stateRef

    -- Setup notebook
    label <- labelWithImage (Just "New graph") (Just "list-add")
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    _ <- Gtk.notebookAppendPageMenu notebook hBox (Just label) (Just label)
    Gtk.widgetSetVisible label True

    addGraphTab stateRef Nothing
    Gtk.notebookSetCurrentPage notebook 0

    setNotebookEvents stateRef

    -- Key press handling
    keyController <- Gtk.eventControllerKeyNew
    _ <- Gtk.onEventControllerKeyKeyPressed keyController $ \keyval _keycode _mods -> do
        keyName <- Gdk.keyvalName keyval
        case keyName of
            Just name -> liftIO $ Graph.onKeyDown stateRef (T.unpack name)
            Nothing -> return False
        return False
    Gtk.widgetAddController win keyController

    -- Main layout
    vBox <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxAppend vBox menuBar
    Gtk.widgetSetVexpand notebook True
    Gtk.boxAppend vBox notebook
    Gtk.boxAppend vBox progressBar
    Gtk.boxAppend vBox statusBar
    Gtk.widgetSetVisible progressBar False

    Gtk.windowSetChild win (Just vBox)

    -- Timer for progress/status updates
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 200 $ do
        state <- readMVar stateRef
        let (pb, percent) = getProgressBar state
            sb = getStatusBar state
        Gtk.progressBarSetFraction pb percent
        if percent == 0
            then do
                Gtk.widgetSetVisible pb False
                Gtk.widgetSetVisible sb True
            else do
                Gtk.widgetSetVisible sb False
                Gtk.widgetSetVisible pb True
        return True

    -- Timer for GUI changed flag
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ do
        modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
        return True

    numCapabilities <- getNumCapabilities
    putStrLn $ "numCapabilities: " ++ show numCapabilities

    exists <- doesFileExist "current.stsz"
    if exists then
        loadState "current.stsz" stateRef
    else
        loadState "current.sts" stateRef

    Gtk.windowPresent win

createMenuBar :: Gtk.Application -> StateRef -> IO Gtk.Widget
createMenuBar app stateRef = do
    menuBar <- Gtk.popoverMenuBarNewFromModel (Nothing :: Maybe Gio.Menu)

    -- Create menu model
    menu <- Gio.menuNew

    -- File menu
    fileMenu <- Gio.menuNew
    Gio.menuAppendItem fileMenu =<< createMenuItem "New" "app.new"
    Gio.menuAppendItem fileMenu =<< createMenuItem "Load..." "app.load"
    Gio.menuAppendItem fileMenu =<< createMenuItem "Save" "app.save"
    Gio.menuAppendItem fileMenu =<< createMenuItem "Save As..." "app.saveas"
    Gio.menuAppendItem fileMenu =<< createMenuSeparator
    Gio.menuAppendItem fileMenu =<< createMenuItem "Import from ISDA..." "app.import"
    Gio.menuAppendItem fileMenu =<< createMenuItem "Export to ISDA..." "app.export"
    Gio.menuAppendItem fileMenu =<< createMenuSeparator
    Gio.menuAppendItem fileMenu =<< createMenuItem "Exit" "app.exit"
    Gio.menuAppendSubmenu menu (Just "File") fileMenu

    -- Data menu
    dataMenu <- Gio.menuNew
    Gio.menuAppendItem dataMenu =<< createMenuItem "Data sets..." "app.datasets"
    Gio.menuAppendItem dataMenu =<< createMenuItem "New..." "app.newdata"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Load..." "app.loaddata"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Function..." "app.function"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Statistics..." "app.statistic"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Modify..." "app.modify"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Build..." "app.build"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Sample..." "app.sample"
    Gio.menuAppendItem dataMenu =<< createMenuItem "Selection..." "app.selection"
    Gio.menuAppendSubmenu menu (Just "Data") dataMenu

    -- Transform menu
    transformMenu <- Gio.menuNew
    Gio.menuAppendItem transformMenu =<< createMenuItem "Interpolate..." "app.interpolate"
    Gio.menuAppendItem transformMenu =<< createMenuItem "Envelopes..." "app.envelope"
    Gio.menuAppendItem transformMenu =<< createMenuItem "FFT..." "app.fft"
    Gio.menuAppendItem transformMenu =<< createMenuItem "Analytic signal..." "app.analyticsignal"
    Gio.menuAppendItem transformMenu =<< createMenuItem "Local phase..." "app.localphase"
    Gio.menuAppendSubmenu menu (Just "Transform") transformMenu

    -- Regression menu
    regressionMenu <- Gio.menuNew
    Gio.menuAppendItem regressionMenu =<< createMenuItem "Least squares regression..." "app.lsq"
    Gio.menuAppendItem regressionMenu =<< createMenuItem "Bayesian linear regression..." "app.bayeslinreg"
    Gio.menuAppendSubmenu menu (Just "Regression") regressionMenu

    -- Analyze menu
    analyzeMenu <- Gio.menuNew
    Gio.menuAppendItem analyzeMenu =<< createMenuItem "Find period..." "app.findperiod"
    Gio.menuAppendItem analyzeMenu =<< createMenuItem "D2..." "app.d2"
    Gio.menuAppendItem analyzeMenu =<< createMenuItem "Find specific points..." "app.findspecificpoints"
    Gio.menuAppendItem analyzeMenu =<< createMenuItem "Find attractor..." "app.attractor"
    Gio.menuAppendItem analyzeMenu =<< createMenuItem "Find correlation..." "app.correlation"
    Gio.menuAppendSubmenu menu (Just "Analyze") analyzeMenu

    -- Graph menu
    graphMenu <- Gio.menuNew
    Gio.menuAppendItem graphMenu =<< createMenuItem "Data..." "app.graphdata"
    Gio.menuAppendItem graphMenu =<< createMenuItem "Settings..." "app.graphsettings"
    Gio.menuAppendItem graphMenu =<< createMenuItem "Markers..." "app.graphmarkers"
    Gio.menuAppendItem graphMenu =<< createMenuSeparator
    Gio.menuAppendItem graphMenu =<< createMenuItem "Gnuplot settings..." "app.gnuplotsettings"
    Gio.menuAppendItem graphMenu =<< createMenuItem "Preview graph as gnuplot..." "app.previewgnuplot"
    Gio.menuAppendItem graphMenu =<< createMenuItem "Save graph as gnuplot..." "app.savegnuplot"
    Gio.menuAppendItem graphMenu =<< createMenuItem "Save graph as PDF..." "app.savepdf"
    Gio.menuAppendSubmenu menu (Just "Graph") graphMenu

    -- Window menu
    windowMenu <- Gio.menuNew
    Gio.menuAppendItem windowMenu =<< createMenuItem "Preferences..." "app.preferences"
    Gio.menuAppendItem windowMenu =<< createMenuItem "Show log..." "app.showlog"
    Gio.menuAppendItem windowMenu =<< createMenuItem "Task manager..." "app.taskmanager"
    Gio.menuAppendSubmenu menu (Just "Window") windowMenu

    Gtk.popoverMenuBarSetMenuModel menuBar (Just menu)

    -- Register actions
    registerAction app "new" $ newProject stateRef
    registerAction app "load" $ loadDialog stateRef
    registerAction app "save" $ saveDialog stateRef
    registerAction app "saveas" $ saveAsDialog stateRef
    registerAction app "import" $ IO.importData stateRef
    registerAction app "export" $ readMVar stateRef >>= \state -> IO.exportData state
    registerAction app "exit" $ quit stateRef

    registerAction app "datasets" $ TSA.GUI.Data.infoDialog stateRef
    registerAction app "newdata" $ IO.newDataDialog stateRef
    registerAction app "loaddata" $ IO.loadDataDialog stateRef
    registerAction app "function" $ functionDialog stateRef
    registerAction app "statistic" $ statisticDialog stateRef
    registerAction app "modify" $ modifyDialog stateRef
    registerAction app "build" $ buildDialog stateRef
    registerAction app "sample" $ sampleDialog stateRef
    registerAction app "selection" $ selectionDialog stateRef

    registerAction app "interpolate" $ Interpolate.paramsDialog stateRef
    registerAction app "envelope" $ Env.paramsDialog stateRef
    registerAction app "fft" $ FFT.paramsDialog stateRef
    registerAction app "analyticsignal" $ analyticSignalDialog stateRef
    registerAction app "localphase" $ localPhaseDialog stateRef

    registerAction app "lsq" $ Lsq.paramsDialog stateRef
    registerAction app "bayeslinreg" $ TSA.GUI.Bayes.linRegWithMLIIDialog stateRef

    registerAction app "findperiod" $ findPeriodDialog stateRef
    registerAction app "d2" $ d2Dialog stateRef
    registerAction app "findspecificpoints" $ findSpecificPointsDialog stateRef
    registerAction app "attractor" $ attractorDialog stateRef
    registerAction app "correlation" $ correlationDialog stateRef

    registerAction app "graphdata" $ Graph.dataDialog stateRef
    registerAction app "graphsettings" $ Graph.settingsDialog stateRef
    registerAction app "graphmarkers" $ TSA.GUI.Markers.showMarkers stateRef
    registerAction app "gnuplotsettings" $ Gnu.paramsDialog stateRef
    registerAction app "previewgnuplot" $ Gnu.previewDialog stateRef
    registerAction app "savegnuplot" $ Gnu.plotDialog stateRef
    registerAction app "savepdf" $ printGraph stateRef

    registerAction app "preferences" $ TSA.GUI.Preferences.preferencesDialog stateRef
    registerAction app "showlog" $ TSA.GUI.Log.showLog stateRef
    registerAction app "taskmanager" $ TSA.GUI.TaskManager.taskManagerDialog stateRef

    Gtk.toWidget menuBar

createMenuItem :: Text -> Text -> IO Gio.MenuItem
createMenuItem label action = do
    item <- Gio.menuItemNew (Just label) (Just action)
    return item

createMenuSeparator :: IO Gio.MenuItem
createMenuSeparator = do
    section <- Gio.menuNew
    item <- Gio.menuItemNewSection Nothing section
    return item

registerAction :: Gtk.Application -> Text -> IO () -> IO ()
registerAction app name callback = do
    action <- Gio.simpleActionNew name Nothing
    _ <- Gio.onSimpleActionActivate action $ \_ -> callback
    Gio.actionMapAddAction app action

quit :: StateRef -> IO ()
quit stateRef = do
    state <- readMVar stateRef
    let settings = settingsParams state
    when (settingsSaveChangesOnExit settings) $
        saveDialog stateRef
    saveState (if settingsSaveInZippedFormat settings then "current.stsz" else "current.sts") state

--------------------------------------------------------------------------------
-- Saving and loading state
--------------------------------------------------------------------------------

loadDialog :: StateRef -> IO ()
loadDialog stateRef = do
    oldState <- readMVar stateRef
    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog "Load"

    -- Create file filter
    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter "*.tsa"
    Gtk.fileFilterAddPattern fileFilter "*.tsaz"
    Gtk.fileFilterSetName fileFilter (Just "TSA files")

    filters <- Gio.listStoreNew =<< glibType @Gtk.FileFilter
    Gio.listStoreAppend filters fileFilter
    Gtk.fileDialogSetFilters dialog (Just filters)
    Gtk.fileDialogSetDefaultFilter dialog (Just fileFilter)

    Gtk.fileDialogOpen dialog (Just (getWindow oldState)) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
        file <- Gtk.fileDialogOpenFinish dialog result
        maybePath <- Gio.fileGetPath file
        case maybePath of
            Just path -> do
                let fileName = path
                loadState fileName stateRef
                modifyMVar_ stateRef $ \state -> do
                    let gp = fromJust (guiParams state)
                    Gtk.windowSetTitle (guiWindow gp) (Just (T.pack fileName <> " - " <> title))
                    return $ state {
                            guiParams = Just (gp {guiFileName = fileName})
                        }
            Nothing -> return ()

newProject :: StateRef -> IO ()
newProject stateRef = do
    saveDialog stateRef
    state <- readMVar stateRef
    let Just gp = guiParams state
    numTabs <- Gtk.notebookGetNPages (guiGraphTabs gp)
    mapM_ (\_ -> Gtk.notebookRemovePage (guiGraphTabs gp) 0) [0 .. numTabs - 2]
    modifyMVar_ stateRef $ \state -> do
        let Just gp = guiParams state
        Gtk.windowSetTitle (guiWindow gp) (Just ("Untitled - " <> title))
        return $ (newState newParams) {
            guiParams = Just $ gp {
                guiMousePos = Nothing,
                guiFileName = ""
                }
            }
    addGraphTab stateRef Nothing
    Gtk.notebookSetCurrentPage (guiGraphTabs gp) 0

loadState :: String -> StateRef -> IO ()
loadState fileName stateRef = do
    exists <- doesFileExist fileName
    when exists $ do
        sState <- B.readFile fileName
        let isZipped f = last (map Char.toLower f) == 'z'
            decompressFunc bs = if isZipped fileName then decompress bs else bs
        TSA.GUI.State.readState stateRef (decode (B.unpack (decompressFunc sState)))

        state <- readMVar stateRef
        let notebook = getGraphTabs state
            gts = graphTabs state
            settings = settingsParams state

        numPages <- Gtk.notebookGetNPages notebook
        mapM_ (\_ -> Gtk.notebookRemovePage notebook 0) [0 .. numPages - 2]
        mapM_ (\i -> addGraphTab stateRef (Just (graphTabName (gts !! i)))) [0 .. length gts - 1]
        Gtk.notebookSetCurrentPage notebook (fromIntegral $ settingsActiveTab settings)
        Gtk.widgetSetVisible notebook True
        modifyMVar_ stateRef $ \state ->
            return $ state {
                    settingsParams = settings {settingsSaveInZippedFormat = isZipped fileName}
                }

saveDialog :: StateRef -> IO ()
saveDialog stateRef = do
    state <- readMVar stateRef
    case guiFileName (fromJust (guiParams state)) of
        "" -> saveAsDialog stateRef
        fileName -> saveState fileName state

saveAsDialog :: StateRef -> IO ()
saveAsDialog stateRef = do
    state <- readMVar stateRef
    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog "Save"

    let settings = settingsParams state
        suffix = if settingsSaveInZippedFormat settings then ".tsaz" else ".tsa"

    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter (T.pack $ "*" ++ suffix)
    Gtk.fileFilterSetName fileFilter (Just "TSA files")

    filters <- Gio.listStoreNew =<< glibType @Gtk.FileFilter
    Gio.listStoreAppend filters fileFilter
    Gtk.fileDialogSetFilters dialog (Just filters)
    Gtk.fileDialogSetDefaultFilter dialog (Just fileFilter)

    Gtk.fileDialogSave dialog (Just (getWindow state)) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
        file <- Gtk.fileDialogSaveFinish dialog result
        maybePath <- Gio.fileGetPath file
        case maybePath of
            Just path -> do
                let f = path
                    fileName = if suffix `List.isSuffixOf` (map Char.toLower f) then f else f ++ suffix
                saveState fileName state
                modifyMVar_ stateRef $ \state -> do
                    let gp = fromJust (guiParams state)
                    Gtk.windowSetTitle (guiWindow gp) (Just (T.pack fileName <> " - " <> title))
                    return $ state {guiParams = Just (gp {guiFileName = fileName})}
            Nothing -> return ()

saveState :: String -> State -> IO ()
saveState fileName state = do
    (currentGraphTab, _) <- getCurrentGraphTab state
    let settings = settingsParams state
        newState = state {settingsParams = settings {settingsActiveTab = currentGraphTab}}
        byteStr = B.pack (encode (Xml.render (Xml.toDocument newState)))
    if settingsSaveInZippedFormat settings
        then B.writeFile fileName (compress byteStr)
        else B.writeFile fileName byteStr
