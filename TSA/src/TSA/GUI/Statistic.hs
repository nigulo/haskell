{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Statistic (statisticDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)
import TSA.GUI.Data
import TSA.Data
import Utils.Concurrent
import Utils.List
import Utils.Str
import GUI.Plot

import Data.Map
import Data.List as List
import qualified Data.Vector.Unboxed as V
import Data.IORef

import Math.Function as F
import Regression.Statistic as S
import Regression.Data as D
import Regression.AnalyticDataWrapper
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Applicative
import System.Random
import Debug.Trace

statisticDialog :: StateRef -> IO ()
statisticDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state

    dialog <- dialogWithTitle state "Statistics"
    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8
    notebook <- Gtk.notebookNew

    pagesRef <- newIORef []

    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

        ------------------------------------------------------------------------
        deleteStatistic page =
            do
                pageIndex <- Gtk.notebookPageNum notebook page
                pages <- readIORef pagesRef
                let dataPage@(page, name, _) = pages !! (fromIntegral pageIndex)
                Gtk.notebookRemovePage notebook pageIndex
                modifyIORef pagesRef (\pages ->
                    deleteBy (\(_, name1, _) (_, name2, _) -> name1 == name2) dataPage pages)
                modifyStateParams stateRef $ \params -> removeStatisticByName name params

        ------------------------------------------------------------------------
        addPageToNotebook (page, maybeName, _) =
            do
                pageIndex <- Gtk.notebookGetNPages notebook
                label <-
                    case maybeName of
                        Just name -> labelWithButton (Just (T.pack name)) "edit-delete" (deleteStatistic page)
                        otherwise -> labelWithImage (Just "Add statistic") (Just "list-add")
                _ <- Gtk.notebookInsertPageMenu notebook page (Just label) (Just label) (max (pageIndex - 1) 0)
                return ()

        ------------------------------------------------------------------------
        showMessageBox :: String -> IO ()
        showMessageBox msg =
            do
                messageWindow <- Gtk.windowNew
                Gtk.windowSetTitle messageWindow (Just "Message")
                Gtk.windowSetModal messageWindow True
                Gtk.windowSetTransientFor messageWindow (Just dialog)
                vBox <- Gtk.boxNew Gtk.OrientationVertical 8
                Gtk.widgetSetMarginTop vBox 16
                Gtk.widgetSetMarginBottom vBox 16
                Gtk.widgetSetMarginStart vBox 16
                Gtk.widgetSetMarginEnd vBox 16
                label <- Gtk.labelNew (Just (T.pack msg))
                Gtk.boxAppend vBox label
                buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
                Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
                okButton <- Gtk.buttonNewWithLabel "Ok"
                Gtk.boxAppend buttonBox okButton
                Gtk.boxAppend vBox buttonBox
                _ <- Gtk.onButtonClicked okButton $ Gtk.windowDestroy messageWindow
                Gtk.windowSetChild messageWindow (Just vBox)
                Gtk.windowPresent messageWindow

        ------------------------------------------------------------------------
        createPage :: Maybe StatisticParams -> IO () --(VBox, String, Entry)
        createPage pageDef =
            do

                page <- Gtk.boxNew Gtk.OrientationVertical 0

                case pageDef of
                    Nothing ->
                        do
                            let
                                statParams = last $ statisticParams (params state)
                                commonParams = statisticCommonParams statParams
                            nameEntry <- Gtk.entryNew
                            entrySetText nameEntry (commonName commonParams)
                            addWidgetToBox (Just "Name: ") nameEntry page

                            statisticTextBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
                            Gtk.textBufferSetText statisticTextBuffer (T.pack (statisticDefinition statParams)) (-1)
                            statisticTextView <- Gtk.textViewNewWithBuffer statisticTextBuffer
                            Gtk.textViewSetMonospace statisticTextView True
                            Gtk.textViewSetAcceptsTab statisticTextView False

                            scrolledWindow <- Gtk.scrolledWindowNew
                            Gtk.scrolledWindowSetChild scrolledWindow (Just statisticTextView)

                            statisticFrame <- Gtk.frameNew (Just "Definition")
                            Gtk.frameSetChild statisticFrame (Just scrolledWindow)
                            addWidgetToBox Nothing statisticFrame page

                            hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
                            addButton <- Gtk.buttonNewWithLabel "Add"
                            Gtk.boxAppend hBox addButton
                            _ <- Gtk.onButtonClicked addButton $
                                do
                                    state <- readMVar stateRef
                                    name <- entryGetString nameEntry
                                    startIter <- Gtk.textBufferGetStartIter statisticTextBuffer
                                    endIter <- Gtk.textBufferGetEndIter statisticTextBuffer
                                    text <- Gtk.textBufferGetText statisticTextBuffer startIter endIter False
                                    let
                                        statParams = last $ statisticParams (params state)
                                        commonParams = statisticCommonParams statParams
                                        newCommonParams = updateCommonParams name commonParams
                                        newStatParams = statParams {statisticCommonParams = newCommonParams}
                                    modifyStateParams stateRef $ \params -> updateStatistic newStatParams params
                                    modifyStateParams stateRef $ \params -> addStatistic name (T.unpack text) params
                                    state <- readMVar stateRef

                                    entrySetText nameEntry (getNameWithNo newCommonParams)
                                    createPage $ Just (getStatisticByName name (params state))
                                    pageIndex <- Gtk.notebookGetNPages notebook
                                    Gtk.notebookSetCurrentPage notebook (pageIndex - 2)

                            addWidgetToBox Nothing hBox page
                            addPageToNotebook (page, Nothing, statisticTextBuffer)

                    Just statParams ->
                        do
                            let
                                name = (commonName . statisticCommonParams) statParams

                            vBox1 <- Gtk.boxNew Gtk.OrientationVertical 0
                            statisticTextBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
                            Gtk.textBufferSetText statisticTextBuffer (T.pack (statisticDefinition statParams)) (-1)
                            statisticTextView <- Gtk.textViewNewWithBuffer statisticTextBuffer
                            Gtk.textViewSetMonospace statisticTextView True
                            Gtk.textViewSetAcceptsTab statisticTextView False

                            scrolledWindow <- Gtk.scrolledWindowNew
                            Gtk.scrolledWindowSetChild scrolledWindow (Just statisticTextView)

                            statisticFrame <- Gtk.frameNew (Just "Definition")
                            Gtk.frameSetChild statisticFrame (Just scrolledWindow)
                            addWidgetToBox Nothing statisticFrame vBox1

                            hBox1 <- Gtk.boxNew Gtk.OrientationHorizontal 0
                            saveButton <- Gtk.buttonNewWithLabel "Save changes"
                            Gtk.boxAppend hBox1 saveButton
                            addWidgetToBox Nothing hBox1 vBox1

                            vBox2 <- Gtk.boxNew Gtk.OrientationVertical 0
                            dataChooser <- dataSetChooserNew (\_ -> True) state
                            addWidgetToBox Nothing (dataSetChooserToWidget dataChooser) vBox2

                            nameEntry <- Gtk.entryNew
                            entrySetText nameEntry name
                            addWidgetToBox (Just "Name:") nameEntry vBox2

                            varValDefsEntry <- Gtk.entryNew
                            entrySetText varValDefsEntry (statisticVarValsDef statParams)
                            addWidgetToBox (Just "Variable values: ") varValDefsEntry vBox2

                            hBox2 <- Gtk.boxNew Gtk.OrientationHorizontal 0
                            applyButton <- Gtk.buttonNewWithLabel "Apply to data"
                            Gtk.boxAppend hBox2 applyButton
                            addWidgetToBox Nothing hBox2 vBox2

                            vPane <- Gtk.panedNew Gtk.OrientationVertical
                            Gtk.panedSetStartChild vPane (Just vBox1)
                            Gtk.panedSetEndChild vPane (Just vBox2)
                            Gtk.boxAppend page vPane

                            _ <- Gtk.onButtonClicked saveButton $
                                do
                                    startIter <- Gtk.textBufferGetStartIter statisticTextBuffer
                                    endIter <- Gtk.textBufferGetEndIter statisticTextBuffer
                                    statisticStr <- Gtk.textBufferGetText statisticTextBuffer startIter endIter False
                                    varValDefsStr <- entryGetString varValDefsEntry
                                    modifyStateParams stateRef $ \params -> updateStatistic (
                                        statParams {statisticDefinition = T.unpack statisticStr,
                                        statisticVarValsDef = varValDefsStr
                                    }) params
                            _ <- Gtk.onButtonClicked applyButton $
                                do
                                    startIter <- Gtk.textBufferGetStartIter statisticTextBuffer
                                    endIter <- Gtk.textBufferGetEndIter statisticTextBuffer
                                    text <- Gtk.textBufferGetText statisticTextBuffer startIter endIter False
                                    dataParams <- dataSetChooserGetChoice dataChooser
                                    let
                                        statistic = S.statistic (T.unpack text)
                                        dataFuncs = S.dataFuncs statistic
                                        applyStatistic =
                                            do
                                                startIter <- Gtk.textBufferGetStartIter statisticTextBuffer
                                                endIter <- Gtk.textBufferGetEndIter statisticTextBuffer
                                                text <- Gtk.textBufferGetText statisticTextBuffer startIter endIter False
                                                name <- entryGetString nameEntry
                                                varValDefsStr <- entryGetString varValDefsEntry
                                                (currentGraphTab, _) <- getCurrentGraphTab state
                                                tEnv <- taskEnv stateRef

                                                let
                                                    graphTabParms = (graphTabs state) !! currentGraphTab
                                                    selectedGraph = graphTabSelection graphTabParms
                                                    statistic = S.statistic (T.unpack text)
                                                    numVars = length (S.varNames statistic)

                                                    varValDefs = List.map (\str -> List.map (\s -> F.function (trim s)) (splitBy ',' str)) $
                                                        List.map trim $ splitBy ';' $
                                                        replaceAll "left" ("" ++ show (plotAreaLeft ga)) $
                                                        replaceAll "right" ("" ++ show (plotAreaRight ga)) $
                                                        replaceAll "bottom" ("" ++ show (plotAreaBottom ga)) $
                                                        replaceAll "top" ("" ++ show (plotAreaTop ga)) varValDefsStr

                                                if numVars > length varValDefs then
                                                    do
                                                        showMessageBox "Variable value definitions must be separated by \";\"\nNumber of variables must be at least \" ++ show numVars"
                                                else if not (all (\d -> length d == 4) varValDefs) then
                                                    do
                                                        showMessageBox "Variable values must be defined in following format: \"start, end, step, value\""
                                                else
                                                    do
                                                        let
                                                            minSegments = minimum (List.map (length . dataSet) dataParams)
                                                        if minSegments /= maximum (List.map (length . dataSet) dataParams) then
                                                            do
                                                                showMessageBox "Not all datasets have the same number of segmentations, using the minimum"
                                                        else return ()
                                                        let
                                                            mapOp i dp = unboxSubData $ subData $ dataSet dp !! i
                                                            segments = List.map (\i -> List.map (mapOp i) dataParams) [0 .. minSegments - 1]
                                                        results <- calcConcurrently (\dataSets puFunc -> S.getValues dataSets varValDefs (mkStdGen 1) puFunc (taskInitializer tEnv) (taskFinalizer tEnv) statistic) (progressUpdateFunc tEnv) (taskInitializer tEnv) (taskFinalizer tEnv) segments
                                                        modifyMVar_ stateRef $ \state ->
                                                            return $ addSegmentedData (List.map SD1 results) name (Just (currentGraphTab, selectedGraph)) state

                                    if not (S.isValid statistic) then
                                        do
                                            showMessageBox "Statistic validation failed"
{-                                        else if length dataParams /= length dataFuncs then
                                            do
                                                showMessageBox "Statistic must be applied to exactly \" ++ (show (length dataFuncs)) ++ \" data set(s)"
                                                -}
                                        else if any (\(dp, df) -> not df && isAnalytic dp) (zip dataParams (S.dataFuncs statistic)) then
                                            do
                                                showMessageBox "Data set type(s) expected by statistic do not match with selected one(s)"
                                        else runTask stateRef "Calculate statistic" $ applyStatistic >> return ()


                            addPageToNotebook (page, Just name, statisticTextBuffer)
                            modifyIORef pagesRef (\pages -> pages ++ [(page, name, statisticTextBuffer)])
                            --return (page, name, statisticEntry)
        ------------------------------------------------------------------------

    createPage Nothing
    mapM_ (createPage) $ (List.map (Just) (init (statisticParams (params state))))
    --modifyIORef pagesRef (\_ -> pages)

    addWidgetToBox Nothing notebook contentBox

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
            state <- readMVar stateRef
            pages <- readIORef pagesRef
            modifyMVar_ stateRef $ \state ->
                foldM (\state (name, statisticTextBuffer) ->
                    do
                        startIter <- Gtk.textBufferGetStartIter statisticTextBuffer
                        endIter <- Gtk.textBufferGetEndIter statisticTextBuffer
                        text <- Gtk.textBufferGetText statisticTextBuffer startIter endIter False
                        return state {params = updateStatistic ((getStatisticByName name (params state)) {statisticDefinition = T.unpack text}) (params state)}
                    ) state $ (Prelude.map (\(_, name, entry) -> (name, entry)) pages)
            Gtk.windowDestroy dialog

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowSetDefaultSize dialog 800 600
    Gtk.windowPresent dialog