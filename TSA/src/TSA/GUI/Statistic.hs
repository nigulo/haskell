
module TSA.GUI.Statistic (statisticDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget
import TSA.GUI.Data
import TSA.Data
import Utils.Concurrent
import Utils.List
import Utils.Str
import GUI.Plot
import GUI.Widget

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
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    notebook <- notebookNew

    pagesRef <- newIORef []

    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

        ------------------------------------------------------------------------    
        deleteStatistic page = 
            do
                Just pageIndex <- notebookPageNum notebook page
                pages <- readIORef pagesRef
                let dataPage@(page, name, _) = pages !! pageIndex
                notebookRemovePage notebook pageIndex
                modifyIORef pagesRef (\pages -> 
                    deleteBy (\(_, name1, _) (_, name2, _) -> name1 == name2) dataPage pages)
                modifyStateParams stateRef $ \params -> removeStatisticByName name params

        ------------------------------------------------------------------------    
        addPageToNotebook (page, maybeName, _) =
            do
                pageIndex <- notebookGetNPages notebook
                label <-
                    case maybeName of
                        Just name -> labelWithButton (Just name) stockDelete (deleteStatistic page)
                        otherwise -> labelWithImage (Just "Add statistic") (Just stockAdd)
                notebookInsertPageMenu notebook page label label (max (pageIndex - 1) 0)
                widgetShowAll label
                widgetShowAll notebook
    
        ------------------------------------------------------------------------    
        createPage :: Maybe StatisticParams -> IO () --(VBox, String, Entry)
        createPage pageDef = 
            do
    
                page <- vBoxNew False 0
                
                case pageDef of
                    Nothing ->
                        do 
                            let
                                statParams = last $ statisticParams (params state)
                                commonParams = statisticCommonParams statParams
                            nameEntry <- entryNew
                            nameEntry `entrySetText` (commonName commonParams)
                            addWidgetToVBox (Just "Name: ") nameEntry PackNatural page
                            
                            statisticTextBuffer <- textBufferNew Nothing
                            textBufferSetText statisticTextBuffer (statisticDefinition statParams)
                            statisticTextView <- textViewNewWithBuffer statisticTextBuffer
                            font <- fontDescriptionNew
                            fontDescriptionSetFamily font "Courier New"
                            widgetModifyFont statisticTextView (Just font)
                            textViewSetAcceptsTab statisticTextView False
                            addWidgetToVBox (Just "Definition:") statisticTextView PackNatural page
                            
                            hBox <- hBoxNew False 0
                            addButton <- buttonNewWithLabel "Add"
                            boxPackEnd hBox addButton PackNatural 2
                            on addButton buttonActivated $
                                do
                                    state <- readMVar stateRef
                                    name <- entryGetString nameEntry
                                    startIter <- textBufferGetStartIter statisticTextBuffer
                                    endIter <- textBufferGetEndIter statisticTextBuffer
                                    text <- textBufferGetText statisticTextBuffer startIter endIter False
                                    let
                                        statParams = last $ statisticParams (params state)
                                        commonParams = statisticCommonParams statParams
                                        newCommonParams = updateCommonParams name commonParams
                                        newStatParams = statParams {statisticCommonParams = newCommonParams}
                                    modifyStateParams stateRef $ \params -> updateStatistic newStatParams params
                                    modifyStateParams stateRef $ \params -> addStatistic name text params
                                    state <- readMVar stateRef
                                
                                    entrySetText nameEntry (getNameWithNo newCommonParams)
                                    createPage $ Just (getStatisticByName name (params state))
                                    pageIndex <- notebookGetNPages notebook
                                    notebookSetCurrentPage notebook (pageIndex - 2)
                                    
                            addWidgetToVBox Nothing hBox PackNatural page
                            addPageToNotebook (page, Nothing, statisticTextBuffer)
                            
                    Just statParams ->
                        do
                            let
                                name = (commonName . statisticCommonParams) statParams
                            statisticTextBuffer <- textBufferNew Nothing
                            textBufferSetText statisticTextBuffer (statisticDefinition statParams)
                            statisticTextView <- textViewNewWithBuffer statisticTextBuffer
                            font <- fontDescriptionNew
                            fontDescriptionSetFamily font "Courier New"
                            widgetModifyFont statisticTextView (Just font)
                            textViewSetAcceptsTab statisticTextView False

                            hAdjustment <- adjustmentNew
                                0 {- the initial value -}
                                0 {- the minimum value -}
                                10000 {- the maximum value -}
                                1 {- the step increment -}
                                10 {- the page increment -}
                                100 {- the page size -}

                            vAdjustment <- adjustmentNew
                                0 {- the initial value -}
                                0 {- the minimum value -}
                                100000 {- the maximum value -}
                                1 {- the step increment -}
                                10 {- the page increment -}
                                10 {- the page size -}

                            scrollwdTextView <- scrolledWindowNew (Just hAdjustment) (Just vAdjustment)

                            scrolledWindowAddWithViewport scrollwdTextView statisticTextView
                            addWidgetToVBox (Just "Definition:") scrollwdTextView PackNatural page
                            
                            
                            hBox <- hBoxNew False 0
                            saveButton <- buttonNewWithLabel "Save changes"
                            boxPackEnd hBox saveButton PackNatural 2
                                    
                            addWidgetToVBox Nothing hBox PackNatural page
                
                            dataChooser <- dataSetChooserNew (\_ -> True) state
                            addWidgetToVBox Nothing (dataSetChooserToWidget dataChooser) PackGrow page
            
                            nameEntry <- entryNew 
                            nameEntry `entrySetText` name
                            addWidgetToVBox (Just "Name:") nameEntry PackNatural page
            
                            varValDefsEntry <- entryNew
                            varValDefsEntry `entrySetText` (statisticVarValsDef statParams)
                            addWidgetToVBox (Just "Variable values: ") varValDefsEntry PackNatural page
                        
                            on saveButton buttonActivated $
                                do
                                    startIter <- textBufferGetStartIter statisticTextBuffer
                                    endIter <- textBufferGetEndIter statisticTextBuffer
                                    statisticStr <- textBufferGetText statisticTextBuffer startIter endIter False
                                    varValDefsStr <- entryGetString varValDefsEntry
                                    modifyStateParams stateRef $ \params -> updateStatistic (
                                        statParams {statisticDefinition = statisticStr, 
                                        statisticVarValsDef = varValDefsStr
                                    }) params
                        
                            hBox <- hBoxNew False 0
                            applyButton <- buttonNewWithLabel "Apply to data"
                            boxPackEnd hBox applyButton PackNatural 2
                            on applyButton buttonActivated $
                                do
                                    startIter <- textBufferGetStartIter statisticTextBuffer
                                    endIter <- textBufferGetEndIter statisticTextBuffer
                                    text <- textBufferGetText statisticTextBuffer startIter endIter False
                                    dataParams <- dataSetChooserGetChoice dataChooser
                                    let
                                        statistic = S.statistic text
                                        dataFuncs = S.dataFuncs statistic
                                        applyStatistic =
                                            do
                                                startIter <- textBufferGetStartIter statisticTextBuffer
                                                endIter <- textBufferGetEndIter statisticTextBuffer
                                                text <- textBufferGetText statisticTextBuffer startIter endIter False
                                                name <- entryGetString nameEntry
                                                varValDefsStr <- entryGetString varValDefsEntry
                                                (currentGraphTab, _) <- getCurrentGraphTab state
                                                
                                                let
                                                    puFunc = progressUpdate stateRef
                                                    graphTabParms = (graphTabs state) !! currentGraphTab
                                                    selectedGraph = graphTabSelection graphTabParms
                                                    statistic = S.statistic text
                                                    numVars = length (S.varNames statistic) 

                                                    varValDefs = List.map (\str -> List.map (\s -> F.function (trim s)) (splitBy ',' str)) $ 
                                                        List.map trim $ splitBy ';' $
                                                        replaceAll "left" ("" ++ show (plotAreaLeft ga)) $
                                                        replaceAll "right" ("" ++ show (plotAreaRight ga)) $
                                                        replaceAll "bottom" ("" ++ show (plotAreaBottom ga)) $
                                                        replaceAll "top" ("" ++ show (plotAreaTop ga)) varValDefsStr
                                                    
                                                if numVars > length varValDefs then
                                                    do
                                                        messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ "Variable value definitions must be separated by \";\"\nNumber of variables must be at least " ++ show numVars
                                                        widgetShowAll messageDialog
                                                        dialogRun messageDialog 
                                                        widgetDestroy messageDialog
                                                else if not (all (\d -> length d == 4) varValDefs) then
                                                    do
                                                        messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ "Variable values must be defined in following format: \"start, end, step, value\""
                                                        widgetShowAll messageDialog
                                                        dialogRun messageDialog 
                                                        widgetDestroy messageDialog
                                                else
                                                    do
                                                        let
                                                            minSegments = minimum (List.map (length . dataSet) dataParams)
                                                        if minSegments /= maximum (List.map (length . dataSet) dataParams) then
                                                            do
                                                                messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageInfo ButtonsOk $ "Not all datasets have the same number of segmentations, using the minimum"
                                                                widgetShowAll messageDialog
                                                                dialogRun messageDialog 
                                                                widgetDestroy messageDialog
                                                        else return ()
                                                        let
                                                            mapOp i dp =
                                                                case subData $ dataSet dp !! i of
                                                                    Left d -> Left d
                                                                    Right (Left s) -> Right (analyticDataWrapper s)
                                                                    Right (Right f) -> Right (analyticDataWrapper f)
                                                            segments = List.map (\i -> List.map (mapOp i) dataParams) [0 .. minSegments - 1]
                                                        results <- calcConcurrently (\dataSets puFunc -> S.getValues dataSets varValDefs (mkStdGen 1) puFunc statistic) puFunc segments
                                                        modifyMVar_ stateRef $ \state -> 
                                                            return $ addSegmentedData (List.map Left results) name (Just (currentGraphTab, selectedGraph)) state
                                    
                                    if not (S.isValid statistic) then
                                        do
                                            messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ "Statistic validation failed"
                                            widgetShowAll messageDialog
                                            dialogRun messageDialog 
                                            widgetDestroy messageDialog
{-                                        else if length dataParams /= length dataFuncs then
                                            do
                                                messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ "Statistic must be applied to exactly " ++ (show (length dataFuncs)) ++ " data set(s)"
                                                widgetShowAll messageDialog
                                                dialogRun messageDialog 
                                                widgetDestroy messageDialog
                                                -}
                                        else if any (\(dp, df) -> not df && isAnalytic dp) (zip dataParams (S.dataFuncs statistic)) then
                                            do
                                                messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ "Data set type(s) expected by statistic do not match with selected one(s)"
                                                widgetShowAll messageDialog
                                                dialogRun messageDialog 
                                                widgetDestroy messageDialog
                                        else forkIO applyStatistic >> return ()
    

                            addWidgetToVBox Nothing hBox PackNatural page
                            addPageToNotebook (page, Just name, statisticTextBuffer)
                            modifyIORef pagesRef (\pages -> pages ++ [(page, name, statisticTextBuffer)]) 
                            --return (page, name, statisticEntry)
        ------------------------------------------------------------------------    
    
    createPage Nothing
    mapM_ (createPage) $ (List.map (Just) (init (statisticParams (params state))))
    --modifyIORef pagesRef (\_ -> pages) 
    
    addWidgetToVBox Nothing notebook PackGrow vBox

    let
        onClose response =
            do
                if response == ResponseOk
                    then 
                        do
                            state <- readMVar stateRef
                            pages <- readIORef pagesRef
                            modifyMVar_ stateRef $ \state -> 
                                foldM (\state (name, statisticTextBuffer) -> 
                                    do
                                        startIter <- textBufferGetStartIter statisticTextBuffer
                                        endIter <- textBufferGetEndIter statisticTextBuffer
                                        text <- textBufferGetText statisticTextBuffer startIter endIter False
                                        return state {params = updateStatistic ((getStatisticByName name (params state)) {statisticDefinition = text}) (params state)}
                                    ) state $ (Prelude.map (\(_, name, entry) -> (name, entry)) pages)
                    else
                        return ()
                
        

    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Ok" ResponseOk

    widgetShowAll dialog
    
    response <- dialogRun dialog 

    onClose response

    widgetDestroy dialog

