
module TSA.GUI.Function (functionDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Data

import GUI.Plot
import GUI.Widget

import Math.Function as F
import Math.Expression
import Regression.AnalyticData
import Control.Concurrent.MVar
import Prelude hiding (catch)
import Control.Exception
import Control.Applicative

functionDialog :: StateRef -> IO ()
functionDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        parms = functionParams (params state)
        commonParams = functionCommonParams parms
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

    dialog <- dialogWithTitle state "Add function"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseNone

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry PackNatural vBox

    let
        xLeft = case functionLeft parms of
            Nothing -> plotAreaLeft ga
            Just left -> left
        xRight = case functionRight parms of
            Nothing -> plotAreaRight ga
            Just right -> right
        yBottom = case functionBottom parms of
            Nothing -> plotAreaBottom ga
            Just bottom -> bottom
        yTop = case functionTop parms of
            Nothing -> plotAreaTop ga
            Just top -> top
            
    leftAdjustment <- adjustmentNew xLeft (-2**52) (2**52) 1 1 10
    leftSpin <- spinButtonNew leftAdjustment 1 10
    addWidgetToBox (Just "Left: ") leftSpin PackNatural vBox

    rightAdjustment <- adjustmentNew xRight (-2**52) (2**52) 1 1 10
    rightSpin <- spinButtonNew rightAdjustment 1 10
    addWidgetToBox (Just "Right: ") rightSpin PackNatural vBox

    bottomAdjustment <- adjustmentNew yBottom (-2**52) (2**52) 1 1 10
    bottomSpin <- spinButtonNew bottomAdjustment 1 10
    addWidgetToBox (Just "Bottom: ") bottomSpin PackNatural vBox

    topAdjustment <- adjustmentNew yTop (-2**52) (2**52) 1 1 10
    topSpin <- spinButtonNew topAdjustment 1 10
    addWidgetToBox (Just "Top: ") topSpin PackNatural vBox
    
    functionTextBuffer <- textBufferNew Nothing
    textBufferSetText functionTextBuffer (functionDefinition parms)
    functionTextView <- textViewNewWithBuffer functionTextBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
    widgetModifyFont functionTextView (Just font)
    textViewSetAcceptsTab functionTextView False

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledWindow functionTextView

    functionFrame <- frameNew
    frameSetLabel functionFrame (stringToGlib "Function" )
    containerAdd functionFrame scrolledWindow
    addWidgetToBox Nothing functionFrame PackGrow vBox

    on okButton buttonActivated $
        do
            name <- entryGetString nameEntry
            startIter <- textBufferGetStartIter functionTextBuffer
            endIter <- textBufferGetEndIter functionTextBuffer
            f <- textBufferGetText functionTextBuffer startIter endIter False
            left <- spinButtonGetValue leftSpin
            right <- spinButtonGetValue rightSpin
            bottom <- spinButtonGetValue bottomSpin
            top <- spinButtonGetValue topSpin
            let
                func = F.function f
                varNames = F.varNames func 
                opNames = F.funcNames func 
            if F.isValid func
                then
                    if opNames /= []
                        then
                            do                    
                                messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ ("Missing definitions for " ++ (show opNames))
                                widgetShowAll messageDialog
                                dialogRun messageDialog 
                                widgetDestroy messageDialog
                        else
                            if length varNames > 2
                                then
                                    do                    
                                        messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ ("Too many function arguments " ++ (show varNames))
                                        widgetShowAll messageDialog
                                        dialogRun messageDialog 
                                        widgetDestroy messageDialog
                                else
                                    do
                                        if length varNames == 1 
                                            then
                                                modifyState stateRef $ addFunction (AnalyticData [([left], [right], func)]) name (Just (currentGraphTab, selectedGraph))
                                            else 
                                                modifyState stateRef $ addFunction (AnalyticData [([left, bottom], [right, top], func)]) name (Just (currentGraphTab, selectedGraph))
            
                                        modifyStateParams stateRef $ \params -> params {functionParams = FunctionParams {
                                                functionCommonParams = updateCommonParams name commonParams,
                                                functionDefinition = f,
                                                functionLeft = Just left,
                                                functionRight = Just right,
                                                functionBottom = Just bottom,
                                                functionTop = Just top
                                            }}
                    
                                        dialogResponse dialog ResponseOk
                else
                    do                    
                        messageDialog <- messageDialogNew (Just (toWindow dialog)) [DialogModal] MessageWarning ButtonsOk $ ("Error while parsing function")
                        widgetShowAll messageDialog
                        dialogRun messageDialog 
                        widgetDestroy messageDialog
                        
    widgetShowAll dialog
    response <- dialogRun dialog
    widgetDestroy dialog

