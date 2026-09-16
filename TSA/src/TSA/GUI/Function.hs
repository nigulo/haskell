{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Function (functionDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Data

import GUI.Plot
import GUI.Widget hiding (entryGetString)

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

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

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

    leftAdjustment <- Gtk.adjustmentNew xLeft (-2**52) (2**52) 1 1 10
    leftSpin <- Gtk.spinButtonNew (Just leftAdjustment) 1 10
    addWidgetToBox (Just "Left: ") leftSpin contentBox

    rightAdjustment <- Gtk.adjustmentNew xRight (-2**52) (2**52) 1 1 10
    rightSpin <- Gtk.spinButtonNew (Just rightAdjustment) 1 10
    addWidgetToBox (Just "Right: ") rightSpin contentBox

    bottomAdjustment <- Gtk.adjustmentNew yBottom (-2**52) (2**52) 1 1 10
    bottomSpin <- Gtk.spinButtonNew (Just bottomAdjustment) 1 10
    addWidgetToBox (Just "Bottom: ") bottomSpin contentBox

    topAdjustment <- Gtk.adjustmentNew yTop (-2**52) (2**52) 1 1 10
    topSpin <- Gtk.spinButtonNew (Just topAdjustment) 1 10
    addWidgetToBox (Just "Top: ") topSpin contentBox

    functionTextBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
    Gtk.textBufferSetText functionTextBuffer (T.pack (functionDefinition parms)) (-1)
    functionTextView <- Gtk.textViewNewWithBuffer functionTextBuffer
    Gtk.textViewSetMonospace functionTextView True
    Gtk.textViewSetAcceptsTab functionTextView False

    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just functionTextView)
    Gtk.widgetSetVexpand scrolledWindow True

    functionFrame <- Gtk.frameNew (Just "Function")
    Gtk.frameSetChild functionFrame (Just scrolledWindow)
    addWidgetToBox Nothing functionFrame contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $ do
        name <- entryGetString nameEntry
        startIter <- Gtk.textBufferGetStartIter functionTextBuffer
        endIter <- Gtk.textBufferGetEndIter functionTextBuffer
        f <- Gtk.textBufferGetText functionTextBuffer startIter endIter False
        left <- spinButtonGetValue leftSpin
        right <- spinButtonGetValue rightSpin
        bottom <- spinButtonGetValue bottomSpin
        top <- spinButtonGetValue topSpin
        let
            func = F.function (T.unpack f)
            varNames = F.varNames func
            opNames = F.funcNames func
        if F.isValid func
            then
                if opNames /= []
                    then
                        showWarning dialog ("Missing definitions for " ++ (show opNames))
                    else
                        if length varNames > 2
                            then
                                showWarning dialog ("Too many function arguments " ++ (show varNames))
                            else
                                do
                                    if length varNames <= 1
                                        then
                                            modifyState stateRef $ addFunction (AnalyticData [([left], [right], func)]) name (Just (currentGraphTab, selectedGraph))
                                        else
                                            modifyState stateRef $ addFunction (AnalyticData [([left, bottom], [right, top], func)]) name (Just (currentGraphTab, selectedGraph))

                                    modifyStateParams stateRef $ \params -> params {functionParams = FunctionParams {
                                            functionCommonParams = updateCommonParams name commonParams,
                                            functionDefinition = T.unpack f,
                                            functionLeft = Just left,
                                            functionRight = Just right,
                                            functionBottom = Just bottom,
                                            functionTop = Just top
                                        }}

                                    Gtk.windowDestroy dialog
            else
                showWarning dialog "Error while parsing function"

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowPresent dialog

showWarning :: Gtk.Window -> String -> IO ()
showWarning parent msg = do
    win <- Gtk.windowNew
    Gtk.windowSetTitle win (Just "Warning")
    Gtk.windowSetModal win True
    Gtk.windowSetTransientFor win (Just parent)

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

    _ <- Gtk.onButtonClicked okButton $ Gtk.windowDestroy win

    Gtk.windowSetChild win (Just vBox)
    Gtk.windowSetDefaultSize win 400 100
    Gtk.windowPresent win