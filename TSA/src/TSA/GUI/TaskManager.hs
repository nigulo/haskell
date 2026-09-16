{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.TaskManager (taskManagerDialog) where

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import Data.GI.Base
import qualified Data.Text as T

import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad (forM_)

import System.Random

taskManagerDialog :: StateRef -> IO ()
taskManagerDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state

    win <- dialogWithTitle state "Task manager"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    destroyedRef <- newMVar False

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox okButton

    Gtk.boxAppend contentBox buttonBox
    Gtk.windowSetChild win (Just contentBox)

    refresh stateRef contentBox destroyedRef

    handlerId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 5000 $ do
        refresh stateRef contentBox destroyedRef
        return True

    _ <- Gtk.onButtonClicked okButton $ do
        GLib.sourceRemove handlerId
        Gtk.windowDestroy win

    _ <- Gtk.onWindowCloseRequest win $ do
        GLib.sourceRemove handlerId
        modifyMVar_ destroyedRef $ \_ -> return True
        return False

    Gtk.windowPresent win

refresh :: StateRef -> Gtk.Box -> MVar Bool -> IO ()
refresh stateRef contentBox destroyedRef =
    modifyMVar_ destroyedRef $ \destroyed -> do
        if destroyed
            then
                return ()
            else do
                state <- readMVar stateRef
                -- Remove old task widgets (all children except the last button box)
                removeTaskWidgets contentBox
                case tasks state of
                    [] -> do
                        label <- Gtk.labelNew (Just "No tasks")
                        Gtk.widgetSetName label "TaskWidget"
                        Gtk.boxPrepend contentBox label
                    _ ->
                        forM_ (filter (\(Task _ taskName _ _) -> taskName /= "") $ tasks state) $ \(Task threadId taskName percent _) -> do
                            progressBar <- Gtk.progressBarNew
                            Gtk.progressBarSetFraction progressBar percent
                            stopButton <- Gtk.buttonNewWithLabel "Stop"
                            hBox <- Gtk.boxNew Gtk.OrientationHorizontal 2
                            Gtk.widgetSetHexpand progressBar True
                            Gtk.boxAppend hBox progressBar
                            Gtk.boxAppend hBox stopButton
                            taskBox <- addWidget (Just taskName) hBox contentBox
                            Gtk.widgetSetName taskBox "TaskWidget"
                            _ <- Gtk.onButtonClicked stopButton $ do
                                killThread threadId
                                refresh stateRef contentBox destroyedRef
                            return ()
        return destroyed

removeTaskWidgets :: Gtk.Box -> IO ()
removeTaskWidgets box = do
    let
        go = do
            maybeChild <- Gtk.widgetGetFirstChild box
            case maybeChild of
                Nothing -> return ()
                Just child -> do
                    name <- Gtk.widgetGetName child
                    if name == "TaskWidget"
                        then do
                            Gtk.boxRemove box child
                            go
                        else do
                            -- Check next sibling
                            goNext child
        goNext widget = do
            maybeSibling <- Gtk.widgetGetNextSibling widget
            case maybeSibling of
                Nothing -> return ()
                Just sibling -> do
                    name <- Gtk.widgetGetName sibling
                    if name == "TaskWidget"
                        then do
                            Gtk.boxRemove box sibling
                            goNext widget
                        else goNext sibling
    go
