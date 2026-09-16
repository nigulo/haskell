{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Preferences (preferencesDialog) where


import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)

import Utils.Misc

import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random

preferencesDialog :: StateRef -> IO ()
preferencesDialog stateRef = do
    state <- readMVar stateRef
    let
        params = settingsParams state

    dialog <- dialogWithTitle state "Preferences"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    saveChangesOnExitCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive saveChangesOnExitCheck (settingsSaveChangesOnExit params)
    addWidgetToBox (Just "Save changes on exit: ") saveChangesOnExitCheck contentBox

    saveZippedCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive saveZippedCheck (settingsSaveInZippedFormat params)
    addWidgetToBox (Just "Save in zipped format: ") saveZippedCheck contentBox

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
        saveChanges <- Gtk.checkButtonGetActive saveChangesOnExitCheck
        saveZipped <- Gtk.checkButtonGetActive saveZippedCheck
        Gtk.windowDestroy dialog
        modifyMVar_ stateRef $ \state -> return $ state {settingsParams = params {settingsSaveChangesOnExit = saveChanges, settingsSaveInZippedFormat = saveZipped}}
        return ()

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowSetDefaultSize dialog 400 200
    Gtk.windowPresent dialog