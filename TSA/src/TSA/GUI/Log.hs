{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Log (showLog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import TSA.GUI.State
import TSA.GUI.Common

import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe

showLog :: StateRef -> IO ()
showLog stateRef = do
    state <- readMVar stateRef
    case guiLog (fromJust (guiParams state)) of
        Just _ -> return ()
        Nothing -> do
            win <- Gtk.windowNew
            Gtk.windowSetTitle win (Just "Log")

            vBox <- Gtk.boxNew Gtk.OrientationVertical 2
            textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
            Gtk.textBufferSetText textBuffer (T.pack (TSA.GUI.State.log state)) (-1)
            textView <- Gtk.textViewNewWithBuffer textBuffer
            Gtk.textViewSetEditable textView False

            scrolledWindow <- Gtk.scrolledWindowNew
            Gtk.scrolledWindowSetChild scrolledWindow (Just textView)
            Gtk.widgetSetVexpand scrolledWindow True

            Gtk.boxAppend vBox scrolledWindow

            hBox <- Gtk.boxNew Gtk.OrientationHorizontal 2
            clearButton <- Gtk.buttonNewWithLabel "Clear"
            Gtk.widgetSetHalign hBox Gtk.AlignEnd
            _ <- Gtk.onButtonClicked clearButton $ clearLog stateRef
            Gtk.boxAppend hBox clearButton
            Gtk.boxAppend vBox hBox

            Gtk.windowSetChild win (Just vBox)

            _ <- Gtk.onWindowCloseRequest win $ do
                modifyMVar_ stateRef $ \st -> return $ st {
                    guiParams = Just (fromJust (guiParams st)) {
                        guiLog = Nothing
                        }
                    }
                return False

            Gtk.windowSetDefaultSize win 640 480
            Gtk.windowPresent win

            textMark <- Gtk.textMarkNew Nothing True
            textIter <- Gtk.textBufferGetEndIter textBuffer
            Gtk.textBufferAddMark textBuffer textMark textIter
            Gtk.textViewScrollToMark textView textMark 0 False 0 0

            modifyMVar_ stateRef $ \st -> return $ st {
                guiParams = Just (fromJust (guiParams st)) {
                    guiLog = Just (textView)
                    }
                }

clearLog :: StateRef -> IO ()
clearLog stateRef = do
    modifyMVar_ stateRef $ \state -> return $ state {
        TSA.GUI.State.log = ""
        }
    refreshLog stateRef
