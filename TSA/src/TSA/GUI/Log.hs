module TSA.GUI.Log (showLog, appendLog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import TSA.GUI.State
import TSA.GUI.Common

import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import Data.Maybe

showLog :: StateRef -> IO ()
showLog stateRef = do
    state <- readMVar stateRef
    case guiLog (fromJust (guiParams state)) of
        Just _ -> return ()
        otherwise -> do
            textBuffer <- textBufferNew Nothing
            textBufferSetText textBuffer (TSA.GUI.State.log state)
            textView <- textViewNewWithBuffer textBuffer
            font <- fontDescriptionNew
            fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
            widgetModifyFont textView (Just font)
            --textViewSetAcceptsTab textView False
            textViewSetEditable textView False 
            
            win <- windowNew
            icon <- pixbufNewFromFile "tsa.bmp"
            win `set` [windowTitle := "Log", windowIcon := Just icon]
            
            vAdjustment <- adjustmentNew 0 0 100 1 10 10
            scrolledWindow <- scrolledWindowNew Nothing (Just vAdjustment)

            --vBox <- vBoxNew False 0
            --set vBox [boxHomogeneous := False]
            --boxPackStart vBox scrolledWindow PackGrow 0
            
            containerAdd win scrolledWindow
            
            --vBox <- vBoxNew False 0
            --set vBox [boxHomogeneous := False]
            --boxPackStart vBox textView PackGrow 0
            
            containerAdd scrolledWindow textView
            
            win `on` objectDestroy $
                modifyMVar_ stateRef $ \state -> return $ state {
                    guiParams = Just (fromJust (guiParams state)) {
                        guiLog = Nothing
                        }
                    }
            
            windowResize win 640 480
            widgetShowAll win
            
            modifyMVar_ stateRef $ \state -> return $ state {
                guiParams = Just (fromJust (guiParams state)) {
                    guiLog = Just (textView)
                    }
                }

appendLog :: StateRef -> String -> IO ()
appendLog stateRef text = do
    state <- readMVar stateRef
    let 
        newText = TSA.GUI.State.log state ++ text ++ "\n"
    modifyMVar_ stateRef $ \state -> return $ state {
        TSA.GUI.State.log = newText
        }
    case guiLog (fromJust (guiParams state)) of
        Just textView -> do
            textBuffer <- textViewGetBuffer textView
            textBufferSetText textBuffer newText
        otherwise -> return ()

