module TSA.GUI.Log (showLog, appendLog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

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
        otherwise -> do
            win <- windowNew
            icon <- pixbufNewFromFile "tsa.bmp"
            win `set` [windowTitle := "Log", windowIcon := Just icon]

            vBox <- vBoxNew False 2
            textBuffer <- textBufferNew Nothing
            textBufferSetText textBuffer (TSA.GUI.State.log state)
            textView <- textViewNewWithBuffer textBuffer
            font <- fontDescriptionNew
            fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
            widgetModifyFont textView (Just font)
            --textViewSetAcceptsTab textView False
            textViewSetEditable textView False 
            
            scrolledWindow <- scrolledWindowNew Nothing Nothing
            containerAdd scrolledWindow textView
            
            boxPackStart vBox scrolledWindow PackGrow 2
            
            hBox <- hBoxNew False 2
            clearButton <- buttonNewWithLabel "Clear"
            on clearButton buttonReleaseEvent $ liftIO (clearLog stateRef >> return True)
            boxPackEnd hBox clearButton PackNatural 2
            boxPackEnd vBox hBox PackNatural 2
            
            containerAdd win vBox
            
            win `on` objectDestroy $
                modifyMVar_ stateRef $ \state -> return $ state {
                    guiParams = Just (fromJust (guiParams state)) {
                        guiLog = Nothing
                        }
                    }
            
            widgetShowAll win
            windowResize win 640 480

            textMark <- textMarkNew Nothing True 
            textIter <- textBufferGetEndIter textBuffer
            textBufferAddMark textBuffer textMark textIter
            textViewScrollToMark textView textMark 0 Nothing 
            
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
    updateLog stateRef

clearLog :: StateRef -> IO ()
clearLog stateRef = do
    modifyMVar_ stateRef $ \state -> return $ state {
        TSA.GUI.State.log = ""
        }
    updateLog stateRef

updateLog :: StateRef -> IO ()
updateLog stateRef = postGUIAsync $ do
    state <- readMVar stateRef
    case guiLog (fromJust (guiParams state)) of
        Just textView -> do
            textBuffer <- textViewGetBuffer textView
            textBufferSetText textBuffer (TSA.GUI.State.log state)
            textMark <- textMarkNew Nothing True 
            textIter <- textBufferGetEndIter textBuffer
            textBufferAddMark textBuffer textMark textIter
            textViewScrollToMark textView textMark 0 Nothing 
        otherwise -> return ()
