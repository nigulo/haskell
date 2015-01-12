module TSA.GUI.Markers (showMarkers) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Data
import Utils.List
import GUI.Plot

import Math.Function as F
import Math.Expression
import Regression.AnalyticData
import Control.Concurrent.MVar
import Prelude hiding (catch)
import Control.Exception
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Data.List

{-
markersDialog :: StateRef -> IO ()
markersDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

    dialog <- dialogWithTitle state "Markers"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
        
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2

    let
        updateSegments markerSpins =
            modifyMVar_ stateRef $ \state -> do
                newSegments <- mapM (\markerSpin -> spinButtonGetValue markerSpin) markerSpins
                let
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms
                    graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
                return $ state {
                    graphTabs = updateAt currentGraphTab (graphTabParms {
                        graphTabGraphs = updateAt selectedGraph (graphParms {
                            graphSegments = newSegments
                        }) (graphTabGraphs graphTabParms)
                    }) (graphTabs state)
                }
        segments = graphSegments ((graphTabGraphs graphTabParms) !! selectedGraph)
        
        
    markerSpinsRemoveButtons <- mapM (\(no, marker) -> do
            markerAdjustment <- adjustmentNew marker (-2**52) (2**52) 1 1 10
            markerSpin <- spinButtonNew markerAdjustment 1 10
            removeButton <- buttonNewFromStock stockRemove
            hBox <- hBoxNew False 0
            boxPackStart hBox markerSpin PackGrow 2
            boxPackEnd hBox removeButton PackNatural 2
            addWidgetToVBox (Just ("Marker " ++ show no ++ ":")) hBox PackNatural vBox
            return (markerSpin, removeButton)
        ) (zip [1, 2 ..] segments)

    let
        (markerSpins, removeButtons) = unzip markerSpinsRemoveButtons
    markerSpinsRef <- newMVar markerSpins

    let
        removeMarker no = do
            modifyMVar_ markerSpinsRef $ \markerSpins-> do
                return $ (take no markerSpins) ++ (drop (no + 1) markerSpins)
            markerSpins <- readMVar markerSpinsRef
            updateSegments markerSpins

    mapM_ (\no -> do
            on (removeButtons !! no) buttonReleaseEvent $ liftIO (removeMarker no >> return True)
        ) [0 .. length markerSpins - 1]
    widgetShowAll dialog
    response <- dialogRun dialog

    if response == ResponseOk 
        then do
            markerSpins <- readMVar markerSpinsRef
            updateSegments markerSpins
            widgetDestroy dialog
        else
            widgetDestroy dialog
-}                  
                        
showMarkers :: StateRef -> IO ()
showMarkers stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

    textBuffer <- textBufferNew Nothing
    
    let
        segments = graphSegments ((graphTabGraphs graphTabParms) !! selectedGraph)
        
    textBufferSetText textBuffer (concatMap (\segment -> show segment ++ "\n") segments)
    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font "Arial"
    widgetModifyFont textView (Just font)
    
    win <- windowNew
    icon <- pixbufNewFromFile "tsa.bmp"
    win `set` [windowTitle := "Markers", windowIcon := Just icon]
    
    vAdjustment <- adjustmentNew 0 0 100 1 10 10
    scrolledWindow <- scrolledWindowNew Nothing (Just vAdjustment)

    containerAdd win scrolledWindow
    containerAdd scrolledWindow textView
    
    let
        updateSegments =
            modifyMVar_ stateRef $ \state -> do
                startIter <- textBufferGetStartIter textBuffer
                endIter <- textBufferGetEndIter textBuffer
                text <- textBufferGetText textBuffer startIter endIter True
                let
                    newSegments = sort $ nub $ map (\segment -> read segment) $ concatMap (\line -> words line) (lines text)
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms
                    graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
                return $ state {
                    graphTabs = updateAt currentGraphTab (graphTabParms {
                        graphTabGraphs = updateAt selectedGraph (graphParms {
                            graphSegments = newSegments
                        }) (graphTabGraphs graphTabParms)
                    }) (graphTabs state)
                }
    
    win `on` objectDestroy $ updateSegments
    
    windowResize win 640 480
    widgetShowAll win
