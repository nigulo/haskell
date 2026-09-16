{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Markers (showMarkers) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Data
import Utils.List
import GUI.Plot
import GUI.Widget

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

showMarkers :: StateRef -> IO ()
showMarkers stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

    textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)

    let
        segments = graphSegments ((graphTabGraphs graphTabParms) !! selectedGraph)

    Gtk.textBufferSetText textBuffer (T.pack (concatMap (\segment -> show segment ++ "\n") segments)) (-1)
    textView <- Gtk.textViewNewWithBuffer textBuffer

    win <- Gtk.windowNew
    Gtk.windowSetTitle win (Just "Markers")

    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just textView)
    Gtk.widgetSetVexpand scrolledWindow True

    Gtk.windowSetChild win (Just scrolledWindow)

    let
        updateSegments =
            modifyMVar_ stateRef $ \state -> do
                startIter <- Gtk.textBufferGetStartIter textBuffer
                endIter <- Gtk.textBufferGetEndIter textBuffer
                text <- Gtk.textBufferGetText textBuffer startIter endIter True
                let
                    newSegments = sort $ nub $ map (\segment -> read segment) $ concatMap (\line -> words (T.unpack line)) (T.lines text)
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

    _ <- Gtk.onWindowCloseRequest win $ do
        updateSegments
        return False

    Gtk.windowSetDefaultSize win 640 480
    Gtk.windowPresent win
