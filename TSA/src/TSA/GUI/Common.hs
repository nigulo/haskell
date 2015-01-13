
module TSA.GUI.Common (
    modifyState, 
    modifyStateParams, 
    addGraphTab, 
    setNotebookEvents) where

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Graph as Graph

import Graphics.UI.Gtk

import GUI.Widget
import Utils.List

import Control.Concurrent.MVar
import Control.Monad.IO.Class

modifyState :: StateRef -> (State -> State) -> IO ()
modifyState stateRef f = do
    modifyMVar_ stateRef $ \state -> return $ f state

modifyStateParams :: StateRef -> (Params -> Params) -> IO ()
modifyStateParams stateRef f = do
    modifyMVar_ stateRef $ \state -> return $ state {params = f (params state)}

addGraphTab :: StateRef -> Maybe String -> IO Int
addGraphTab stateRef maybeGraphName =
    do
        state <- readMVar stateRef
        let
            notebook = getGraphTabs state
        pageIndex <- notebookGetNPages notebook
        page <- drawingAreaNew
        widgetModifyBg page StateNormal (Color 65535 65535 65535)
        let
            removeTab page =
                do
                    numPages <- notebookGetNPages notebook
                    if numPages > 2 
                        then
                            do
                                Just pageIndex <- notebookPageNum notebook page
                                if pageIndex == numPages - 2
                                    then
                                        notebookPrevPage notebook
                                    else
                                        return ()
                                notebookRemovePage notebook pageIndex
                                modifyMVar_ stateRef $ \state -> return $ state {graphTabs = (removeAt pageIndex (graphTabs state))}
                        else 
                            return ()
            tabName = 
                case maybeGraphName of
                    Nothing -> "Graph " ++ show pageIndex
                    Just graphName -> graphName
        label <- labelWithButton (Just tabName) stockRemove (removeTab page)
        
        case maybeGraphName of
            Nothing ->
                modifyMVar_ stateRef $ \state -> return $ state {graphTabs = (insertAt (pageIndex - 1) (newGraphTab tabName) (graphTabs state))}
            Just _ -> 
                return ()
        
        notebookInsertPageMenu notebook page label label (pageIndex - 1)
        notebookSetCurrentPage notebook (pageIndex - 1)
        setNotebookEvents stateRef
        widgetShowAll label
        widgetShowAll notebook
        return (pageIndex - 1)

setNotebookEvents :: StateRef -> IO ()
setNotebookEvents stateRef =
    do
        state <- readMVar stateRef
        let
            notebook = getGraphTabs state
            
        on notebook switchPage $
            \i ->
                do 
                    modifyMVar_ stateRef $ \state -> return $ updateGuiChanged True state
                    numPages <- notebookGetNPages notebook
                    if numPages > 1 && i == numPages - 1 
                        then 
                            do
                                    addGraphTab stateRef Nothing
                                    notebookPrevPage notebook
                                    return ()
                        else return ()
            
        numPages <- notebookGetNPages notebook
    
        let    
            mapOp i = 
                do    
                    Just page <- notebookGetNthPage notebook i
                    widgetAddEvents page [ButtonMotionMask, ButtonPressMask, ButtonReleaseMask]
                    on page draw $ liftIO (Graph.drawGraph stateRef Nothing)
                    on page buttonPressEvent $
                        do 
                            button <- eventButton
                            modifiers <- eventModifier
                            click <- eventClick
                            (x, y) <- eventCoordinates
                            timestamp <- eventTime
                            liftIO $ Graph.onMouseButton stateRef button modifiers click (x, y) timestamp
                    on page buttonReleaseEvent $ 
                        do 
                            button <- eventButton
                            modifiers <- eventModifier
                            click <- eventClick
                            (x, y) <- eventCoordinates
                            timestamp <- eventTime
                            liftIO $ Graph.onMouseButton stateRef button modifiers click (x, y) timestamp
                    on page motionNotifyEvent $ 
                        do 
                            (x, y) <- eventCoordinates
                            modifiers <- eventModifier
                            liftIO $ Graph.onMouseMove stateRef (x, y) modifiers
                    on page scrollEvent $
                        do
                            (x, y) <- eventCoordinates
                            direction <- eventScrollDirection
                            liftIO $ Graph.onMouseScroll stateRef (x, y) direction
        mapM_ mapOp [0 .. numPages - 2] 


