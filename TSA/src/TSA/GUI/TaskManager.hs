module TSA.GUI.TaskManager (taskManagerDialog) where


import Graphics.UI.Gtk hiding (addWidget)

import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative
import Control.Monad.IO.Class

import System.Random

taskManagerDialog :: StateRef -> IO ()
taskManagerDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    
    dialog <- dialogWithTitle state "Tasks"
    dialogAddButton dialog "Ok" ResponseOk
    
    mapM_ (\(Task threadId taskName _) -> do
            stopButton <- buttonNewFromStock stockStop
            on stopButton buttonReleaseEvent $ liftIO (killThread threadId >> return True)
            addWidget (Just taskName) stopButton dialog 
        ) $ filter (\(Task _ taskName _) -> taskName /= "") $ tasks state
        
    widgetShowAll dialog
    response <- dialogRun dialog
        
    widgetDestroy dialog


