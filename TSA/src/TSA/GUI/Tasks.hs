module TSA.GUI.Tasks (tasksDialog) where


import Graphics.UI.Gtk hiding (addWidget)

import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

import Utils.Misc

import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random

tasksDialog :: StateRef -> IO ()
tasksDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    
    dialog <- dialogWithTitle state "Tasks"
    dialogAddButton dialog "Ok" ResponseOk
    
    mapM_ (\(_, taskName) -> do
            taskLabel <- labelNew (Just taskName)
            addWidget Nothing taskLabel dialog 
        ) $ tasks state
        
    widgetShowAll dialog
    response <- dialogRun dialog
        
    widgetDestroy dialog


