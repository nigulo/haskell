module TSA.GUI.Preferences (preferencesDialog) where


import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox
import qualified Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.AnalyticData
import Regression.Data as D
import Regression.Utils
import qualified Math.Function as F
import qualified Math.Expression as E

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

preferencesDialog :: StateRef -> IO ()
preferencesDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        Just sel = graphSelection graphParms

        params = settingsParams state
    
    dialog <- dialogWithTitle state "Preferences"
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk
    
    saveChangesOnExitCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button (settingsSaveChangesOnExit params) >> return button
    addWidget (Just "Save changes on exit: ") saveChangesOnExitCheck dialog 

    saveZippedCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button (settingsSaveInZippedFormat params) >> return button
    addWidget (Just "Save in zipped format: ") saveZippedCheck dialog 

    widgetShowAll dialog
    response <- dialogRun dialog
        
    if response == ResponseOk 
        then
            do
                saveChanges <- toggleButtonGetActive saveChangesOnExitCheck
                saveZipped <- toggleButtonGetActive saveZippedCheck
                widgetDestroy dialog
                modifyMVar_ stateRef $ \state -> return $ state {settingsParams = params {settingsSaveChangesOnExit = saveChanges, settingsSaveInZippedFormat = saveZipped}}
                return ()
        else
            widgetDestroy dialog

