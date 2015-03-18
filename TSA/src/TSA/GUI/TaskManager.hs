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
    
    dialog <- dialogWithTitle state "Task manager"
    okButton <- dialogAddButton dialog "Ok" ResponseOk
    
    destroyedRef <- newMVar False
    refresh stateRef dialog destroyedRef
    
    handlerId <- timeoutAdd (
        do
            refresh stateRef dialog destroyedRef
            return True
        ) 5000
    on okButton buttonReleaseEvent $ liftIO $
        do
            widgetDestroy dialog
            return True
    on dialog unrealize $ 
        do
            timeoutRemove handlerId
            modifyMVar_ destroyedRef $ \_ -> do
                return True
    return ()    

refresh :: StateRef -> Dialog -> MVar Bool -> IO ()
refresh stateRef dialog destroyedRef =
    --postGUIAsync $
        modifyMVar_ destroyedRef $ \destroyed -> do 
            if destroyed
                then
                    return ()
                else do
                    --putStrLn "Tere"
                    state <- readMVar stateRef
                    contentBox <- castToBox <$> dialogGetContentArea dialog
                    containerForeach contentBox (\widget -> 
                            widgetGetName widget >>= \name -> 
                                if name == "TaskWidget" then containerRemove contentBox widget else return ()
                        )
                    case tasks state of
                        [] -> do
                            label <- labelNew (Just "No tasks")
                            hBox <- addWidget Nothing label dialog
                            widgetSetName hBox "TaskWidget"
                        otherwise ->
                            mapM_ (\(Task threadId taskName percent _) -> do
                                    progressBar <- progressBarNew
                                    progressBarSetFraction progressBar percent
                                    stopButton <- buttonNewFromStock stockStop
                                    hBox <- hBoxNew False 2
                                    boxPackStart hBox progressBar PackGrow 2
                                    boxPackStart hBox stopButton PackNatural 2
                                    addWidget (Just taskName) hBox dialog >>= (flip widgetSetName) "TaskWidget" 
                                    on stopButton buttonReleaseEvent $ liftIO $ 
                                        do
                                            killThread threadId
                                            refresh stateRef dialog destroyedRef
                                            return True
                                ) $ filter (\(Task _ taskName _ _) -> taskName /= "") $ tasks state
                    widgetShowAll dialog
            return destroyed
        