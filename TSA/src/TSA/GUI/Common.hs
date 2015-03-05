
module TSA.GUI.Common (
    modifyState, 
    modifyStateParams, 
    defaultFontFamily,
    runTask) where

import TSA.Params
import TSA.GUI.State
import GUI.Widget
import Utils.List

import Data.List
import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.SSem as SSem
import Control.Monad.IO.Class

defaultFontFamily :: String
defaultFontFamily = "Monospace"

modifyState :: StateRef -> (State -> State) -> IO ()
modifyState stateRef f = do
    modifyMVar_ stateRef $ \state -> return $ f state

modifyStateParams :: StateRef -> (Params -> Params) -> IO ()
modifyStateParams stateRef f = do
    modifyMVar_ stateRef $ \state -> return $ state {params = f (params state)}


runTask :: StateRef -> String -> IO () -> IO ()
runTask stateRef taskName task = do
    sem <- SSem.new 1
    newThreadId <- forkFinally task $ \_ -> do
        SSem.wait sem
        threadId <- myThreadId
        removeTask stateRef threadId
    state <- readMVar stateRef
    currentThreadId <- myThreadId
    let
        newTask = Task newThreadId taskName []
        (parentTask, otherTasks) = partition (\(Task threadId _ _) -> threadId == currentThreadId) (tasks state)
        updatedParentTask =
            case parentTask of
                [Task parentThreadId parentTaskName childern] -> [Task parentThreadId parentTaskName (newTask:childern)]
                [] -> []
    modifyState stateRef $ \state ->
        state {
            tasks = (newTask:(updatedParentTask ++ otherTasks))
        }
    SSem.signal sem
         
removeTask :: StateRef -> ThreadId -> IO ()
removeTask stateRef threadId = 
    modifyMVar_ stateRef $ \state -> do
        case find (\(Task threadId' _ _) -> threadId' == threadId) (tasks state) of
            Just task -> do
                let
                    removeTask' (Task threadId _ children) = do
                        killThread threadId
                        mapM_ (\(Task threadId _ _) -> killThread threadId) children
                removeTask' task 
            Nothing -> return ()
        return $ state {
            tasks = filter (\(Task threadId' _ _) -> threadId' /= threadId) (tasks state)
        }

