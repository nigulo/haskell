
module TSA.GUI.Common (
    modifyState, 
    modifyStateParams, 
    defaultFontFamily,
    runTask,
    initTask,
    taskEnv) where

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
    initTask stateRef taskName newThreadId
    SSem.signal sem

removeTask :: StateRef -> ThreadId -> IO ()
removeTask stateRef threadId = 
    modifyMVar_ stateRef $ \state -> do
        case find (\(Task threadId' _ _ _) -> threadId' == threadId) (tasks state) of
            Just (Task _ _ _ children) -> mapM_ (\(Task threadId _ _ _) -> killThread threadId) children
            Nothing -> return ()
        return $ state {
            tasks = filter (\(Task threadId' _ _ _) -> threadId' /= threadId) (tasks state)
        }

initTask :: StateRef -> String -> ThreadId -> IO ()
initTask stateRef taskName newThreadId = do
    state <- readMVar stateRef
    currentThreadId <- myThreadId
    let
        newTask = Task newThreadId taskName 0 []
        (parentTask, otherTasks) = partition (\(Task threadId _ _ _) -> threadId == currentThreadId) (tasks state)
        updatedParentTask =
            case parentTask of
                [Task parentThreadId parentTaskName percent childern] -> [Task parentThreadId parentTaskName percent (newTask:childern)]
                [] -> []
    modifyState stateRef $ \state ->
        state {
            tasks = (newTask:(updatedParentTask ++ otherTasks))
        }

progressUpdate :: StateRef -> ThreadId -> Double -> IO ()
progressUpdate stateRef threadId percent = do
    modifyMVar_ stateRef $ \state -> do
        case find (\(Task threadId' _ _ _) -> threadId' == threadId) (tasks state) of
            Just (Task threadId taskName _ children) -> 
                return $ state {
                    tasks = Task threadId taskName percent children:filter (\(Task threadId' _ _ _) -> threadId' /= threadId) (tasks state)
                }
            Nothing -> return state



taskEnv :: StateRef -> IO TaskEnv
taskEnv stateRef = do
    threadId <- myThreadId
    return TaskEnv {
            progressUpdateFunc = progressUpdate stateRef threadId, 
            logFunc = appendLog stateRef,
            taskInitializer = initTask stateRef "",
            taskFinalizer = myThreadId >>= \threadId -> removeTask stateRef threadId
        }
    