
module TSA.GUI.Common (
    modifyState, 
    modifyStateParams, 
    defaultFontFamily,
    runTask) where

import TSA.Params
import TSA.GUI.State
import GUI.Widget
import Utils.List

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
    threadId <- forkFinally task $ \_ -> do
        SSem.wait sem
        threadId <- myThreadId
        removeTask stateRef threadId
    modifyState stateRef $ \state -> state {
            tasks = (tasks state) ++ [(threadId, taskName)]
        }
    SSem.signal sem

removeTask :: StateRef -> ThreadId -> IO ()
removeTask stateRef threadId = 
    modifyState stateRef $ \state -> state {
            tasks = filter (\(threadId', _) -> threadId' /= threadId') (tasks state)
        }
