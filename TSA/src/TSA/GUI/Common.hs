
module TSA.GUI.Common (
    modifyState, 
    modifyStateParams, 
    defaultFontFamily) where

import TSA.Params
import TSA.GUI.State
import GUI.Widget
import Utils.List

import Graphics.UI.Gtk
import Control.Concurrent.MVar
import Control.Monad.IO.Class

defaultFontFamily :: String
defaultFontFamily = "Monospace"

modifyState :: StateRef -> (State -> State) -> IO ()
modifyState stateRef f = do
    modifyMVar_ stateRef $ \state -> return $ f state

modifyStateParams :: StateRef -> (Params -> Params) -> IO ()
modifyStateParams stateRef f = do
    modifyMVar_ stateRef $ \state -> return $ state {params = f (params state)}

