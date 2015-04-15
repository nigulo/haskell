module TSA.Interpolate (interpolate) where

import Regression.Regression as R

import TSA.Params
import TSA.Data

interpolate :: Int -> String -> DataParams -> TaskEnv -> IO DataParams
interpolate method fitName dat taskEnv = do
    let 
        func i j (Left dat) _ = interpolateWithSpline dat >>= \spline -> return $ Right $ Left spline
    applyToData1 func dat fitName taskEnv

