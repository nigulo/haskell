module TSA.Envelopes (envelopes) where


import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Functions as FS
import Regression.Utils as U
import qualified Math.Function as F

import TSA.Params
import TSA.Data

import Utils.Misc
import Utils.Concurrent

import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent
import System.Random

import Control.Applicative


envelopes :: EnvParams -> (String, String, String) -> TaskEnv -> DataUpdateFunc String -> IO (Either D.Data (Either S.Spline FS.Functions))
envelopes parms (upperName, lowerName, meanName) taskEnv (DataUpdateFunc dataUpdateFunc) =
    do
        g <- getStdGen 
        let
            upperParams = envUpperParams parms
            lowerParams = envLowerParams parms
            Just dataParms = envData parms
            sdp = head $ dataSet $ dataParms
            Left dat = subData sdp
            puFunc = progressUpdateFunc taskEnv
            
        upperSpline <- fitWithSpline_ 
            (fitPolynomRank upperParams) 
            (splineNumNodes (fitSplineParams upperParams)) 
            dat
            2
            puFunc
        let
            upperSdev = U.stdev dat (Right (Left upperSpline))

            --lowerSpline = fitWithSpline3_ 
            --    (fitPolynomRank upperParms) 
            --    (fitNumKnots upperParms) 
            --    dat;
            --lowerSdev = R.stdev dat lowerSpline;

            strictExtremaDetection = case envExtrema parms of
                EnvExtremaStrict -> True
                _ -> False
        
        let
            env 0 = envelope 
                True 
                (fitPolynomRank upperParams) 
                (splineNumNodes (fitSplineParams upperParams))
                (upperSdev / envPrecision parms) 
                strictExtremaDetection
                dat
                (\_ -> return ())
                (\spline -> dataUpdateFunc (Right (Left spline)) upperName True)
                (\dat -> dataUpdateFunc (Left dat) (upperName ++ "_") True)
            env 1 = envelope 
                False 
                (fitPolynomRank lowerParams) 
                (splineNumNodes (fitSplineParams lowerParams)) 
                (upperSdev / envPrecision parms) 
                strictExtremaDetection
                dat
                (\_ -> return ())
                (\spline -> dataUpdateFunc (Right (Left spline)) lowerName True)
                (\dat -> dataUpdateFunc (Left dat) (lowerName ++ "_") True)
        
        [upperEnv, lowerEnv] <- calcConcurrently_ env [0, 1]
            
        let 
            envMean = (upperEnv `S.add` lowerEnv) `S.divide` 2
            dataOrSpline = U.binaryOp (F.subtr) (Left dat) (Right (Left envMean)) True g
        dataUpdateFunc (Right (Left envMean)) meanName False
        dataUpdateFunc dataOrSpline ((dataName dataParms) ++ "_r") False
        return dataOrSpline
        

        
        