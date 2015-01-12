module Main where

import Math.Function as F
import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import Regression.Bootstrap as B
import TSA.LeastSquares
import TSA.Extrema
import TSA.Params

import System.Random

import qualified Utils.Xml as Xml
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String
import qualified Data.Vector.Unboxed as V


main :: IO ()
main = do --mpiWorld $ \size rank ->
        
    spline <- Xml.parseFromFile "spline" "spline" >>= \doc -> return (Xml.fromDocument doc)
    dat <- Xml.parseFromFile "data" "data" >>= \doc -> return (Xml.fromDocument doc)
    --fitParams <- Xml.parseFromFile "fitparams" "fitparams" >>= \doc -> return (Xml.fromDocument doc)
    g <- getStdGen 
    do
            let splineParams = DataParams {
                dataName = "spline",
                dataSet = [
                    SubDataParams {
                        subData = Right (Left spline),
                        subDataBootstrapSet = []
                    }
                ]
            }
            let 
                Left diff = U.binaryOp F.subtr (Left dat) (Right (Left spline)) True g
            Xml.renderToFile (Xml.toDocument diff) "diff"
            splineExtrema <- findExtrema splineParams 1000000 (dataName splineParams) (\_ -> return ())
            let
                Left minima = subData $ head $ dataSet $ snd splineExtrema
            Xml.renderToFile (Xml.toDocument minima) "minima"

    return ()
    
