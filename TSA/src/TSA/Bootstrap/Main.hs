module Main where

import Math.Function as F
import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import Regression.Bootstrap as B
import TSA.LeastSquares
import TSA.Extrema
import TSA.Params
import TSA.Data

import System.Random
import System.Environment

import qualified Utils.Xml as Xml
--import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import System.FileLock
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String
import qualified Data.Vector.Unboxed as V


main :: IO ()
main = do --mpiWorld $ \size rank ->
    fileLock <- lockFile "lock" Exclusive
    byteStr <- B.readFile "lock"
    args <- getArgs
    let
        rank = read $ decode (B.unpack byteStr)
        precision = if length args > 0 then read (head args) else 1000000
    B.writeFile "lock" (B.pack (encode (show (rank + 1))))
        
    spline <- Xml.parseFromFile "spline" "spline" >>= \doc -> return (Xml.fromDocument doc)
    dat <- Xml.parseFromFile "data" "data" >>= \doc -> return (Xml.fromDocument doc)
    fitParams <- Xml.parseFromFile "fitparams" "fitparams" >>= \doc -> return (Xml.fromDocument doc)
    g <- getStdGen 
    if rank == 0 then
        do
            let 
                splineParams = createDataParams_ ("spline" ++ show rank) [createSubDataParams__ (Right (Left spline))]
                Left diff = U.binaryOp F.subtr (Left dat) (Right (Left spline)) True g
            Xml.renderToFile (Xml.toDocument diff) "diff"
            splineExtrema <- findExtrema splineParams precision (dataName splineParams) (\_ -> return ())
            let
                Left minima = subData $ head $ dataSet $ snd splineExtrema
            Xml.renderToFile (Xml.toDocument minima) "minima"
    else
        return ()
    unlockFile fileLock

    diff <- Xml.parseFromFile "data" "diff" >>= \doc -> return (Xml.fromDocument doc)
    minima <- Xml.parseFromFile "data" "minima" >>= \doc -> return (Xml.fromDocument doc)
    
    let
        byteStr = B.pack (encode (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList (D.xys1 minima))))
    --B.writeFile ("all_minima") byteStr
    
    bsSpline <- B.bootstrapSpline rank (fitData fitParams) spline dat diff
    let bsSplineParams = createDataParams_ ("bsSpline" ++ show rank) [createSubDataParams__ (Right (Left bsSpline))]
    bsSplineExtrema <- findExtrema bsSplineParams precision (dataName bsSplineParams) (\_ -> return ())
    let
        Left bsMinima = subData $ head $ dataSet $ snd bsSplineExtrema
        xysMinima = D.xys1 minima
        xysBsMinima = D.xys1 bsMinima
    
        xysMinimaDiff = V.map (\(x, y) -> 
                let
                    (smaller, greather) = V.partition (\(bsX, _) -> bsX < x) xysBsMinima
                in
                    if V.null smaller then
                        let
                            (bsX, bsY) = V.head greather
                        in 
                            (x - bsX, y - bsY)
                    else if V.null greather then
                        let
                            (bsX, bsY) = V.last smaller
                        in 
                            (x - bsX, y - bsY)
                    else 
                        let
                            (bsX1, bsY1) = V.head greather
                            (bsX2, bsY2) = V.last smaller
                            diff1 = x - bsX1
                            diff2 = x - bsX2
                        in
                            if (abs diff1) - (abs diff2) > 0 then (diff2, y - bsY2) else (diff1, y - bsY1)
            ) xysMinima
        byteStr = B.pack (encode (concatMap (\(diffX, diffY) -> show diffX ++ " " ++ show diffY ++ "\n") (V.toList xysMinimaDiff)))
    B.writeFile ("minimadiff" ++ show rank) byteStr
    
    --    byteStr = B.pack (encode (concatMap (\(x, y) -> show x ++ " " ++ show y ++ "\n") (V.toList xysBsMinima)))
    --B.writeFile ("minima" ++ show rank) byteStr

    return ()
    