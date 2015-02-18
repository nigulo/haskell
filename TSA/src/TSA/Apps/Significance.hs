module Main where

import Math.Function as F
import Regression.Spline as S
import Regression.Data as D
import Regression.Utils as U
import Regression.Bootstrap as B
import TSA.LeastSquares
import TSA.Extrema
import TSA.Params

import Data.List as List
import System.Random
import System.Environment
import Filesystem
import Control.Concurrent.MVar
import Control.Monad

import qualified Utils.Xml as Xml
--import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Filesystem.Path.CurrentOS as F
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String as UTF8
import qualified Data.Vector.Unboxed as V

import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Test.KolmogorovSmirnov
import qualified Statistics.Sample as Sample

import System.Random
import System.Random.MWC

main :: IO ()
main = do --mpiWorld $ \size rank ->
    args <- getArgs
    let
        rank = read $ head args
    minima <- Xml.parseFromFile "data" "minima" >>= \doc -> return (Xml.fromDocument doc)
    let xysMinima = D.xs1 minima
    mapM_ (\j -> do
            workDir <- getWorkingDirectory
            fileNames <- listDirectory workDir >>= \names -> filterM (isFile) names
            dists <- mapM (\fileName -> do
                byteStr <- Filesystem.readFile fileName
                let
                    diffs :: [(Double, Double)] = map (\line -> let [diffXStr, diffYStr] = words line in (read diffXStr, read diffYStr)) $ lines $ UTF8.decode $ B.unpack byteStr
                return $ take 100 $ drop j diffs
                ) (filter (\name -> isPrefixOf "minimadiff" (F.encodeString (filename name))) fileNames)
            let
                distsT = List.transpose dists
                
            mapM_ (\(dist, k) -> do
                    let
                        distX = map fst dist
                        minDist = minimum distX
                        maxDist = maximum distX
                        step = (maxDist - minDist) / 10
                        distribution = map (\x ->
                            (x, length $ filter (\d -> d >= x && d < x + step) distX)
                            ) [minDist, minDist + step .. maxDist]
                        byteStr1 = B.pack (UTF8.encode (concatMap (\(x, d) -> show x ++ " " ++ show d ++ "\n") distribution))
                        byteStr2 = B.pack (UTF8.encode (concatMap (\x -> show x ++ "\n") distX))
                    --B.writeFile ("hist" ++ show (j + k)) byteStr1
                    B.writeFile ("dist" ++ show (j + k)) byteStr2
                ) (zip distsT [2])
               
            rndVect :: V.Vector Int <- withSystemRandom . asGenST $ \gen -> uniformVector gen 20

            let
                stats = zipWith (\dist k ->
                        let
                            --x = xysMinima V.! (j + k)
                            --xMax = x - (if j + k > 0 then xysMinima V.! (j + k - 1) else -100000) 
                            --xMin = x - (if j + k < V.length xysMinima - 1 then xysMinima V.! (j + k + 1) else 100000) 
                            (distX, distY) = V.unzip (V.fromList dist)
                            --filteredXDist = V.filter (\x -> x > xMin / 2 && x < xMax / 2) distX
                            sampledDistX = V.map (\r -> distX V.! (r `mod` V.length distX)) rndVect
                            nx = normalFromSample sampledDistX
                            ny = normalFromSample distY
                        in 
                            (( {-(fromIntegral (V.length filteredXDist) :: Double) / fromIntegral (V.length distX), -} Sample.mean distX, Sample.stdDev distX, kolmogorovSmirnovD nx sampledDistX, kolmogorovSmirnovTest nx 0.01 sampledDistX), (mean ny, stdDev ny, kolmogorovSmirnovD ny distY, kolmogorovSmirnovTest ny 0.01 distY)) 
                    ) distsT [0 ..]
            mapM_ (\((({- weightX ,-} meanX, stDevX, ksStatX, testResX), (meanY, stDevY, ksStatY, testResY)), k) -> putStrLn (show (j + k) {- ++ " " ++ show weightX -} ++ " " ++ " " ++ show meanX ++ " " ++ show stDevX ++ " " ++ show ksStatX ++ " " ++ show testResX ++ " " ++ show meanY ++ " " ++ show stDevY ++ " " ++ show ksStatY ++ " " ++ show testResY)
                ) (zip stats [0 ..])
        ) [2000 * rank, 2000 * rank + 100 .. 2000 * (rank + 1) - 1]