module Main where

import System.Environment
import Filesystem
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    (arg1:arg2:arg3:args) <- getArgs
    let
        fileName = arg1
        n  = read arg2
        delta = read arg3
    byteStr <- B.readFile fileName
    let
        vals = map (\line -> map (read) (words line)) $ lines $ UTF8.decode $ B.unpack byteStr
        xs = map (\row -> init row) vals
        rs = map (\row -> last row) vals
        gradAsc w0 ws = do
            let
                l = foldl (\res (xst, rt) -> 
                        let
                            yt = y w0 ws xst
                        in
                            res + rt * log yt + (1 - rt) * log (1 - yt)
                    ) 0 (zip xs rs)
                (partialW0, partialWs) = foldl (\(res0, res) (xst, rt) -> 
                        let
                            yt = y w0 ws xst
                        in
                            (res0 + rt  - yt, map (\(resi, x) -> resi + (rt - yt) * x) (zip res xst))
                    ) (0, repeat 0) (zip xs rs) 
                w01 = w0 + n * partialW0
                ws1 = zipWith (\w partialW -> w + n * partialW) ws partialWs
            putStrLn $ "w01, ws1: " ++ show (w01 - w0) ++ ", " ++ show (zipWith (-) ws1 ws)
            --putStrLn $ "L: " ++ show l
            if abs (w01 - w0) < delta && all (\(w1, w) -> abs (w1 - w) < delta) (zip ws1 ws)
                then return (w01, ws1)
                else gradAsc w01 ws1
    (w0, ws) <- gradAsc 0 (repeat 0)
    let
        ys = map (\xst -> y w0 ws xst) xs
    putStrLn $ concatMap (\yt -> (show yt) ++ "\n") ys
        
y :: Double -> [Double] -> [Double] -> Double        
y w0 ws xst = sigmoid ((foldl (\res (w, x) -> res + w * x) 0 (zip ws xst)) + w0)

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp(-x))
