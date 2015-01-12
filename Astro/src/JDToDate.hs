module Main where

import Data.List as List
import System.Environment
import Filesystem
import Control.Concurrent.MVar
import Control.Monad

import Filesystem.Path.CurrentOS as F
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String as UTF8
import qualified Data.Vector.Unboxed as V

import Astro.Ephem.Types

main :: IO ()
main = do
    args <- getArgs
    let
        
        inFileName = if length args > 0 then head args else "input.dat"
        outFileName = if length args > 1 then (args !! 1) else "output.dat"
    inStr <- B.readFile inFileName
    let
        ymds = map (\line -> let jd = JD (read line) in toYMD jd) $ filter (\line -> words line /= []) (lines $ UTF8.decode $ B.unpack inStr)
        outStr = B.pack (UTF8.encode (concatMap (\(YMD y m d) -> show y ++ "-" ++ show m ++ "-" ++ show d ++ "\n") ymds))
    B.writeFile outFileName outStr
        