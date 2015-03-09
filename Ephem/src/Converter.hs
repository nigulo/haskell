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
import Data.Char
import Utils.List

import Ephem.Types

readDate :: String -> String -> Date
readDate "ymd" str =
    case words str of
        (y:m:d:_) -> YMD (read y) (read m) (read d)
        otherwise ->
            case splitBy '-' str of
                (y:m:d:_) -> YMD (read y) (read m) (read d)
                otherwise -> YMD (read (take 4 str)) (read (take 2 (drop 4 str))) (read (drop 6 str))
readDate "jd" str = JD (read str)
readDate "tropicalyears" str = TropicalYears (read str)
readDate "years" str = TropicalYears (read str)
readDate "rjd" str = RJD (read str)
readDate "mjd" str = MJD (read str)
readDate "tjd" str = TJD (read str)

convertDate :: String -> Date -> Date
convertDate "ymd" d = toYMD d
convertDate "jd" d = toJD d
convertDate "tropicalyears" d = toTropicalYears d
convertDate "years" d = toTropicalYears d
convertDate "rjd" d = toRJD d
convertDate "mjd" d = toMJD d
convertDate "tjd" d = toTJD d

showDate :: Date -> String
showDate (YMD y m d) = show y ++ "-" ++ show m ++ "-" ++ show d
showDate (JD jd) = show jd
showDate (TropicalYears years) = show years
showDate (RJD rjd) = show rjd
showDate (MJD mjd) = show mjd
showDate (TJD tjd) = show tjd

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then putStrLn "Usage: Converter from_type to_type [input_file] [output_file]"
    else do
        let
            fromType = map (toLower) $ head args
            toType = map (toLower) $ args !! 1
            inFileName = if length args > 2 then (args !! 2) else "input.dat"
            outFileName = if length args > 3 then (args !! 3) else "output.dat"
        inStr <- B.readFile inFileName
        let
            dates = map (convertDate toType . readDate . fromType) $ filter (\line -> words line /= []) (lines $ UTF8.decode $ B.unpack inStr)
            outStr = B.pack (UTF8.encode (concatMap (\d -> showDate d ++ "\n") dates))
        B.writeFile outFileName outStr
