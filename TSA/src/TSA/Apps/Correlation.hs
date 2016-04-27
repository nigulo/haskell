module Main (main) where

import Regression.Data as D
import TSA.Correlation
import TSA.Data
import TSA.Params
import Utils.IO

import qualified Data.Vector.Unboxed as V
import System.Environment

numLinesToSkip = 1

main :: IO ()
main = do
    [fileName1, fileName2, precisionS, shiftS] <- getArgs >>= \args ->
        case args of
            [fileName1, fileName2, precisionS, shiftS] -> return args
            [fileName1, fileName2, precisionS] -> return [fileName1, fileName2, precisionS, "0"]
            [fileName1, fileName2] -> return [fileName1, fileName2, "10000", "0"]
    let
        precision = read precisionS
        shift = read shiftS
        shifts = if shift == 0 then [shift] else [-shift, -shift + 2 * shift / 100 .. shift]

    str1 <- Utils.IO.readFromFile fileName1
    let
        xys1 :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ drop numLinesToSkip $ lines $ str1
        dat1 = D.data1' $ V.fromList xys1
    str2 <- Utils.IO.readFromFile fileName2
    let
        xys2 :: [(Double, Double)] = map (\line -> let [xStr, yStr] = words line in (read xStr, read yStr)) $ drop numLinesToSkip $ lines $ str2
        dat2 = D.data1' $ V.fromList xys2

    findCorrelation defaultTaskEnv (createDataParams_ "dat1" [createSubDataParams__ (SD1 dat1)]) (createDataParams_ "dat2" [createSubDataParams__ (SD1 dat2)]) precision shifts "correlation"
    return ()

    
