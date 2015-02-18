module Main (main) where

import TSA.D2
import TSA.Params
import qualified Utils.Xml as Xml
import System.Environment
import System.IO


main :: IO ()
main = do
    [periodStartS, periodEndS, corrStartS, corrEndS, methodS, precisionS, bootstrapCountS] <- getArgs
    let
        periodStart = read periodStartS 
        periodEnd = read periodEndS 
        corrLenStart = read corrStartS
        corrLenEnd = read corrEndS
        method = read methodS
        precision = read precisionS
        bootstrapCount = read bootstrapCountS
        
    handle <- openFile ("result") AppendMode
    let
        logFunc str = hPutStr handle (str ++ "\n")

    dp <- Xml.parseFromFile "data" "data" >>= \doc -> return (Xml.fromDocument doc)
    dispersions <- calcDispersions dp periodStart periodEnd corrLenStart corrLenEnd method precision (dataName dp ++ "_period") False (\_ -> return ()) (logFunc)

    mapM_ (\i -> do
        calcDispersions dp periodStart periodEnd corrLenStart corrLenEnd method precision (dataName dp ++ "_period" ++ (show i)) True (\_ -> return ()) (logFunc)
        return ()
        ) [1 .. bootstrapCount]
    hClose handle
    
    