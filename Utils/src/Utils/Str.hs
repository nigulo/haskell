
module Utils.Str (trim, keyValuePairs, stringIndex, stringIndices, substring, replaceAll) where

import Data.Char
import Data.List

-- Removes heading or trailing whitespaces from String
trim :: String -> String
trim str = 
    if (length str <= 0) then str
    else let fstChr = head str
               in 
            if (isSpace fstChr) then trim (tail str)
            else let lstChr = last str
                in
                    if (isSpace lstChr) then trim (init str)
                    else str
                         
-- Splits a String into key and value pairs. The input String must be in format
-- "key1 = value1\nkey2 = value2\n ..."
keyValuePairs :: String -> [(String, String)]
keyValuePairs str = let (keys, values) = unzip (map (break (== '=')) (lines str))
                                        in zip (map trim keys) (map (trim.tail) values)

stringIndex :: String -> String -> Maybe Int
stringIndex [] _ = Nothing
stringIndex _ [] = Nothing
stringIndex strToFind str =
    stringIndex1 strToFind str 0 where
        stringIndex1 _ [] _ = Nothing
        stringIndex1 strToFind str i =
            if isPrefixOf strToFind str then Just i
            else stringIndex1 strToFind (tail str) (i + 1)

stringIndices :: String -> String -> [Int]
stringIndices strToFind str =
    case stringIndex strToFind str of
        Nothing -> []
        Just i -> i:(map (\index -> index + i + 1) $ stringIndices strToFind (drop (i + 1) str))

substring :: String -> Int -> Int -> String
substring str startIndex endIndex = 
    take (endIndex - startIndex) $ drop startIndex str

replaceAll :: String -> String -> String -> String
replaceAll "" _ str = str
replaceAll strToFind newStr str =
    let
        strIndex = stringIndex strToFind str
    in
        case strIndex of
            Just i -> (substring str 0 i) ++ newStr ++ (replaceAll strToFind newStr ((substring str (i + length strToFind) (length str))))
            Nothing -> str 

    