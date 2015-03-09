
module Utils.List (
    unique, 
    union, 
    intersection, 
    maxIndex, 
    minIndex, 
    removeAt,
    insertAt,
    updateAt,
    splitBy
    ) where

import qualified Data.List as List
--import Data.Ord
import Debug.Trace

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = 
    let xs1 = unique xs
    in
        if not (x `elem` xs1) then x:xs1
        else xs1
 
union :: Eq a => [a] -> [a] -> [a]
union [] xs = unique xs
union xs [] = unique xs
union (x1:x1s) x2s =
    if not (x1 `elem` x2s) && not (x1 `elem` x1s) then 
        x1:union x1s x2s
    else
        union x1s x2s
   
intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection (x1:x1s) x2s =
    if x1 `elem` x2s && (x1 `notElem` x1s) then
        x1:intersection x1s x2s
    else
        intersection x1s x2s


maxIndex :: (Ord a, Show a) => [a] -> Maybe Int
maxIndex xs = List.findIndex (== xMax) xs where
    xMax = (List.maximum  xs)

minIndex :: Ord a => [a] -> Maybe Int
minIndex xs = List.findIndex (== xMin) xs where
    xMin = minimum xs

removeAt :: Int -> [a] -> [a]
removeAt i xs = (take i xs) ++ (drop (i + 1) xs) 

insertAt :: Int -> a -> [a] -> [a]
insertAt i a xs = (take (i) xs) ++ [a] ++ (drop (i) xs)

updateAt :: Int -> a -> [a] -> [a]
updateAt i a xs = (take i xs) ++ [a] ++ (drop (i + 1) xs)

--------------------------------------------------------------------------------
-- | Splits the list into sublists using given delimiter
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter xs = 
    case List.elemIndex delimiter xs of
        Just i -> (take i xs):(splitBy delimiter (drop (i + 1) xs))
        Nothing -> [xs]
