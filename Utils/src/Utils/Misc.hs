
module Utils.Misc (
        left, right,
        isLeft, isRight, maybe_, randomGens,
        nubVector, sortVectorBy, sortVector, 
        groupVectorBy, 
        groupVector,
        segmentVector,
        applyToVectorWithRandomGen
    ) where

import Debug.Trace
import System.Random
import qualified Data.Vector.Unboxed as V
import qualified Data.List as List

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight = not.isLeft

left :: Either a b -> a
left (Left x) = x

right :: Either a b -> b
right (Right x) = x

maybe_ :: a -> Maybe a -> a
maybe_ a = maybe a id

randomGens :: RandomGen g => g -> [g]
randomGens g =
    let
        (g1, g2) = System.Random.split g
    in g:g1:randomGens g2

nubVector :: (V.Unbox a, Eq a) => V.Vector a -> V.Vector a
nubVector v = 
    if (V.length v == 0) 
        then V.empty
        else
        let
            i = V.init v
            l = V.last v
        in
           if l `V.notElem` i then (nubVector i) `V.snoc` l else (nubVector i)  

sortVectorBy :: (V.Unbox a, Eq a) => (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortVectorBy f v = 
    if (V.length v == 0) 
        then V.empty
        else
        let
            i = V.minIndexBy f v
            h = V.take i v
            t = V.drop (i + 1) v
        in
           (v V.! i) `V.cons` (sortVectorBy f (h V.++ t))  

sortVector :: (V.Unbox a, Eq a, Ord a) => V.Vector a -> V.Vector a
sortVector = sortVectorBy compare

groupVectorBy :: (V.Unbox a, Eq a) => (a -> a -> Bool) -> V.Vector a -> [V.Vector a]
groupVectorBy f v =
    if V.null v then []
    else 
    let
        val = V.head v
        (g1, g2) = V.span (\a -> f a val) v 
    in
        g1:groupVectorBy f g2

groupVector :: (V.Unbox a, Eq a) => V.Vector a -> [V.Vector a]
groupVector = groupVectorBy (==)

-- | Generalization of span/break/partition
segmentVector :: (V.Unbox a, Eq a) => (a -> Bool) -> V.Vector a -> ([V.Vector a], [V.Vector a])
segmentVector f v = 
    if V.null v
        then ([], [])
        else
            let
                (v1, v2) = V.span f v
            in
                if V.null v1 
                    then 
                        let
                            (v2, v1) = V.break f v
                            (v21s, v22s) = segmentVector f v1 
                        in
                            (v21s, v2:v22s)
                    else
                        let
                            (v21s, v22s) = segmentVector f v2 
                        in
                            (v1:v21s, v22s)

applyToVectorWithRandomGen :: (V.Unbox a, V.Unbox b, RandomGen g) => (a -> g -> b) -> V.Vector a -> g -> V.Vector b
applyToVectorWithRandomGen fn v g = 
    if (V.null v) 
        then 
            V.empty
        else
            let
                (g1, g2) = System.Random.split g
            in
                fn (V.head v) g1 `V.cons` applyToVectorWithRandomGen fn (V.tail v) g2 
