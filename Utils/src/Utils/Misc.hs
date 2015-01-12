
module Utils.Misc (for, for_, for__, forM, forM_, forM__, 
        forl, forl_, forMl, forMl_,
        left, right,
        isLeft, isRight, maybe_, randomGens,
        nubVector, sortVectorBy, sortVector, 
        groupVectorBy, groupVector) where

import Debug.Trace
import System.Random
import qualified Data.Vector.Unboxed as V
import qualified Data.List as List

--------------------------------------------------------------------------------
-- | This function represents a Java/C like for loop in the form:
-- | val = val0;
-- | for (i = i0; cond i; inc i) {
-- |    val = func i val;
-- | }
for :: Num a => a              -- ^ initial counter value
             -> (a -> Bool)    -- condition to check 
             -> (a -> a)       -- ^ incremental function
             -> b              -- ^ input value
             -> c              -- ^ initual return value
             -> (a -> b -> c -> c) -- ^ function to run on every step
             -> c              -- ^ final value returned
for i cond inc inVal retVal f  = 
    if cond i then 
        for (inc i) cond inc inVal (f i inVal retVal) f
    else 
        retVal
--------------------------------------------------------------------------------
-- | This function represents a Java/C like for loop in the form:
-- | val = val0;
-- | for (i = i0; i <= n; i++) {
-- |    val = func i val;
-- | }
for_ :: Real a => a            -- ^ initial counter value
             -> a              -- final counter value (including)
             -> b              -- ^ input value
             -> c              -- ^ initual return value
             -> (a -> b -> c -> c) -- ^ function to run on every step
             -> c              -- ^ final value returned
for_ i n inVal retVal f = for i (<= n) (+1) inVal retVal f

for__ :: Real a => a            -- ^ initial counter value
             -> a              -- final counter value (including)
             -> c              -- ^ initual return value
             -> (a -> c -> c) -- ^ function to run on every step
             -> c              -- ^ final value returned
for__ i n retVal f = for_ i n () retVal (\j _ retVal -> f j retVal)


forl :: [a]              -- ^ initial counter value
             -> b              -- ^ input value
             -> c              -- ^ initual return value
             -> (a -> b -> c -> c) -- ^ function to run on every step
             -> c              -- ^ final value returned
forl is inVal retVal f  = 
    --forList is inVal (f i inVal retVal) f 
    List.foldl' (\r i -> f i inVal r) retVal is

forl_ :: [a]              -- ^ initial counter value
             -> b              -- ^ input value
             -> (a -> b -> b) -- ^ function to run on every step
             -> b              -- ^ final value returned
forl_ is retVal f  = forl is () retVal (\j _ retVal -> f j retVal)

--------------------------------------------------------------------------------
forM :: (Num a, Monad m) => 
                 a              -- ^ initial counter value
             -> (a -> Bool)    -- condition to check 
             -> (a -> a)       -- ^ incremental function
             -> b              -- ^ input value
             -> c              -- ^ initual return value
             -> (a -> b -> c -> m c) -- ^ function to run on every step
             -> m c              -- ^ final value returned
forM i cond inc inVal retVal f  = 
        if not (cond i)
            then 
                do
                    return retVal
            else
                do
                    val2 <- (f i inVal retVal)
                    forM (inc i) cond inc inVal val2 f


forM_ :: (Real a, Monad m) => a            -- ^ initial counter value
             -> a              -- final counter value (including)
             -> b              -- ^ input value
             -> c              -- ^ initual return value
             -> (a -> b -> c -> m c) -- ^ function to run on every step
             -> m c              -- ^ final value returned
forM_ i n inVal retVal f = forM i (<= n) (+1) inVal retVal f

forM__ :: (Real a, Monad m) => a            -- ^ initial counter value
             -> a              -- final counter value (including)
             -> b              -- ^ initual return value
             -> (a -> b -> m b) -- ^ function to run on every step
             -> m b              -- ^ final value returned
forM__ i n retVal f = forM_ i n () retVal (\j _ retVal -> f j retVal)


forMl :: (Num a, Monad m) => [a]              -- ^ initial counter value
             -> b              -- ^ input value
             -> c              -- ^ initual return value
             -> (a -> b -> c -> m c) -- ^ function to run on every step
             -> m c              -- ^ final value returned
forMl is inVal retVal f  = 
    List.foldl' (\r i -> r >>= f i inVal) (return retVal) is

forMl_ :: (Num a, Monad m) => [a]              -- ^ initial counter value
             -> b              -- ^ input value
             -> (a -> b -> m b) -- ^ function to run on every step
             -> m b              -- ^ final value returned
forMl_ is retVal f  = forMl is () retVal (\j _ retVal -> f j retVal)


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
