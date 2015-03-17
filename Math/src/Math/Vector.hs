module Math.Vector ( 
                    Vector,
                    vector,
                    nullVector, 
                    get, 
                    set, 
                    setAll,
                    elemOp,
                    op,
                    getLength,
                    values,
                    add
                    ) where

import Debug.Trace
import Data.Array


newtype Num a => Vector a = Vector (Array Int a) deriving Show 

-- creates a new vector
vector :: Num a => [a] -> Vector a
vector list = Vector (listArray (0, length list - 1) list)

-- creates a null vector of given length
nullVector :: Num a => Int -> Vector a
nullVector len = Vector (listArray (0, len - 1) (take len (repeat 0)))

-- returns the element at given index
get :: Num a => Int -> Vector a -> a
get i (Vector v) = v ! i

-- sets the element at given index
set :: Num a => Int -> a -> Vector a -> Vector a
set i value (Vector v) = Vector (v // [(i, value)])

setAll :: Num a => [(Int, a)] -> Vector a -> Vector a
setAll values (Vector v) = Vector (v // values)

elemOp :: Num a => Int -> (a -> a) -> Vector a -> Vector a
elemOp i f v = set i (f (get i v)) v

getLength :: Num a => Vector a -> Int
getLength (Vector v) = u - l + 1 where
    (l, u) = bounds v

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct (Vector v1) (Vector v2) =
    sum [v1 ! i * v2 ! i | i <- indices v1]

op :: Num a => (a -> a -> a) -> Vector a -> Vector a -> Vector a
op f (Vector v1) (Vector v2) = 
    let v2assocs = assocs v2
    in Vector (accum f v1 v2assocs)


add :: Num a => Vector a -> Vector a -> Vector a
add = op (+)

values :: Num a => Vector a -> [a]
values (Vector v) = elems v
