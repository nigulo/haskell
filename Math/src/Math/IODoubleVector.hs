module Math.IODoubleVector ( 
                    IODoubleVector,
                    vector,
                    cloneVector,
                    nullVector, 
                    get, 
                    set, 
                    setAll,
                    elemOp,
                    op,
                    op3,
                    getLength,
                    values,
                    add
                    ) where

import Debug.Trace
import Data.Array.IO
import Utils.Misc


newtype IODoubleVector = IODoubleVector (IOUArray Int Double)

-- creates a new vector
vector :: [Double] -> IO IODoubleVector
vector list =
    do
        array <- newListArray (0, length list - 1) list
        return (IODoubleVector array)

cloneVector :: IODoubleVector -> IO IODoubleVector
cloneVector (IODoubleVector v) = 
    do
        v1 <- mapArray id v
        return (IODoubleVector v1)

-- creates a null vector of given length
nullVector :: Int -> IO IODoubleVector
nullVector len = vector (take len (repeat 0))

-- returns the element at given index
get :: Int -> IODoubleVector -> IO Double
get i (IODoubleVector v) = readArray v i

-- sets the element at given index
set :: Int -> Double -> IODoubleVector -> IO ()
set i value (IODoubleVector v) = writeArray v i value


setAll :: [(Int, Double)] -> IODoubleVector -> IO ()
setAll values (IODoubleVector v) =
    mapM_ (\(i, value) -> writeArray v i value) values

elemOp :: Double -> (Double -> Double -> Double) -> IODoubleVector -> IO IODoubleVector
elemOp k f v = 
    do
        len <- getLength v
        mapM_ (\i -> elemOpi i k f v) [0 .. len - 1]
        return v

elemOpi :: Int -> Double -> (Double -> Double -> Double) -> IODoubleVector -> IO ()
elemOpi i value f v = 
    do
        curVal <- get i v
        set i (curVal `f` value) v

getLength :: IODoubleVector -> IO Int
getLength (IODoubleVector v) = 
    do
        (l, u) <- getBounds v
        return $ u - l + 1

dotProduct :: IODoubleVector -> IODoubleVector -> IO Double
dotProduct v@(IODoubleVector v1) (IODoubleVector v2) =

    do
        (l1, u1) <- getBounds v1
        (l2, u2) <- getBounds v2
        forMl_ [0 .. min (u1 - l1) (u2 - l2)] 0 $
            \i retVal -> 
                do
                    v1i <- readArray v1 i
                    v2i <- readArray v2 i
                    return $ retVal + v1i * v2i

-- binary operation between two vectors
-- first vector will be changed and returned
op :: (Double -> Double -> Double) -> IODoubleVector -> IODoubleVector -> IO IODoubleVector
op f (IODoubleVector v1) (IODoubleVector v2) =
    do
        (l, u) <- getBounds v1
        forMl_ [0 .. u - l] () $
            \i _ -> 
                do
                    v1i <- readArray v1 i
                    v2i <- readArray v2 i
                    writeArray v1 i (v1i `f` v2i)
        return $ IODoubleVector v1

-- binary operation between two vectors
-- first vector will be changed and returned
op3 :: (Double -> Double -> Double -> Double) -> IODoubleVector -> IODoubleVector -> IODoubleVector -> IO IODoubleVector
op3 f (IODoubleVector v1) (IODoubleVector v2) (IODoubleVector v3) =
    do
        (l, u) <- getBounds v1
        forMl_ [0 .. u - l] () $
            \i _ -> 
                do
                    v1i <- readArray v1 i
                    v2i <- readArray v2 i
                    v3i <- readArray v3 i
                    writeArray v1 i (f v1i v2i v3i)
        return $ IODoubleVector v1

-- | Adds second vector to first and returns the result
add :: IODoubleVector -> IODoubleVector -> IO IODoubleVector
add = op (+)

values :: IODoubleVector -> IO [Double]
values (IODoubleVector v) = getElems v

