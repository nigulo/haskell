module Math.IOVector ( 
                    IOVector,
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
import Control.Monad


newtype Num a => IOVector a = IOVector (IOArray Int a)

-- creates a new vector
vector :: Num a => [a] -> IO (IOVector a)
vector list =
    do
        array <- newListArray (0, length list - 1) list
        return (IOVector array)

cloneVector :: Num a => IOVector a -> IO (IOVector a)
cloneVector (IOVector v) = 
    do
        v1 <- mapArray id v
        return (IOVector v1)

-- creates a null vector of given length
nullVector :: Num a => Int -> IO (IOVector a)
nullVector len = vector (take len (repeat 0))

-- returns the element at given index
get :: Num a => Int -> IOVector a -> IO a
get i (IOVector v) = readArray v i

-- sets the element at given index
set :: Num a => Int -> a -> IOVector a -> IO ()
set i value (IOVector v) = writeArray v i value


setAll :: Num a => [(Int, a)] -> IOVector a -> IO ()
setAll values (IOVector v) =
    mapM_ (\(i, value) -> writeArray v i value) values

elemOp :: Num a => a -> (a -> a -> a) -> IOVector a -> IO (IOVector a)
elemOp k f v = 
    do
        len <- getLength v
        mapM_ (\i -> elemOpi i k f v) [0 .. len - 1]
        return v

elemOpi :: Num a => Int -> a -> (a -> a -> a) -> IOVector a -> IO ()
elemOpi i value f v = 
    do
        curVal <- get i v
        set i (curVal `f` value) v

getLength :: Num a => IOVector a -> IO Int
getLength (IOVector v) = 
    do
        (l, u) <- getBounds v
        return $ u - l + 1

dotProduct :: Num a => IOVector a -> IOVector a -> IO a
dotProduct v@(IOVector v1) (IOVector v2) =

    do
        (l1, u1) <- getBounds v1
        (l2, u2) <- getBounds v2
        let
            func retVal i = 
                do
                    v1i <- readArray v1 i
                    v2i <- readArray v2 i
                    return $ retVal + v1i * v2i
        foldM (func) 0 [0 .. min (u1 - l1) (u2 - l2)] 

-- binary operation between two vectors
-- first vector will be changed and returned
op :: (Num a, Num b) => (a -> b -> a) -> IOVector a -> IOVector b -> IO (IOVector a)
op f (IOVector v1) (IOVector v2) =
    do
        (l, u) <- getBounds v1
        let
            func _ i =
                do
                    v1i <- readArray v1 i
                    v2i <- readArray v2 i
                    writeArray v1 i (v1i `f` v2i)
        foldM_ (func) () [0 .. u - l]
        return $ IOVector v1

-- binary operation between two vectors
-- first vector will be changed and returned
op3 :: (Num a, Num b, Num c) => (a -> b -> c -> a) -> IOVector a -> IOVector b -> IOVector c -> IO (IOVector a)
op3 f (IOVector v1) (IOVector v2) (IOVector v3) =
    do
        (l, u) <- getBounds v1
        let
            func _ i =
                do
                    v1i <- readArray v1 i
                    v2i <- readArray v2 i
                    v3i <- readArray v3 i
                    writeArray v1 i (f v1i v2i v3i)
        foldM_ (func) () [0 .. u - l]
        return $ IOVector v1

-- | Adds second vector to first and returns the result
add :: Num a => IOVector a -> IOVector a -> IO (IOVector a)
add = op (+)

values :: Num a => IOVector a -> IO [a]
values (IOVector v) = getElems v

