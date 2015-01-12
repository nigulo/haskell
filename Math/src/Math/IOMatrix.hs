
module Math.IOMatrix (
                IOMatrix, 
                matrix,
                cloneMatrix,
                matrixFromIndicesAndValues,
                nullSquareMatrix, nullMatrix, 
                emptyMatrix,
                set, get, getNumRows, getNumColumns, 
                setAll,
                modifyAll,
                modifyAll_,
                getColumn, setColumn, 
                getRow, setRow, 
                getValues,
                swapRows,
                addColumn,
                transpose, mul, mulVect,
                getDeterminant,
                getSubMatrix
                ) where

import qualified Math.IOVector as V
import Data.Array.IO
import Data.List hiding (transpose)

-- Defines a mathematical m * n matrix in the form [rows] = [[elements]]
newtype Num a => IOMatrix a = IOMatrix (IOArray (Int, Int) a)

matrix :: Num a => [[a]] -> IO (IOMatrix a)
matrix [[]] = 
    do
        array <- newListArray ((0, 0), (0, 0)) []
        return (IOMatrix array)

matrix rows@(r0:rs) =
    do
        let 
            lastRow = length rows - 1
            lastCol = length r0 - 1
        array <- newListArray ((0, 0), (lastRow, lastCol)) [((rows !! i) !! j) | i <- [0 .. lastRow], j <- [0 .. lastCol]]
        return (IOMatrix array)

cloneMatrix :: Num a => IOMatrix a -> IO (IOMatrix a)
cloneMatrix (IOMatrix m) = 
    do
        m1 <- mapArray id m
        return (IOMatrix m1)

matrixFromIndicesAndValues :: Num a => Int -> Int -> [((Int, Int), a)] -> IO (IOMatrix a)
matrixFromIndicesAndValues numRows numCols indicesAndValues = 
    do
        array <- newListArray ((0, 0), (numRows - 1, numCols - 1)) $ snd $ unzip $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) indicesAndValues
        return (IOMatrix array)

-- creates a square null matrix of given size
nullSquareMatrix :: Num a => Int -> IO (IOMatrix a)
nullSquareMatrix size = 
    do
        array <- newListArray ((0, 0), (size - 1, size - 1)) [0 | i <- [0 .. size - 1], j <- [0 .. size - 1]]
        return (IOMatrix array)

-- creates a nullMatrix with m rows and n columns
nullMatrix :: Num a => Int -> Int -> IO (IOMatrix a)
nullMatrix numRows numCols = 
    do
        array <- newListArray ((0, 0), (numRows - 1, numCols - 1)) [0 | i <- [0 .. numRows - 1], j <- [0 .. numCols - 1]]
        return (IOMatrix array)

emptyMatrix :: Num a => Int -> Int -> IO (IOMatrix a)
emptyMatrix numRows numCols = 
    do
        array <- newListArray ((0, 0), (numRows - 1, numCols - 1)) []
        return (IOMatrix array)

-- Sets the matrix element at the given row and column
set :: Num a => (Int, Int) -> a -> IOMatrix a -> IO ()
set (i, j) value (IOMatrix m) = writeArray m (i, j) value 


setAll :: Num a => [((Int, Int), a)] -> IOMatrix a -> IO ()
setAll values (IOMatrix m) = 
    mapM_ (\((i, j), value) -> writeArray m (i, j) value) values

modifyAll :: Num a => (a -> a -> a) -> [((Int, Int), a)] -> IOMatrix a -> IO ()
modifyAll f values (IOMatrix m) = 
    mapM_ (\((i, j), value) -> do oldVal <- readArray m (i, j); writeArray m (i, j) (oldVal `f` value)) values

modifyAll_ :: Num a => (a -> a) -> [(Int, Int)] -> IOMatrix a -> IO ()
modifyAll_ f indices (IOMatrix m) = 
    mapM_ (\(i, j) -> do oldVal <- readArray m (i, j); writeArray m (i, j) (f oldVal)) indices

-- gets the matrix element at the given row and column
get :: Num a => (Int, Int) -> IOMatrix a -> IO a
get i (IOMatrix m) = readArray m i


getNumRows :: Num a => IOMatrix a -> IO Int
getNumRows (IOMatrix m) = 
    do 
        ((l, _), (u, _)) <- getBounds m
        return $ u - l + 1

getNumColumns :: Num a => IOMatrix a -> IO Int
getNumColumns (IOMatrix m) =
    do
        ((_, l), (_, u)) <- getBounds m
        return $ u - l + 1

setRow :: Num a => Int -> [a] -> IOMatrix a -> IO ()
setRow i vals (IOMatrix m) =
    do
        ((_, l), (_, u)) <- getBounds m
        mapM_ (\(j, value) -> writeArray m (i, j) value) $ zip [l .. u] vals

-- returns the given row
getRow :: Num a => Int -> IOMatrix a -> IO [a]
getRow i (IOMatrix m) =
    do
        ((_, l), (_, u)) <- getBounds m
        row <- mapM (\j -> readArray m (i, j)) [l .. u]
        return row

setColumn :: Num a => Int -> [a] -> IOMatrix a -> IO ()
setColumn j vals (IOMatrix m) =
    do
        ((l, _), (u, _)) <- getBounds m
        mapM_ (\(i, value) -> writeArray m (i, j) value) $ zip [l .. u] vals

-- returns the given column
getColumn :: Num a => Int -> IOMatrix a -> IO [a]
getColumn j (IOMatrix m) =
    do
        ((l, _), (u, _)) <- getBounds m
        col <- mapM (\i -> readArray m (i, j)) [l .. u]
        return col

getValues :: Num a => IOMatrix a -> IO [((Int, Int), a)]
getValues (IOMatrix m) = getAssocs m

swapRows :: Num a => Int -> Int -> IOMatrix a -> IO ()
swapRows i j (IOMatrix m) =
    do 
        ((_, l), (_, u)) <- getBounds m
        mapM_ (\col -> do val1 <- readArray m (i, col); val2 <- readArray m (j, col); writeArray m (j, col) val1; writeArray m (i, col) val2) [l .. u]


-- adds a new column to the matrix
addColumn :: Num a => [a] -> IOMatrix a -> IO (IOMatrix a)
addColumn values m = 
    do
        numRows <- getNumRows m
        numCols <- getNumColumns m
        newMatrix <- emptyMatrix numRows (numCols + 1)
        mapM_ (\j -> do col <- getColumn j m; setColumn j col newMatrix) [0 .. numCols - 1]
        setColumn numCols values newMatrix
        return newMatrix

-- transposes the matrix
transpose :: Num a => IOMatrix a -> IO ()
transpose (IOMatrix m) =  
    do
        ((l1, l2), (u1, u2)) <- getBounds m
        mapM_ (\(i, j) -> do val1 <- readArray m (i, j); val2 <- readArray m (j, i); writeArray m (j, i) val1; writeArray m (i, j) val2) [(i, j) | i <- [l1 .. u1], j <- [l2, u2]]

-- multiplies two matrices
mul :: Num a => IOMatrix a -> IOMatrix a -> IO (IOMatrix a)
mul m1 m2 = 
    do
        let
            dotProduct :: Num a => [a] -> [a] -> a
            dotProduct [] _ = 0
            dotProduct _ [] = 0
            dotProduct (r:rs) (c:cs) = (r * c) + dotProduct rs cs
            
            --update :: Int -> Int -> IOMatrix a -> IO ();
            update i j m = 
                do
                    row <- getRow i m1
                    col <- getColumn j m2
                    set (i, j) (dotProduct row col) m
            
            
        numCols <- getNumColumns m2
        numRows <- getNumRows m1
        newMatrix <- nullMatrix numRows numCols
        let
            --calc :: Num a => Int -> Int -> IOMatrix a -> IO ();
            calc i j m = 
                if (i == numRows - 1 && j == numCols - 1) then update i j m
                else if (j == numCols - 1) then
                    do
                        update i j m
                        calc (i + 1) 0 m
                else 
                    do
                        update i j m
                        calc i (j + 1) m
        calc 0 0 newMatrix
        return newMatrix

-- multiplies matrix with vector
mulVect :: Num a => IOMatrix a -> V.IOVector a -> IO (V.IOVector a)
mulVect m v = 
    do
        values <- V.values v
        m1 <- matrix [values]
        transpose m1
        m2 <- m `mul` m1
        col0 <- getColumn 0 m2
        V.vector col0

-- Methods for square matrices only --------------------------------------------

getDeterminant :: Num a => IOMatrix a -> IO a
getDeterminant m = do
    numRows <- getNumRows m 
    numCols <- getNumColumns m
    if numRows == 1 then get (0, 0) m
        else 
            do
                subDets <- mapM (\col -> do a <- get (0, col) m; sub <- getSubMatrix col m; det <- getDeterminant sub; return ((-1) ^ col * a * det)) [0 .. numCols - 1]
                return $ sum subDets
                --foldl (+) 0 [(-1) ^ col * (get (0, col)) m * (getDeterminant (getSubMatrix col m)) | col <- [0 .. getNumCols - 1]]

-- returns sub matrix for the given column
getSubMatrix :: Num a => Int -> IOMatrix a -> IO (IOMatrix a)
getSubMatrix col (IOMatrix m) =
    do 
        ((l1, l2), (u1, u2)) <- getBounds m
        assocs <- getAssocs m
        let
            filteredElements = filter (\((i, j), _) -> j /= col && i > 0) assocs
        m1 <- newListArray ((l1, l2), (u1 - 1, u2 - 1)) $ snd $ unzip $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) filteredElements
        return $ IOMatrix m1
--------------------------------------------------------------------------------
