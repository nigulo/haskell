
module Math.IODoubleMatrix (
                IODoubleMatrix, 
                matrix,
                cloneMatrix,
                matrixFromIndicesAndValues,
                nullSquareMatrix, nullMatrix, 
                emptyMatrix,
                set, get, getNumRows, getNumColumns, 
                setAll,
                modifyAll,
                getColumn, setColumn, 
                getRow, setRow, 
                getValues,
                swapRows,
                addColumn,
                transpose, mul, mulVect,
                getDeterminant,
                getSubMatrix
                ) where

import qualified Math.IODoubleVector as V
import Data.Array.IO
import Data.List hiding (transpose)

-- Defines a mathematical m * n matrix in the form [rows] = [[elements]]
newtype IODoubleMatrix = IODoubleMatrix (IOUArray (Int, Int) Double)

matrix :: [[Double]] -> IO IODoubleMatrix
matrix [[]] = 
    do
        array <- newListArray ((0, 0), (0, 0)) []
        return (IODoubleMatrix array)

matrix rows@(r0:_) =
    do
        let 
            lastRow = length rows - 1
            lastCol = length r0 - 1
        array <- newListArray ((0, 0), (lastRow, lastCol)) (concat rows)
        return (IODoubleMatrix array)

cloneMatrix :: IODoubleMatrix -> IO IODoubleMatrix
cloneMatrix (IODoubleMatrix m) = 
    do
        m1 <- mapArray id m
        return (IODoubleMatrix m1)

matrixFromIndicesAndValues :: Int -> Int -> [((Int, Int), Double)] -> IO IODoubleMatrix
matrixFromIndicesAndValues numRows numCols indicesAndValues = 
    do
        array <- newListArray ((0, 0), (numRows - 1, numCols - 1)) $ snd $ unzip $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) indicesAndValues
        return (IODoubleMatrix array)

-- creates a square null matrix of given size
nullSquareMatrix :: Int -> IO IODoubleMatrix
nullSquareMatrix size = 
    do
        array <- newListArray ((0, 0), (size - 1, size - 1)) [0 | i <- [0 .. size - 1], j <- [0 .. size - 1]]
        return (IODoubleMatrix array)

-- creates a nullMatrix with m rows and n columns
nullMatrix :: Int -> Int -> IO IODoubleMatrix
nullMatrix numRows numCols = 
    do
        array <- newListArray ((0, 0), (numRows - 1, numCols - 1)) [0 | i <- [0 .. numRows - 1], j <- [0 .. numCols - 1]]
        return (IODoubleMatrix array)

emptyMatrix :: Int -> Int -> IO IODoubleMatrix
emptyMatrix numRows numCols = 
    do
        array <- newListArray ((0, 0), (numRows - 1, numCols - 1)) []
        return (IODoubleMatrix array)

-- Sets the matrix element at the given row and column
set :: (Int, Int) -> Double -> IODoubleMatrix -> IO ()
set (i, j) value (IODoubleMatrix m) = writeArray m (i, j) value 


setAll :: [((Int, Int), Double)] -> IODoubleMatrix -> IO ()
setAll values (IODoubleMatrix m) = 
    mapM_ (\((i, j), value) -> writeArray m (i, j) value) values

modifyAll :: (Double -> Double) -> [(Int, Int)] -> IODoubleMatrix -> IO ()
modifyAll f indices (IODoubleMatrix m) = 
    mapM_ (\(i, j) -> do oldVal <- readArray m (i, j); writeArray m (i, j) (f oldVal)) indices

-- gets the matrix element at the given row and column
get :: (Int, Int) -> IODoubleMatrix -> IO Double
get i (IODoubleMatrix m) = readArray m i


getNumRows :: IODoubleMatrix -> IO Int
getNumRows (IODoubleMatrix m) = 
    do 
        ((l, _), (u, _)) <- getBounds m
        return $ u - l + 1

getNumColumns :: IODoubleMatrix -> IO Int
getNumColumns (IODoubleMatrix m) =
    do
        ((_, l), (_, u)) <- getBounds m
        return $ u - l + 1

setRow :: Int -> [Double] -> IODoubleMatrix -> IO ()
setRow i vals (IODoubleMatrix m) =
    do
        ((_, l), (_, u)) <- getBounds m
        mapM_ (\(j, value) -> writeArray m (i, j) value) $ zip [l .. u] vals

-- returns the given row
getRow :: Int -> IODoubleMatrix -> IO [Double]
getRow i (IODoubleMatrix m) =
    do
        ((_, l), (_, u)) <- getBounds m
        row <- mapM (\j -> readArray m (i, j)) [l .. u]
        return row

setColumn :: Int -> [Double] -> IODoubleMatrix -> IO ()
setColumn j vals (IODoubleMatrix m) =
    do
        ((l, _), (u, _)) <- getBounds m
        mapM_ (\(i, value) -> writeArray m (i, j) value) $ zip [l .. u] vals

-- returns the given column
getColumn :: Int -> IODoubleMatrix -> IO [Double]
getColumn j (IODoubleMatrix m) =
    do
        ((l, _), (u, _)) <- getBounds m
        col <- mapM (\i -> readArray m (i, j)) [l .. u]
        return col

getValues :: IODoubleMatrix -> IO [((Int, Int), Double)]
getValues (IODoubleMatrix m) = getAssocs m

swapRows :: Int -> Int -> IODoubleMatrix -> IO ()
swapRows i j (IODoubleMatrix m) =
    do 
        ((_, l), (_, u)) <- getBounds m
        mapM_ (\col -> do val1 <- readArray m (i, col); val2 <- readArray m (j, col); writeArray m (j, col) val1; writeArray m (i, col) val2) [l .. u]


-- adds a new column to the matrix
addColumn :: [Double] -> IODoubleMatrix -> IO IODoubleMatrix
addColumn values m = 
    do
        numRows <- getNumRows m
        numCols <- getNumColumns m
        newMatrix <- emptyMatrix numRows (numCols + 1)
        mapM_ (\j -> do col <- getColumn j m; setColumn j col newMatrix) [0 .. numCols - 1]
        setColumn numCols values newMatrix
        return newMatrix

-- transposes the matrix
transpose :: IODoubleMatrix -> IO ()
transpose (IODoubleMatrix m) =  
    do
        ((l1, l2), (u1, u2)) <- getBounds m
        mapM_ (\(i, j) -> do val1 <- readArray m (i, j); val2 <- readArray m (j, i); writeArray m (j, i) val1; writeArray m (i, j) val2) [(i, j) | i <- [l1 .. u1], j <- [l2, u2]]

-- multiplies two matrices
mul :: IODoubleMatrix -> IODoubleMatrix -> IO IODoubleMatrix
mul m1 m2 = 
    do
        let
            dotProduct :: [Double] -> [Double] -> Double
            dotProduct [] _ = 0
            dotProduct _ [] = 0
            dotProduct (r:rs) (c:cs) = (r * c) + dotProduct rs cs
            
            --update :: Int -> Int -> IODoubleMatrix -> IO ();
            update i j m = 
                do
                    row <- getRow i m1
                    col <- getColumn j m2
                    set (i, j) (dotProduct row col) m
            
            
        numCols <- getNumColumns m2
        numRows <- getNumRows m1
        newMatrix <- nullMatrix numRows numCols
        let
            --calc :: Int -> Int -> IODoubleMatrix -> IO ();
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
mulVect :: IODoubleMatrix -> V.IODoubleVector -> IO (V.IODoubleVector)
mulVect m v = 
    do
        values <- V.values v
        m1 <- matrix [values]
        transpose m1
        m2 <- m `mul` m1
        col0 <- getColumn 0 m2
        V.vector col0

-- Methods for square matrices only --------------------------------------------

getDeterminant :: IODoubleMatrix -> IO Double
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
getSubMatrix :: Int -> IODoubleMatrix -> IO IODoubleMatrix
getSubMatrix col (IODoubleMatrix m) =
    do 
        ((l1, l2), (u1, u2)) <- getBounds m
        assocs <- getAssocs m
        let
            filteredElements = filter (\((i, j), _) -> j /= col && i > 0) assocs
        m1 <- newListArray ((l1, l2), (u1 - 1, u2 - 1)) $ snd $ unzip $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) filteredElements
        return $ IODoubleMatrix m1
--------------------------------------------------------------------------------
