
module Math.Matrix (
                Matrix, 
                matrix,
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

import qualified Math.Vector as V
import Data.Array
import Debug.Trace

-- Defines a mathematical m * n matrix in the form [rows] = [[elements]]
newtype Num a => Matrix a = Matrix (Array (Int, Int) a) deriving Show

matrix :: Num a => [[a]] -> Matrix a
matrix [[]] = Matrix (array ((0, 0), (0, 0)) [])
matrix rows@(r0:rs) = 
    Matrix (listArray ((0, 0), (length rows - 1, length r0 - 1)) (concat rows))

matrixFromIndicesAndValues :: Num a => Int -> Int -> [((Int, Int), a)] -> Matrix a
matrixFromIndicesAndValues numRows numCols indicesAndValues = 
    Matrix (array ((0, 0), (numRows - 1, numCols - 1)) indicesAndValues)

-- creates a square null matrix of given size
nullSquareMatrix :: Num a => Int -> Matrix a
nullSquareMatrix size = Matrix (array ((0, 0), (size - 1, size - 1)) [((i, j), 0) | i <- [0 .. size - 1], j <- [0 .. size - 1]])

-- creates a nullMatrix with m rows and n columns
nullMatrix :: Num a => Int -> Int -> Matrix a
nullMatrix numRows numCols = Matrix (array ((0, 0), (numRows - 1, numCols - 1)) [((i, j), 0) | i <- [0 .. numRows - 1], j <- [0 .. numCols - 1]])

emptyMatrix :: Num a => Int -> Int -> Matrix a
emptyMatrix numRows numCols = Matrix (array ((0, 0), (numRows - 1, numCols - 1)) [])

-- Sets the matrix element at the given row and column
set :: Num a => (Int, Int) -> a -> Matrix a -> Matrix a
set (i, j) value (Matrix m) = Matrix (m // [((i, j), value)])

setAll :: Num a => [((Int, Int), a)] -> Matrix a -> Matrix a
setAll values (Matrix m) = Matrix (m // values)

modifyAll :: Num a => (a -> a -> a) -> [((Int, Int), a)] -> Matrix a -> Matrix a
modifyAll f values (Matrix m) = Matrix (accum f m values)

-- gets the matrix element at the given row and column
get :: Num a => (Int, Int) -> Matrix a -> a
get i (Matrix m) = m ! i

getNumRows :: Num a => Matrix a -> Int
getNumRows (Matrix m) = 
    let ((l, _), (u, _)) = bounds m
    in u - l + 1

getNumColumns :: Num a => Matrix a -> Int
getNumColumns (Matrix m) =
    let ((_, l), (_, u)) = bounds m
    in u - l + 1

setRow :: Num a => Int -> [a] -> Matrix a -> Matrix a
setRow i vals (Matrix m) =
    Matrix $ m // [((i, j), vals !! j)| j <- [0 .. length vals - 1]]

-- returns the given row
getRow :: Num a => Int -> Matrix a -> [a]
getRow i (Matrix m) =
    map (\(_, val) -> val) $ filter (\((row, _), _) -> row == i) (assocs m)

setColumn :: Num a => Int -> [a] -> Matrix a -> Matrix a
setColumn i vals (Matrix m) =
    Matrix $ m // [((j, i), vals !! j)| j <- [0 .. length vals - 1]]

-- returns the given column
getColumn :: Num a => Int -> Matrix a -> [a]
getColumn i (Matrix m) =
    map (\(_, val) -> val) $ filter (\((_, col), _) -> col == i) (assocs m)


getValues :: Num a => Matrix a -> [((Int, Int), a)]
getValues (Matrix m) = assocs m

swapRows :: Num a => Int -> Int -> Matrix a -> Matrix a
swapRows i j (Matrix m) = 
    let 
        rowi = map (\((_, col), val) -> ((j, col), val)) $ filter (\((row, _), _) -> row == i) (assocs m)
        rowj = map (\((_, col), val) -> ((i, col), val)) $ filter (\((row, _), _) -> row == j) (assocs m)
    in Matrix (m // (rowi ++ rowj))


-- adds a new column to the matrix
addColumn :: Num a => [a] -> Matrix a -> Matrix a
addColumn values m = 
    let
        numCols = getNumColumns m
        updateColumn i m1 = 
            if (i == numCols) then setColumn i values m1
            else updateColumn (i + 1) (setColumn i (getColumn i m) m1)
     in updateColumn 0 (nullMatrix (getNumRows m) (numCols + 1))

-- transposes the matrix
transpose :: Num a => Matrix a -> Matrix a
transpose (Matrix m) = 
    let 
        ((l1, l2), (u1, u2)) = bounds m
    in Matrix (array ((l2, l1), (u2, u1)) [((j, i), m ! (i, j)) | i <- [l1 .. u1], j <- [l2 .. u2]])

-- multiplies two matrices
mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul m1 m2 = 
    let 
        numCols = getNumColumns m2;
        numRows = getNumRows m1;
        
        dotProduct :: Num a => [a] -> [a] -> a;
        dotProduct [] _ = 0;
        dotProduct _ [] = 0;
        dotProduct (r:rs) (c:cs) = (r * c) + dotProduct rs cs;
        
        update i j = set (i, j) (dotProduct (getRow i m1) (getColumn j m2));
        
        calc i j m = 
            if (i == numRows - 1 && j == numCols - 1) then update i j m
            else if (j == numCols - 1) then calc (i + 1) 0 (update i j m)
            else calc i (j + 1) (update i j m)
    in calc 0 0 (nullMatrix numRows numCols)

-- multiplies matrix with vector
mulVect :: Num a => Matrix a -> V.Vector a -> V.Vector a
mulVect m v = V.vector (getColumn 0 (m `mul` (transpose (matrix [V.values v]))))

-- Methods for square matrices only --------------------------------------------

getDeterminant :: Num a => Matrix a -> a
getDeterminant m = 
    if getNumRows m == 1 then get (0, 0) m
    else foldl (+) 0 [(-1) ^ col * (get (0, col)) m * (getDeterminant (getSubMatrix col m)) | col <- [0 .. (getNumColumns m) - 1]]

-- returns sub matrix for the given column
getSubMatrix :: Num a => Int -> Matrix a -> Matrix a
getSubMatrix col (Matrix m) = 
    let ((l1, l2), (u1, u2)) = bounds m
    in Matrix (array ((l1, l2), (u1 - 1, u2 - 1)) (filter (\((i, j), _) -> j /= col && i > 0) (assocs m)))
--------------------------------------------------------------------------------

