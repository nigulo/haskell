-- | Module for solving system of linear equations
module Math.IODoubleLinearEquations (solveGauss, backSubstitution) where 

import Math.IODoubleVector as V
import Math.IODoubleMatrix as M
import Utils.List

-- | Solves the system of linear equations using Gauss' method
solveGauss :: IODoubleMatrix -> IO IODoubleVector
solveGauss m = 
    do
        numRows <- getNumRows m
        let p = numRows - 2
        mapM_ (\i -> calcForward i m) [0 .. p]
        backSubstitution m
                

calcForward :: Int -> IODoubleMatrix -> IO ()
calcForward i m = 
    do
        numRows <- getNumRows m
        let p = numRows - 1
        col <- M.getColumn i m
        ----------------------
        -- with row exchange
        let iMax = maxi + i where
            Just maxi = maxIndex (map (abs) (drop i col))
        M.swapRows iMax i m
        ----------------------
        -- without row exchange
        --m1 = m
        -----------------------
        let
            a j k = M.get (j, k) m
        aii_ <- a i i
        let 
            aii = {-trace ("aii=" ++ (show (aii_))) $-} 1 / aii_
            val j k =
                do
                    ajk <- a j k
                    aji <- a j i
                    aik <- a i k 
                    return ((j, k), (ajk - (aji) * aii * (aik)))
        
        mapM_ (\(j, k) -> do (indices, value) <- val j k; M.set indices value m) [(j , k) | j <- [i + 1 .. p], k <- [i + 1 .. p + 1]]

-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitution :: IODoubleMatrix -> IO IODoubleVector
backSubstitution m = 
    do
        numRows <- getNumRows m
        retVal <- nullVector numRows
        let
            p = numRows - 1
            a i j = (M.get (i, j) m)

            calc i = 
                do
                    aipp1 <- a i (p + 1)
                    aii <- a i i
                    toSum <- mapM (\j -> do aij <- a i j; bsj <- V.get j retVal; return (aij * bsj)) [i + 1 .. p]
                    let bi = (aipp1 - (sum toSum)) / aii
                    V.set i bi retVal
        mapM_ calc [p, p - 1 .. 0]
        return retVal

