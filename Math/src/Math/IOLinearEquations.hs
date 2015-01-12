
-- | Module for solving system of linear equations
module Math.IOLinearEquations (solveGauss, backSubstitution) where 

import Math.IOVector as IOVector
import Math.IOMatrix as IOMatrix
import Utils.List

-- | Solves the system of linear equations using Gauss' method
solveGauss :: (Fractional a, Ord a, Show a) => IOMatrix a -> IO (IOVector a)
solveGauss m = 
    do
        numRows <- getNumRows m
        let p = numRows - 2
        mapM_ (\i -> calcForward i m) [0 .. p]
        backSubstitution m
                

calcForward :: (Fractional a, Ord a, Show a) => Int -> IOMatrix a -> IO ()
calcForward i m = 
    do
        numRows <- getNumRows m
        let p = numRows - 1
        col <- IOMatrix.getColumn i m
        ----------------------
        -- with row exchange
        let iMax = maxi + i where
            Just maxi = maxIndex (map (abs) (drop i col))
        IOMatrix.swapRows iMax i m
        ----------------------
        -- without row exchange
        --m1 = m
        -----------------------
        let
            a j k = IOMatrix.get (j, k) m
        aii_ <- a i i
        let 
            aii = {-trace ("aii=" ++ (show (aii_))) $-} 1 / aii_
            val j k =
                do
                    ajk <- a j k
                    aji <- a j i
                    aik <- a i k 
                    return ((j, k), (ajk - (aji) * aii * (aik)))
        
        mapM_ (\(j, k) -> do (indices, value) <- val j k; IOMatrix.set indices value m) [(j , k) | j <- [i + 1 .. p], k <- [i + 1 .. p + 1]]

-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitution :: Fractional a => IOMatrix a -> IO (IOVector a)
backSubstitution m = 
    do
        numRows <- getNumRows m
        retVal <- nullVector numRows
        let
            p = numRows - 1
            a i j = (IOMatrix.get (i, j) m)

            calc i = 
                do
                    aipp1 <- a i (p + 1)
                    aii <- a i i
                    toSum <- mapM (\j -> do aij <- a i j; bsj <- IOVector.get j retVal; return (aij * bsj)) [i + 1 .. p]
                    let bi = (aipp1 - (sum toSum)) / aii
                    IOVector.set i bi retVal
        mapM_ calc [p, p - 1 .. 0]
        return retVal

