-- | Module for solving system of linear equations
module Math.LinearEquations (solveCramer, solveGauss, calcForward, backSubstitution, backSubstitutionIO) where 

import Math.Vector as Vector
import Math.Matrix as Matrix
import Utils.Misc
import Utils.List
import Debug.Trace
import Data.List
import Control.Monad

-- | Solves the system of linear equations using Cramer's rule.
--   This is very slow and imprecise method, use 'solveGauss' instead.
solveCramer :: Fractional a => Matrix a -> Vector a -> Vector a
solveCramer m v = 
    let 
        mainDet = getDeterminant m
        vals = values v
    in vector [x | x <- [(getDeterminant (setColumn col vals m)) / mainDet | col <- [0 .. (length vals) - 1]]]

-- | Solves the system of linear equations using Gauss' method
solveGauss :: (Fractional a, Ord a, Show a) => Matrix a -> Vector a
solveGauss m = 
    let 
        p = getNumRows m - 2
        m1 = f m where
            f = foldr1 (.) [calcForward i | i <- [p, p - 1 .. 0]]
    in backSubstitution m1
                

calcForward :: (Fractional a, Ord a, Show a) => Int -> Matrix a -> Matrix a
calcForward i m = 
    let
        p = getNumRows m - 1
        m1 = Matrix.swapRows iMax i m where
            iMax = maxi + i where
                Just maxi = maxIndex (map (abs) (drop i (Matrix.getColumn i m)))
        a j k = (Matrix.get (j, k) m1)
        aii = 1 / a i i
        val j k = ((j, k), (a j k - (a j i) * aii * (a i k)))
        vals = [val j k | j <- [i + 1 .. p], k <- [i + 1 .. p + 1]]
    in 
        Matrix.setAll vals m1


-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitution :: Fractional a => Matrix a -> Vector a
backSubstitution m = 
    let
        p = getNumRows m - 1
        a i j = (Matrix.get (i, j) m)
        calc bs i = 
            let 
                aii = {-trace ("aii_back=" ++ (show (a i i)))-} a i i
                bi = (a i (p + 1) - (sum [(a i j) * (bs !! (j - i - 1)) | j <- [i + 1 .. p]])) / (aii) 
            in (bi:bs)
    in vector $ foldl' (calc) [] [p, p - 1 .. 0]


-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitutionIO :: Fractional a => Matrix a -> IO (Vector a)
backSubstitutionIO m = 
    do
        let
            p = getNumRows m - 1
            a i j = (Matrix.get (i, j) m)

            calc bs i = 
                do
                    let bi = (a i (p + 1) - (sum [(a i j) * (bs !! (j - i - 1)) | j <- [i + 1 .. p]])) / (a i i) 
                    return (bi:bs)
        retVal <- foldM (calc) [] [p, p - 1 .. 0]
        
        return $ vector retVal

