-- | Module for solving system of linear equations
module Math.LinearEquations (solveCramer, solveGauss, backSubstitution, backSubstitutionIO) where 

import Math.Vector as Vector
import Math.Matrix as Matrix
import Utils.Misc
import Utils.List
import Debug.Trace

-- | Solves the system of linear equations using Cramer's rule.
-- | This is very slow and imprecise method, use 'solveGauss' instead.
solveCramer :: Fractional a => Matrix a -> Vector a -> Vector a
solveCramer m v = 
    let 
        mainDet = getDeterminant m
        vals = values v
    in vector [x | x <- [(getDeterminant (setColumn col vals m)) / mainDet | col <- [0 .. (length vals) - 1]]]
--------------------------------------------------------------------------------            

-- | Solves the system of linear equations using Gauss' method
solveGauss :: (Fractional a, Ord a, Show a) => Matrix a -> Vector a
solveGauss m = 
    let 
        p = getNumRows m - 2
        m1 = f m where
            f = foldr1 (.) [calcForward i | i <- [p, p - 1 .. 0]]
        --m1 = calc 0 m where
        --    calc i = 
        --        if i == p 
        --        then calcForward i
        --        else (calc (i + 1)).(calcForward i)
    in backSubstitution m1
                

calcForward :: (Fractional a, Ord a, Show a) => Int -> Matrix a -> Matrix a
calcForward i m = 
    let
        p = getNumRows m - 1
        ----------------------
        -- with row exchange
        --m1 = Matrix.setRow iMax (Matrix.getRow i m) (Matrix.setRow i (Matrix.getRow iMax m) m) where
        m1 = Matrix.swapRows iMax i m where
            iMax = maxi + i where
                Just maxi = maxIndex (map (abs) (drop i (Matrix.getColumn i m)))
        ----------------------
        -- without row exchange
        --m1 = m
        -----------------------
        a j k = (Matrix.get (j, k) m1)
        --rowi = Matrix.getRow i m1
        --coli = Matrix.getColumn i m1
        aii = {-trace ("aii=" ++ (show (a i i))) $-} 1 / a i i
        --func j k = Matrix.set (j, k) (a j k - (a j i) * aii * (a i k))
        --funcs = [func j k | j <- [i + 1 .. p], k <- [i + 1 .. p + 1]]
        val j k = ((j, k), (a j k - (a j i) * aii * (a i k)))
        vals = [val j k | j <- [i + 1 .. p], k <- [i + 1 .. p + 1]]
    in 
        --(foldr1 (.) funcs) m1
        Matrix.setAll vals m1


-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitution :: Fractional a => Matrix a -> Vector a
backSubstitution m = 
    let
        p = getNumRows m - 1;
        a i j = (Matrix.get (i, j) m);
        calc i _ bs = 
            let 
                aii = {-trace ("aii_back=" ++ (show (a i i)))-} a i i
                bi = (a i (p + 1) - (sum [(a i j) * (bs !! (j - i - 1)) | j <- [i + 1 .. p]])) / (aii) 
            in (bi:bs)
    in vector $ for p (>=0) (\p -> p - 1) () [] calc
--------------------------------------------------------------------------------            

-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitutionIO :: Fractional a => Matrix a -> IO (Vector a)
backSubstitutionIO m = 
    do
        let
            p = getNumRows m - 1;
            a i j = (Matrix.get (i, j) m);

            calc i _ bs = 
                do
                    let bi = (a i (p + 1) - (sum [(a i j) * (bs !! (j - i - 1)) | j <- [i + 1 .. p]])) / (a i i) 
                    return (bi:bs) --(Vector.set i bi bs)
        retVal <- forM p (>=0) (\p -> p - 1) () [] calc
        
        return $ vector retVal

