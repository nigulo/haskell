module Math.CUDALinearEquations (backSubstitution) where

import Data.Array.Accelerate as A
--import Data.Array.Accelerate.CUDA

type Matrix = Array DIM2 Double

-- | Returns a solution vector for given system of linear equations
--   represented by (n * (n + 1)) upper triangular matrix.
backSubstitution :: Math.CUDALinearEquations.Matrix -> Vector Double
backSubstitution m = 
    let
        Z:.rows:._ = arrayShape m
        p = rows - 1
        a i j = m `indexArray` (Z:.i:.j)
        calc (-1) bs = bs 
        calc i bs = 
            let 
                aii = a i i
                bi = (a i (p + 1) - (Prelude.sum [(a i j) * (bs Prelude.!! (j - i - 1)) | j <- [i + 1 .. p]])) / (aii) 
            in 
                calc (i - 1) (bi:bs)
    in 
        fromList (Z:.rows) (calc p [])
