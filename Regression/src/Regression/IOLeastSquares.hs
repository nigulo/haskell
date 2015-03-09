
-- | Module for obtaining least squares estimate for given measurements
--   using Gentleman's and Stirling's procedures (see paragraphs 6.6 - 6.10)
module Regression.IOLeastSquares (
    IOLSQState (..), 
    initialize, 
    addMeasurement, 
    invert, 
    addConstraint, 
    solve) where

import Math.IODoubleMatrix as M
import Math.IODoubleVector as V
import Math.IODoubleLinearEquations (backSubstitution)

import Debug.Trace

newtype IOLSQState = IOLSQState (
    IODoubleMatrix, -- ^ Unit upper trianguilar matrix
    IODoubleVector, -- ^ d or f depending on if Gentleman's or 
                   -- ^ Stirlings procedure is used
    Double)        -- ^ e


-- | Returns the initial state for weighted least square computation
initialize :: Int -- ^ Number of x variables
           -> IO IOLSQState
initialize p = 
    do
        b <- nullMatrix p (p + 1)
        d <- nullVector (p + 1)
        mapM_ (\j -> do M.set (j, j) 1 b; V.set j 0 d; mapM_ (\k -> M.set (j, k) 0 b) [j + 1 .. p - 1]) [0 .. p - 1]
        return $ IOLSQState (b, d, 0)

-- | Adds a next row of measurements to the computation. If you have
--   finished adding measurements, call 'invert' and start adding constraints 
--   using 'addConstraint'. In case there are no constraints to impose
--   immediately call solve to obtain the LSQ estimate.
addMeasurement ::
          IODoubleVector -- ^ first p elements is an array of x values, last value is y value (NB! will be modified)
       -> Double        -- ^ weight of the measurement (0 = don't use)
       -> IOLSQState      -- previous state of calculation
       -> IO (IOLSQState)      -- next state of calculation
addMeasurement _ 0 state = return state 
addMeasurement x w (IOLSQState (b, d, e)) =
    do 
        p <- getLength x >>= \len -> return (len - 1)
        pCheck <- M.getNumRows b
        if (p /= pCheck) then putStrLn "addMeasurement: illegal input vector" else return ()
        let 
            fori i =
                do
                    h <- V.get i x 
                    dm <- V.get p d
                    if h == 0 || dm == 0 
                        then return ()
                        else 
                            do
                                di <- V.get i d
                                let
                                    d' =  di + dm * h * h
                                    c' = di / d'
                                    s' = dm * h / d'
                                V.set i d' d
                                V.set p (c' * dm) d
                                
                                --------------------------------------------------------
                                let
                                    f k = M.get (i, k)
                                    g = V.get
    
                                mapM_ (\k -> do 
                                    fkb <- f k b
                                    gkx <- g k x
                                    M.set (i, k) (c' * (fkb) + s' * (gkx)) b
                                    V.set k (gkx - h * (fkb)) x
                                    ) [i + 1 .. p]
                                --------------------------------------------------------
        V.set p w d
        mapM_ fori [0 .. p - 1]
        xyp <- V.get p x
        coef <- V.get p d
        return $ IOLSQState (b, d, e + coef * xyp * xyp)


invert :: IOLSQState      -- previous state of calculation
       -> IO ()      -- next state of calculation
invert (IOLSQState (_, d, _)) = 
    do
        len <- V.getLength d
        mapM_ (\i -> do 
            val <- V.get i d
            V.set i (1 / val) d
            ) [0 .. len - 1]


-- | Adds a next row of constraints to the computation. If you have
--   finished adding constraints, call 'solve' to obtain the LSQ estimate.
addConstraint ::
          IODoubleVector -- ^ first p elements is an array of r values, last value is s value (NB! will be modified)
       -> Double        -- ^ weight of the constraint (0 = infinite)
       -> IOLSQState      -- previous state of calculation
       -> IO (IOLSQState)      -- next state of calculation
addConstraint r w (IOLSQState (b, f, e)) = 
    do 
        p <- getLength r >>= \len -> return (len - 1)
        pCheck <- M.getNumRows b
        if (p /= pCheck) then putStrLn "addConstraint: illegal input vector" else return ()
        let
            fori i =
                do 
                    h <- V.get i r
                    if_ <- V.get i f
                    if if_ == 0 && h /= 0 
                        then
                            mapM_ (\k -> do 
                                val1 <- V.get k r
                                val2 <- M.get (i, k) b
                                V.set k (val1 - h * val2) r
                                ) [i + 1 .. p]
                        else if h /= 0 then
                            do 
                                fm <- V.get p f
                                fi <- V.get i f
                                let
                                    f' =  fm + fi * h * h
                                    c' = fm / f'
                                    s' = fi * h / f'
                                V.set i (c' * fi) f
                                V.set p f' f
                                mapM_ (\k -> do 
                                    g1 <- M.get (i, k) b
                                    g2 <- V.get k r
                                    M.set (i, k) (c' * g1 + s' * g2) b
                                    V.set k (g2 - h * g1) r
                                    ) [i + 1 .. p]
                        else return ()
        V.set p w f
        mapM_ fori [0 .. p - 1]
        rsp <- V.get p r
        fm <- V.get p f
        let
            e1 = if fm /= 0 then (e + rsp * rsp / fm) else e
        return $ IOLSQState (b, f, e1)

solve :: IOLSQState -> IO IODoubleVector
solve (IOLSQState (b, _, _)) = backSubstitution b


