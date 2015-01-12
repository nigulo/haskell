-- | Module for obtaining least squares estimate for given measurements
--   using Gentleman's and Stirling's procedures (see paragraphs 6.6 - 6.10)
module Regression.LeastSquares (
    LSQState, 
    initialize, 
    addMeasurement, 
    invert, 
    addConstraint, 
    solve,
    solveIO) where

import Utils.Misc
import Math.Matrix as M
import Math.Vector as V
import Math.LinearEquations (backSubstitution, backSubstitutionIO)

import Debug.Trace

newtype LSQState = LSQState (
    Matrix Double, -- ^ Unit upper trianguilar matrix
    Vector Double, -- ^ d or f depending on if Gentleman's or 
                   -- ^ Stirlings procedure is used
    Double)        -- ^ e
        deriving Show

-- | Returns the initial state for weighted least square computation
initialize :: Int -- ^ Number of x variables
           -> LSQState
initialize p = 
    let (b, d) = for__ 0 (p - 1) (nullMatrix p (p + 1), nullVector (p + 1)) f where
    --let (b, d) = forl_ [0 .. (p - 1)] (nullMatrix p (p + 1), nullVector (p + 1)) f where
        f j (b, d) = 
            let (b1, d1) = (M.set (j, j) 1 b, V.set j 0 d);
            --in (for__ (j + 1) (p - 1) b1 (\k b -> M.set (j, k) 0 b), d1)
            in (forl_ [(j + 1) .. (p - 1)] b1 (\k b -> M.set (j, k) 0 b), d1)
    in LSQState (b, d, 0)

-- | Adds a next row of measurements to the computation. If you have
--   finished adding measurements, call 'invert' and start adding constraints 
--   using 'addConstraint'. In case there are no constraints to impose
--   immediately call solve to obtain the LSQ estimate.
addMeasurement ::
          Vector Double -- ^ array of x values
       -> Double        -- ^ y value
       -> Double        -- ^ weight of the measurement (0 = don't use)
       -> LSQState      -- previous state of calculation
       -> LSQState      -- next state of calculation
addMeasurement _ _ 0 state = state -- return same state if weight is zero
addMeasurement xVect y w (LSQState (b, d, e)) = 
    let 
        p = getLength xVect
        fori i (b, x, d) = 
            let h = V.get i x;
                dm = (V.get p d);
            in
                if h == 0 || dm == 0 then (b, x, d)
                else 
                    let
                        di = (V.get i d);
                        d' =  di + dm * h * h;
                        c' = di / d';
                        s' = dm * h / d';
                        d1 = ((V.set p (c' * dm)).(V.set i d')) d;
                        
                        --------------------------------------------------------
                        f k = M.get (i, k)
                        g k = V.get k 
                        
                        
                        b1 = M.setAll [((i, k), (c' * (f k b) + s' * (g k x))) | k <- [i + 1 .. p]] b
                        x1 = V.setAll [(k, (g k x - h * (f k b))) | k <- [i + 1 .. p]] x
                        --------------------------------------------------------
                     in (b1, x1, d1)
        --(b1, x1, d1) = f (b, vector (values xVect ++ [y]), (V.set p w d)) where
        --    f = foldr1 (.) [fori i | i <- [p - 1, p - 2 .. 0]]
        --(b1, x1, d1) = foldl (\x f -> f x) (b, Vector (x ++ [y]), (V.set p w d)) [fori i | i <- [0 .. (p - 1)]]
        --(b1, x1, d1) = forl_ [0 .. (p - 1)] (b, Vector (x ++ [y]), (V.set p w d)) fori;
        (b1, x1, d1) = for__ 0 (p - 1) (b, vector (values xVect ++ [y]), (V.set p w d)) fori;
        xyp = (V.get p x1);
    in LSQState (b1, d1, e + (V.get p d1) * xyp * xyp)

invert :: LSQState      -- previous state of calculation
       -> LSQState      -- next state of calculation
invert (LSQState (b, d, e)) = LSQState (b, f, e) where
    f = vector [1 / V.get i d | i <- [0 .. V.getLength d - 1]]

-- | Adds a next row of constraints to the computation. If you have
--   finished adding constraints, call 'solve' to obtain the LSQ estimate.
addConstraint ::
          Vector Double -- ^ array of r values
       -> Double        -- ^ s value
       -> Double        -- ^ weight of the constraint (0 = infinite)
       -> LSQState      -- previous state of calculation
       -> LSQState      -- next state of calculation
addConstraint rVect s w (LSQState (b, f, e)) = 
    let
        p = getLength rVect
        fori i (b, r, f) = 
            let h = V.get i r;
            in
                if (V.get i f) == 0 && h /= 0 then 
                    let 
                        fork k r = V.set k ((V.get k r) - h * (M.get (i, k) b)) r
                        --func = foldr1 (.) [fork k | k <- [p, p - 1 .. i + 1]]
                    in 
                        --(b, func r, f)
                            
                        --(b, foldl (\x f -> f x) r [fork k | k <- [(i + 1) .. p]], f)
                        --(b, forl_ [(i + 1) .. p] r fork, f)
                        (b, for__ (i + 1) p r fork, f)
                else if h /= 0 then 
                    let 
                        fm = (V.get p f);
                        fi = (V.get i f);
                        f' =  fm + fi * h * h;
                        c' = fm / f';
                        s' = fi * h / f';
                        f1 = ((V.set p f').(V.set i (c' * fi))) f;
                        fork k (b, r) = 
                            let
                                g1 = M.get (i, k) b;
                                g2 = V.get k r
                            in
                                (M.set (i, k) (c' * g1 + s' * g2) b, V.set k (g2 - h * g1) r);
                        --(b1, r1) = func (b, r) where
                        --    func = foldr1 (.) [fork k | k <- [p, p - 1 .. i + 1]]
                        --(b1, r1) = foldl (\x f -> f x) (b, r) [fork k | k <- [(i + 1) .. p]]
                        --(b1, r1) = forl_ [(i + 1) .. p] (b, r) fork
                        (b1, r1) = for__ (i + 1) p (b, r) fork
                     in (b1, r1, f1)
                else (b, r, f);
        --(b1, r1, f1) = func (b, vector (values rVect ++ [s]), (V.set p w f)) where
        --    func = foldr1 (.) [fori i | i <- [p - 1, p - 2 .. 0]]
        --(b1, r1, f1) = foldl (\x f -> f x) (b, Vector (r ++ [s]), (V.set p w f)) [fori i | i <- [0 .. (p - 1)]]
        --(b1, r1, f1) = forl_ [0 .. (p - 1)] (b, Vector (r ++ [s]), (V.set p w f)) fori;
        (b1, r1, f1) = for__ 0 (p - 1) (b, vector (values rVect ++ [s]), (V.set p w f)) fori;
        rsp = (V.get p r1);
        fm = V.get p f;
        e1 = if fm /= 0 then (e + rsp * rsp / fm) else e
    in LSQState (b1, f1, e1)

solve :: LSQState -> Vector Double
solve (LSQState (b, d, e)) = backSubstitution b

solveIO :: LSQState -> IO (Vector Double)
solveIO (LSQState (b, d, e)) = backSubstitutionIO b
