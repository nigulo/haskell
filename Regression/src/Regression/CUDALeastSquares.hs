module Regression.CUDALeastSquares (
    LSQState, 
    initialize, 
    addMeasurements, 
    invert, 
    addConstraint, 
    solve
    ) where

import Utils.Misc
import Math.CUDALinearEquations (backSubstitution)
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
--import Data.Array.Accelerate.CUDA

import Debug.Trace
type Matrix = Array DIM2 Double

newtype LSQState = LSQState (
    Matrix, -- ^ Unit upper trianguilar matrix
    Vector Double, -- ^ d or f depending on if Gentleman's or 
                   -- ^ Stirlings procedure is used
    Double)        -- ^ e
        deriving Show

-- | Returns the initial state for weighted least square computation
initialize :: Int -- ^ Number of x variables
           -> LSQState
initialize p = LSQState (fromList (Z:.p:.(p + 1)) [if i == j then 1 else 0 | i <- [0 .. p - 1], j <- [0 .. p]], fromList (Z:.(p + 1)) (repeat 0), 0) 



-- | Adds a next row of measurements to the computation. If you have
--   finished adding measurements, call 'invert' and start adding constraints 
--   using 'addConstraint'. In case there are no constraints to impose
--   immediately call solve to obtain the LSQ estimate.
addMeasurements ::
          [[Double]] -- ^ array of x values
       -> [Double]        -- ^ y value
       -> [Double]        -- ^ weight of the measurement (0 = don't use)
       -> LSQState      -- previous state of calculation
       -> LSQState      -- next state of calculation
--addMeasurements _ _ 0 state = state -- return same state if weight is zero
addMeasurements xVect y w (LSQState (b, d, e)) = 
    let 
        n = Prelude.length xVect
        pInt = Prelude.length (head xVect) 
        --pInt = length xVect
        fori :: Acc (Scalar Int) -> Acc (Scalar Int) -> Acc Matrix -> Acc Matrix
        fori pScalar iScalar bdx = 
            let 
                p = the pScalar
                i = the iScalar
                ------------------------------
                --       1 . . . p + 1
                --       |b b b b b b| 1
                --       |b b b b b b| .
                --       |b b b b b b| .
                -- bxd = |b b b b b b| .
                --       |b b b b b b| p
                --       |d d d d d w| p + 1
                --       |x x x x x y| p + 2
                ------------------------------
                b i j = bdx ! (index2 i j)
                d i = bdx ! (index2 p i)
                x i = bdx ! (index2 (p + 1) i)
                ------------------------------
                h = x i
                dm = d p
                di = d i
                d' =  di + dm * h * h
                c' = di / d'
                s' = dm * h / d'
                
                bdx1 = generate (shape bdx) genBDX1 where
                    genBDX1 :: Exp DIM2 -> Exp Double
                    genBDX1 ix = 
                        let
                            (Z :. i1 :. k1) = unlift ix
                            fkb = b i k1
                            fkb1 = b i1 k1
                            gkx = x k1
                        in
                            i1 ==* p + 1 A.? ( -- x1
                                k1 <=* i ||* k1 >* p A.? (gkx,
                                    gkx - h * fkb),
                                i1 ==* p A.? ( -- d1
                                    k1 ==* i A.? (d', 
                                        k1 ==* p A.? (c' * dm, 
                                            d k1)),
                                    i1 /=* i ||* k1 <=* i ||* k1 >* p A.? (fkb1, -- b1
                                        c' * fkb1 + s' * gkx)))
            in
                h ==* 0 ||* dm ==* 0 A.?| (bdx, bdx1)
        
        foris :: [Acc Matrix -> Acc Matrix]                  
        foris = [fori (unit (constant pInt)) (unit (constant i)) | i <- [0 .. pInt - 1]]
        
        calc :: Acc (Vector Double) -> Acc (Scalar Double) -> Acc (Scalar Int) -> Acc Matrix -> Acc Matrix
        calc xy w pScalar bds = 
            let
                p = the pScalar
                bdx = generate (constant (Z:. pInt + 2 :. pInt + 1)) genBDX1 where
                    genBDX1 :: Exp DIM2 -> Exp Double
                    genBDX1 ix = 
                        let 
                            (Z :. i :. j) = unlift ix 
                        in 
                            i ==* p + 1 A.? (xy A.!! j,
                                i ==* p &&* j ==* p A.? (the w, bds ! ix))

                bdx1 = foldl1 (>->) foris bdx
            in
                bdx1
                --calc bdx1 xys ws (i - 1) pScalar (unit (iConst + 1))
        
        calcs :: Acc Matrix -> Acc (Vector Double) -> [Acc Matrix -> Acc Matrix]                  
        calcs xys ws = [calc (slice xys (constant (Z :. i :. All))) (unit (ws A.!! (constant i))) (unit (constant pInt)) | i <- [0 .. n - 1]]
        
        calcE :: Acc Matrix -> Acc (Scalar Int) -> Acc (Matrix, Scalar Double)
        calcE bxd jScalar =
            let
                j = the jScalar
                p = constant pInt
                d i = bxd ! (index2 p i)
                x i = bxd ! (index2 (p + 1 + j) i)
                xyp = x p
            in 
               lift (bxd, unit ((d p) * xyp * xyp))
        
        
        
        
        ------------
        --bdx = fromList (Z:. pInt + 2 :. pInt + 1) $ (toList b) Prelude.++ ((Prelude.init (toList d)) Prelude.++ [w]) Prelude.++ (xVect Prelude.++ [y])
        --bds1 = run $ foldl1 (>->) foris (use bdx)
        ------------
        bds = fromList (Z:. pInt + 1 :. pInt + 1) $ (toList b) Prelude.++ ((Prelude.init (toList d)) Prelude.++ [0])
        xys = fromList (Z:. n :. pInt + 1) $ Prelude.concat $ Prelude.zipWith (\xs y -> xs Prelude.++ [y]) xVect y
        ws = fromList (Z:. n) w
        --pScalar = unit (constant pInt)
        --iScalar = unit (constant 0)
        --bds1 = run $ calc (use bds) (use xys) (use ws) n pScalar iScalar
        bds1 = run $ foldl1 (>->) (calcs (use xys) (use ws)) (use bds)
        ------------
        --(b1, x1, d1) = for__ 0 (p - 1) (b, vector (values xVect ++ [y]), (V.set p w d)) fori;
        bds1List = toList bds1
        b1 = fromList (arrayShape b) $ Prelude.take (pInt * (pInt + 1)) bds1List
        xd1 = Prelude.drop (pInt * (pInt + 1)) bds1List
        d1 = Prelude.take (pInt + 1) xd1
        --x1 = Prelude.drop (pInt + 1) xd1
        --xyp = x1 Prelude.!! pInt
    in LSQState (b1, fromList (arrayShape d) d1, e)-- + (head (toList e1)))


invert :: LSQState      -- previous state of calculation
       -> LSQState      -- next state of calculation
invert (LSQState (b, d, e)) = LSQState (b, f, e) where
    f = run (A.map (\x -> 1 / x) (use f))
    --f = vector [1 / V.get i d | i <- [0 .. V.getLength d - 1]]


-- | Adds a next row of constraints to the computation. If you have
--   finished adding constraints, call 'solve' to obtain the LSQ estimate.
addConstraint ::
          [Double] -- ^ array of r values
       -> Double        -- ^ s value
       -> Double        -- ^ weight of the constraint (0 = infinite)
       -> LSQState      -- previous state of calculation
       -> LSQState      -- next state of calculation
addConstraint rVect s w (LSQState (b, f, e)) = 
    let
        pInt = Prelude.length rVect
        p = constant $ pInt
        fori :: Acc (Scalar Int) -> Acc (Matrix, Vector Double, Vector Double) -> Acc (Matrix, Vector Double, Vector Double)
        fori iScalar brf = 
            let 
                i = the iScalar
                (b, r, f) = unlift brf
                h = r A.!! i
                then1 =
                   let 
                        genR1 :: Exp DIM1 -> Exp Double
                        genR1 ix = 
                            let
                                (Z :. k) = unlift ix
                            in
                                k <=* i ||* k >* p A.? (r A.!! k,
                                    r A.!! k - h * (b A.! (index2 i k)))
                      in
                            lift (b, generate (shape r) genR1, f)
                then2 = 
                    let 
                        fm = f A.!! p
                        fi = f A.!! i
                        f' =  fm + fi * h * h;
                        c' = fm / f';
                        s' = fi * h / f';
                        f1 = generate (shape f) genF1 where
                            genF1 :: Exp DIM1 -> Exp Double
                            genF1 ix = 
                                let
                                    (Z :. k) = unlift ix
                                in
                                    k ==* i A.? (c' * fi,
                                        k ==* p A.? (f',
                                            f A.!! k))

                        b1 = generate (shape b) genB1 where
                            genB1 :: Exp DIM2 -> Exp Double
                            genB1 ix = 
                                let
                                    (Z :. i1 :. k1) = unlift ix
                                    g1 = b A.! (index2 i1 k1);
                                    g2 = r A.!! k1
                                    fkb = b ! (index2 i1 k1)
                                in
                                    i1 /=* i ||* k1 <=* i ||* k1 >* p A.? (fkb,
                                        c' * g1 + s' * g2)
                        
                        r1 = generate (shape r) genR1 where
                            genR1 :: Exp DIM1 -> Exp Double
                            genR1 ix = 
                                let
                                    (Z :. k1) = unlift ix
                                    g1 = b A.! (index2 i k1);
                                    g2 = r A.!! k1
                                    gkx = r A.!! k1
                                in
                                    k1 <=* i ||* k1 >* p A.? (gkx,
                                        g2 - h * g1)
                        
                        
                     in lift (b1, r1, f1)
            in
                f A.!! i ==* 0 &&* h /=* 0 A.?| (then1, 
                    h /=* 0 A.?| (then2, 
                        lift (b, r, f)))
        foris :: [Acc (Matrix, Vector Double, Vector Double) -> Acc (Matrix, Vector Double, Vector Double)]                  
        foris =  [fori (unit (constant i)) | i <- [0 .. pInt - 1]]
        fs = toList f
        (b1, r1, f1) = run $ foldl1 (>->) foris (use (b, fromList (Z:.(pInt + 1)) (rVect Prelude.++ [s]), fromList (Z:.Prelude.length fs) ((Prelude.init fs) Prelude.++ [w])))
        --(b1, r1, f1) = run $ for__ 0 (p - 1) (b, vector (values rVect ++ [s]), (V.set p w f)) fori;
        rsp = indexArray r1 (Z:.pInt)
        fm = indexArray f (Z:.pInt)
        e1 = if fm /= 0 then (e + rsp * rsp / fm) else e
    in LSQState (b1, f1, e1)

solve :: LSQState -> [Double]
solve (LSQState (b, _, _)) = toList $ backSubstitution b

