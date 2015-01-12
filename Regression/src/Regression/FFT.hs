module Regression.FFT (
    fromTimeToFrequency,
    fromFrequencyToTime) where

import Data.Complex
import Data.Bits
import qualified Data.Map as Map
import Data.List as List
import Utils.Misc
import Math.IOMatrix as M 
import Math.IOVector as V
import Debug.Trace
import Control.Concurrent.MVar


fromTimeToFrequency :: [Complex Double] -- ^ List of complex numbers
          -> Int -- ^ Number of output frequences / time moments
          -> Double  -- ^ Phase shift in radians
          -> (Double -> IO ()) -- ^ progressUpdate func
          -> IO [Complex Double] -- ^ Transformed values
fromTimeToFrequency inputValues kMax phaseShift puFunc = 
    transform inputValues kMax 1 (-1) phaseShift puFunc

fromFrequencyToTime :: [Complex Double] -- ^ List of complex numbers
          -> Int -- ^ Number of output frequences / time moments
          -> Double  -- ^ Phase shift in radians
          -> (Double -> IO ()) -- ^ progressUpdate func
          -> IO [Complex Double] -- ^ Transformed values
fromFrequencyToTime inputValues kMax phaseShift puFunc = 
    transform inputValues kMax (1 / fromIntegral (length inputValues)) 1 phaseShift puFunc

transform :: [Complex Double] -- ^ List of complex numbers
          -> Int -- ^ Number of output frequences / time moments
          -> Double -- ^ The whole transformation is multiplied with this coeficient 
                    --   Should be 1 in case direct transform, 1/N in case backward
                    --   transform
          -> Double 
          -> Double
          -> (Double -> IO ()) -- ^ progressUpdate func
          -> IO [Complex Double] -- ^ Transformed values
transform inputValues _ coef exponCoef phaseShift puFunc =
    do
        countRef <- newMVar 0 -- just for progress update purposes
        let 
            n = length inputValues
            nDiv2 = n `shiftR` 1
            cCoef = (:+) coef 0
            kList = [0 .. n - 1]
            shiftedValues = [fori i | i <- kList] where
                fori i | i > 0 && i < nDiv2 = inputValues !! i * (mkPolar 1 (-phaseShift))
                       | i > nDiv2 = inputValues !! i * (mkPolar 1 (phaseShift))
                       | otherwise = inputValues !! i
        swappedVals <- swap shiftedValues kList >>= vector
        kVect <- vector kList
        
        let    
            n2 = n `shiftL` 1
            log2nm2 = round (logBase 2 (fromIntegral n)) - 2
            phaseCoef = exponCoef * 2 * pi
            w m k = cis phase where 
                phase = phaseCoef * (fromIntegral k) / m
            -------------------
            -- first time 
            firstM = fromIntegral n2 / (fromIntegral n)
            firstVals j =
                do 
                    v1 <- V.get j swappedVals
                    v2 <- V.get (j + 1) swappedVals  
                    vector $ map (\k -> v1 + (w firstM k) * v2) kList
        
            vals (-1) j = firstVals (j `shiftL` 1)
            vals i j = 
                do
                    let
                        maxLen = nDiv2 `shiftR` i
                        m = fromIntegral n2 / (fromIntegral maxLen)
                        j2 = j `shiftL` 1
                    v1s <- vals (i - 1) j2
                    v2s <- vals (i - 1) (j2 + 1)
                    vs <- V.op3 (\v1 v2 k -> v1 + (w m k) * v2) v1s v2s kVect
                    count <- takeMVar countRef
                    puFunc $ (fromIntegral count) / (fromIntegral n)
                    putMVar countRef (count + 2)
                    return vs
                    
        --result <- vals log2nm2 0 >>= V.values
        --return $ mapM (\x -> ((:+) coef 0) * x) result
        result <- vals log2nm2 0 >>= V.elemOp ((:+) coef 0) (*)
        puFunc 0
        V.values result 


-- | returns an array of pairs where the first element is a swappad value
--   from input array and second element is the original array index of the
--   element    
swap :: [a] -> [Int] -> IO [a]
swap array kList =
    do  
        let 
            numBits = round (logBase 2 (fromIntegral (length array)))
            func i =
                do
                    let
                        val = array !! swappedIndex where
                            swappedIndex = swapBits numBits i
                    return val

        mapM func kList

swapBits :: Bits a => Int -> a -> a
swapBits numBits x =
    let 
        lastBit = numBits - 1
        swapBit bit x =
            let
                bit1Set = testBit x bit
                bit2Set = testBit x (lastBit - bit)
                
                setBitWith :: Bits a => Int -> Bool -> a -> a
                setBitWith bit value x = if value then setBit x bit else clearBit x bit
            in 
                setBitWith (lastBit - bit) bit1Set $
                setBitWith bit bit2Set x
    in
        let swapped = for__ 0 ((shiftR numBits 1) - 1) x swapBit
        in swapped 

