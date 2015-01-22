module Regression.FFT (
    fromTimeToFrequency,
    fromFrequencyToTime) where


import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Foreign.Storable
import Data.Complex
import Data.Array.CArray
import qualified Math.FFT as FFT
import Data.Bits

fromTimeToFrequency :: V.Vector (Complex Double) -- ^ List of complex numbers
        -> Double -- ^ Phase shift
        -> IO (V.Vector (Complex Double)) -- ^ Transformed values
fromTimeToFrequency = transform FFT.dft  

fromFrequencyToTime :: V.Vector (Complex Double) -- ^ List of complex numbers
        -> Double -- ^ Phase shift
        -> IO (V.Vector (Complex Double)) -- ^ Transformed values
fromFrequencyToTime = transform FFT.idft

transform :: (CArray Int (Complex Double) -> CArray Int (Complex Double)) 
        -> V.Vector (Complex Double)
        -> Double
        -> IO (V.Vector (Complex Double))
transform fftFunc inputValues phaseShift = do
    let
        n = V.length inputValues
        nDiv2 = n `shiftR` 1
        shiftedValues = if phaseShift /= 0
            then 
                let
                    fori i | i > 0 && i < nDiv2 = inputValues V.! i * (mkPolar 1 (-phaseShift))
                           | i > nDiv2 = inputValues V.! i * (mkPolar 1 (phaseShift))
                           | otherwise = inputValues V.! i
                in                
                    V.map (\i -> fori i) (V.generate (V.length inputValues) (\i -> i))
            else
                inputValues
    inArray <- vectorToCArray shiftedValues
    let
        outArray = fftFunc inArray
    cArrayToVector outArray

vectorToCArray :: V.Vector (Complex Double) -> IO (CArray Int (Complex Double))
vectorToCArray v =
    createCArray (0, V.length v - 1) $ \ptrToVals -> 
        V.zipWithM_ (\i val -> pokeElemOff ptrToVals i val) (V.generate (V.length v) (\i -> i)) v

cArrayToVector :: CArray Int (Complex Double) -> IO (V.Vector (Complex Double))
cArrayToVector a =
    withCArray a $ \ptrToVals -> 
        V.mapM (\i -> peekElemOff ptrToVals i) (V.generate (size a) (\i -> i))
 