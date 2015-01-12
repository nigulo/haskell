module Main where

import Debug.Trace
import ISDA.InOut
import Utils.Str

import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.FFT as F
import Math.Vector

import Foreign.Storable
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Complex
import Foreign.C.String

getFFT :: ISDAState -> IO (Spec, Spec)
getFFT (params, Right spec@(specHeader, specBlocks)) = 
    do
        let 
            --tMin = head (ts dataBlocks);
            --tMax = last (ts dataBlocks);
            n = fromIntegral (nLim specHeader)
            step = (swMax specHeader - swMin specHeader) / (fromIntegral n - 1)
            ys = map (realToFrac) (pty specBlocks)
            fftFunc = case getString params "FFT_Direction" of 
                       Just "OFF" -> F.fromFrequencyToTime
                       Just "ON" -> F.fromTimeToFrequency
                       Nothing -> F.fromTimeToFrequency
            bandStart = case getParam params "FFT_Band_Start" of 
                            Just a -> a
                            Nothing -> 0.5
            bandEnd = case getParam params "FFT_Band_End" of 
                       Just a -> a
                       Nothing -> 1.5
            prec = case getParam params "FFT_Precision" of 
                       Just a -> a
                       Nothing -> 1000

        spec <- fftFunc (map (\y -> (:+) y 0) ys) n 0 (\_ -> return ())
        return
                ((SpecHeader {nLim = fromIntegral (length spec), 
                                   swMin = bandStart, 
                                   swMax = bandEnd}, 
                       SpecBlocks {pty = map realToFrac (map realPart spec)}),


                 (SpecHeader {nLim = fromIntegral (length spec), 
                                   swMin = bandStart, 
                                   swMax = bandEnd}, 
                       SpecBlocks {pty = map realToFrac (map imagPart spec)}))


main ::IO ()
main = 
    do
        state@(params, _) <- readState
        (realSpec, imagSpec) <- getFFT state
        writeStateToFile "OUTPUT" (params, Right realSpec)
        writeStateToFile "IMAG" (params, Right imagSpec)
        writeLn "Data converted to IMF successfully"


