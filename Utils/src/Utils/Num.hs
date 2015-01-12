
module Utils.Num (extendedToDouble, doubleToExtended, Word8, isInteger, isDouble, isComplex, isNumeric) where

import Data.Bits
import Data.Word
import Data.Complex

--------------------------------------------------------------------------------
-- Helper methods for converting extended (80 bit) floating point number to double
--------------------------------------------------------------------------------

getMantissa :: [Word8] -> Integer
getMantissa extended = let mantissa = fromIntegral (shifting 0 7) --(shiftR (shifting 0 7) 11)
                                                          where shifting :: Integer -> Int -> Integer;
                                                                         shifting w i = --let byte = if (i == 7) then (extended !! i) .&. 0x7F else (extended !! i)
                                                                                                         --in 
                                                                                                         if i < 0 then w
                                                                                                               else shifting ((shiftL w 8) + (fromIntegral (extended !! i))) (i - 1)
                                                  in
                                                          if (testBit (extended !! 9) 7) then negate mantissa
                                                                                                                     else mantissa

getExponent :: [Word8] -> Int
getExponent extended = let a :: Word16;
                                                    a = ((fromIntegral((extended !! 9) .&. 0x7F)) `shiftL` 8) + fromIntegral(extended !! 8)
                                            in (fromIntegral a) - 0x3FFF - 63

-- Extended number to double number format
extendedToDouble :: [Word8] -> Double
extendedToDouble extended = encodeFloat (getMantissa extended) (getExponent extended)

--------------------------------------------------------------------------------

toBytes :: Integer -> Int -> [Word8]
toBytes value numBytes = [fromIntegral ((shiftR value (i * 8)) .&. 0xFF) | i <- [0 .. numBytes - 1]]

-- Converts double number to extended number format
doubleToExtended :: Double -> [Word8]
doubleToExtended d = let (mantissa, expon) = decodeFloat d;
                                         in if (mantissa == 0) then take 10 (repeat 0)
                                                 else
                                                         let
                                                                mantissaBytes = toBytes (shiftL (abs mantissa) 11) 8;
                                                                [eByte0, eByte1] = toBytes (toInteger (expon + 52 + 0x3FFF)) 2;
                                                                sign = if (mantissa < 0) then 0x80 else 0 :: Word8
                                                        in mantissaBytes ++ [eByte0] ++ [(eByte1 .|. sign)]
--------------------------------------------------------------------------------

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

isComplex s = case reads s :: [(Complex Double, String)] of
  [(_, "")] -> True
  otherwise         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s
