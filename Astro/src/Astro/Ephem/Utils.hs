module Astro.Ephem.Utils (
    pi2,
    clipAngle,
    clipAngleRad,
    clipAngleDeg,
    clipHour,
    calcElement,
    calcAngleElement
    )
    where

import Astro.Ephem.Types

pi2 :: Double
pi2 = 2 * pi

clipAngle :: Angle -> Angle
clipAngle (Rad x) = Rad $ clipAngleRad x
clipAngle (Deg x) = Deg $ clipAngleDeg x

clipAngleRad :: Double -> Double
clipAngleRad = clip pi2

clipAngleDeg :: Double -> Double
clipAngleDeg = clip 360

clipHour :: Double -> Double
clipHour = clip 24

clip :: Double -> Double -> Double
clip clipVal val
    | val >= 0 && val < clipVal = val
    | val < 0 = 
        let
            (n, f) = properFraction (val / clipVal)
        in
            clipVal + clipVal * f
    | otherwise =
        if isNaN val || isInfinite val 
            then val 
            else 
                let
                    (n, f) = properFraction (val / clipVal)
                in
                    clipVal * f
 

calcElement :: (Double, Double) -> Double -> Double
calcElement (elem, elemRate) ctys = elem + elemRate * ctys

calcAngleElement :: (Angle, Angle) -> Double -> Angle
calcAngleElement (elem, elemRate) timePassed = elem `add` (elemRate `mul` timePassed)

