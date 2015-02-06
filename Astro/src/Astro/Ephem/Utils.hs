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
clipAngleRad angle
    | angle >= 0 && angle < pi2 = angle
    | angle < 0 =
        let
            (n, f) = properFraction (angle / pi2)
        in
            pi2 + pi2 * f
    | otherwise = 
        let
            (n, f) = properFraction (angle / pi2)
        in
            pi2 * f

clipAngleDeg :: Double -> Double
clipAngleDeg angle
    | angle >= 0 && angle < 360 = angle
    | angle < 0 = 
        let
            (n, f) = properFraction (angle / 360)
        in
            360 + 360 * f
    | otherwise = 
        let
            (n, f) = properFraction (angle / 360)
        in
            360 * f

clipHour :: Double -> Double
clipHour hour
    | hour >= 0 && hour < 24 = hour
    | hour < 0 = 
            let
            (n, f) = properFraction (hour / 24)
        in
            24 + 24 * f
    | otherwise = 
            let
            (n, f) = properFraction (hour / 24)
        in
            24 * f


calcElement :: (Double, Double) -> Double -> Double
calcElement (elem, elemRate) ctys = elem + elemRate * ctys

calcAngleElement :: (Angle, Angle) -> Double -> Angle
calcAngleElement (elem, elemRate) timePassed = elem `add` (elemRate `mul` timePassed)

