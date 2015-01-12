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
    | angle < 0 = clipAngleRad (angle + pi2)
    | otherwise = clipAngleRad (angle - pi2)

clipAngleDeg :: Double -> Double
clipAngleDeg angle
    | angle >= 0 && angle < 360 = angle
    | angle < 0 = clipAngleDeg (angle + 360)
    | otherwise = clipAngleDeg (angle - 360)

clipHour :: Double -> Double
clipHour hour
    | hour >= 0 && hour < 24 = hour
    | hour < 0 = clipHour (hour + 24)
    | otherwise = clipHour (hour - 24)


calcElement :: (Double, Double) -> Double -> Double
calcElement (elem, elemRate) ctys = elem + elemRate * ctys

calcAngleElement :: (Angle, Angle) -> Double -> Angle
calcAngleElement (elem, elemRate) timePassed = elem `add` (elemRate `mul` timePassed)

