{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Astro.Ephem.TestUtils where

import Astro.Ephem.Types
import Astro.Ephem.Coords
import Test.Framework

assertEqualDouble x y = 
    --assertBool $ abs (x - y) < 1e-14        
    assertEqual (fromIntegral (round (x * 1e11) :: Integer) * 1e-11 :: Double) (fromIntegral (round (y * 1e11) :: Integer) * 1e-11 :: Double)        

assertEqualFloat x y = 
    assertEqual (fromIntegral (round (x * 1e6) :: Integer) * 1e-6 :: Double) (fromIntegral (round (y * 1e6) :: Integer) * 1e-6 :: Double)        

assertEqualAngle (DMS d1 m1 s1) (DMS d2 m2 s2) = do 
    assertEqual d1 d2  
    assertEqual m1 m2  
    assertEqualFloat s1 s2
assertEqualAngle (Deg x) (Deg y) = assertEqualDouble x y  
assertEqualAngle (Rad x) (Rad y) = assertEqualDouble x y
assertEqualAngle (Sec x) (Sec y) = assertEqualDouble x y
assertEqualAngle angle1@(DMS _ _ _) angle2 = assertEqualAngle angle1 (toDMS angle2)
assertEqualAngle angle1@(Deg _) angle2 = assertEqualAngle angle1 (toDeg angle2)  
assertEqualAngle angle1@(Rad _) angle2 = assertEqualAngle angle1 (toRad angle2)  
assertEqualAngle angle1@(Sec _) angle2 = assertEqualAngle angle1 (toSec angle2)  

assertEqualHours (HMS h1 m1 s1) (HMS h2 m2 s2) = do 
    assertEqual h1 h2  
    assertEqual m1 m2  
    assertEqualFloat s1 s2
assertEqualHours (Hrs x) (Hrs y) = assertEqualDouble x y 
assertEqualHours hours1@(HMS _ _ _) hours2 = assertEqualHours (hours1) (toHMS hours2) 
assertEqualHours hours1@(Hrs _) hours2 = assertEqualHours (hours1) (toHrs hours2) 

