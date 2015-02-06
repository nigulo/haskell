{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Astro.Ephem.UtilsTest where

import Astro.Ephem.Types
import Astro.Ephem.Utils
import Astro.Ephem.TestUtils
import Test.Framework

test_clipAngle = do 
    assertEqualAngle (Deg 0) (clipAngle (Deg 0))
    assertEqualAngle (Deg 0) (clipAngle (Deg 360))
    assertEqualAngle (Deg 5) (clipAngle (Deg 5))
    assertEqualAngle (Deg 5) (clipAngle (Deg 365))
    assertEqualAngle (Deg 355) (clipAngle (Deg (-365)))
    assertEqualAngle (Deg 355) (clipAngle (Deg (-5)))

    assertEqualAngle (Rad 0) (clipAngle (Rad 0))
    assertEqualAngle (Rad 0) (clipAngle (Rad pi2))
    assertEqualAngle (Rad 0.1) (clipAngle (Rad 0.1))
    assertEqualAngle (Rad 0.1) (clipAngle (Rad (pi2 + 0.1)))
    assertEqualAngle (Rad (pi2 - 0.1)) (clipAngle (Rad (-pi2 - 0.1)))
    assertEqualAngle (Rad (pi2 - 0.1)) (clipAngle (Rad (-0.1)))

test_clipHour = do 
    assertEqual 0 (clipHour 0)
    assertEqual 0 (clipHour 24)
    assertEqual 1 (clipHour 1)
    assertEqual 1 (clipHour 25)
    assertEqual 23 (clipHour (-1))
    assertEqual 23 (clipHour (-25))
    