module Ephem.UtilsTest (tests) where

import Ephem.Types
import Ephem.Utils
import Ephem.TestUtils
import Test.Tasty
import Test.Tasty.HUnit

test_clipAngle :: Assertion
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

test_clipHour :: Assertion
test_clipHour = do
    assertEqualFloat 0 (clipHour 0)
    assertEqualFloat 0 (clipHour 24)
    assertEqualFloat 1 (clipHour 1)
    assertEqualFloat 1 (clipHour 25)
    assertEqualFloat 23 (clipHour (-1))
    assertEqualFloat 23 (clipHour (-25))

tests :: TestTree
tests = testGroup "Ephem.UtilsTest"
    [ testCase "clipAngle" test_clipAngle
    , testCase "clipHour" test_clipHour
    ]
