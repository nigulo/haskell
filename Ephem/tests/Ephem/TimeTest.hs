module Ephem.TimeTest (tests) where

import Ephem.Types
import Ephem.Time
import Ephem.TestUtils
import Test.Tasty
import Test.Tasty.HUnit


test_gmtToGST :: Assertion
test_gmtToGST = do
    let
        gmt = fromDateAndHours (YMD 1980 4 22) (HMS 14 36 51.67)
        gst = gmtToGST gmt
    assertEqualHours (HMS 4 40 5.228953) (toHMS gst)

test_gstToGMT :: Assertion
test_gstToGMT = do
    let
        date = YMD 1980 4 22
        gst = HMS 4 40 5.17
        gmt = gstToGMT gst date
    assertEqualHours (HMS 14 36 51.611232) (getHours gmt)

test_gstToLST :: Assertion
test_gstToLST = do
    let
        longitude = Long (Deg 64) W
        gst = HMS 4 40 5.17
        lst = gstToLST gst longitude
    assertEqualHours (HMS 0 24 5.16999999999598) (toHMS lst)

test_lstToGST :: Assertion
test_lstToGST = do
    let
        longitude = Long (Deg 64) W
        lst = HMS 0 24 5.17
        gst = lstToGST lst longitude
    assertEqualHours (HMS 4 40 5.169999999992569) (toHMS gst)

tests :: TestTree
tests = testGroup "Ephem.TimeTest"
    [ testCase "gmtToGST" test_gmtToGST
    , testCase "gstToGMT" test_gstToGMT
    , testCase "gstToLST" test_gstToLST
    , testCase "lstToGST" test_lstToGST
    ]
