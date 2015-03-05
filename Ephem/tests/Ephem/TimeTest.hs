{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ephem.TimeTest where

import Ephem.Types
import Ephem.Time
import Ephem.TestUtils
import Test.Framework


test_gmtToGST = do
    let
        date = YMD 1980 4 22
        gmt = HMS 14 36 51.67
        gst = gmtToGST gmt date
    assertEqualHours (HMS 4 40 5.164531805391448) (toHMS gst)

test_calcB = do
    assertEqualDouble 17.395558591334748 (calcB 1979)
    assertEqualDouble 17.401211325672648 (calcB 2000)

test_gstToGMT = do
    let
        date = YMD 1980 4 22
        gst = HMS 4 40 5.17
        gmt = gstToGMT gst date
    assertEqualHours (HMS 14 36 51.70308807222682) (toHMS gmt)

test_gstToLST = do
    let
        longitude = Long (Deg 64) W
        gst = HMS 4 40 5.17
        lst = gstToLST gst longitude
    assertEqualHours (HMS 0 24 5.16999999999598) (toHMS lst)

test_lstToGST = do
    let
        longitude = Long (Deg 64) W
        lst = HMS 0 24 5.17
        gst = lstToGST lst longitude
    assertEqualHours (HMS 4 40 5.169999999992569) (toHMS gst)
