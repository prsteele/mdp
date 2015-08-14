{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} MDPTest
import {-@ HTF_TESTS @-} ValueIterationTest
import {-@ HTF_TESTS @-} Ex_3_1Test

main = htfMain htf_importedTests
