{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Algorithms.MDP.Ex_3_1_Test
import {-@ HTF_TESTS @-} Algorithms.MDP.Ex_3_1_RelativeTest
import {-@ HTF_TESTS @-} Algorithms.MDP.Ex_MM1_Test

main = htfMain htf_importedTests
