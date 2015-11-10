{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Algorithms.MDP.ValueIterationTest

main = htfMain htf_importedTests
