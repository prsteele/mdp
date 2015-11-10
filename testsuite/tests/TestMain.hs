{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Algorithms.MDP.DiscountedValueIterationTest

main = htfMain htf_importedTests
