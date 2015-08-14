-- | The base module for our tests.
module TestingBase where

import Control.Monad
import Test.QuickCheck.Test
import System.Exit

-- | A quickcheck property and the name of the associated test.
type TestProperty = (String, IO (Result))

-- | The result of a quickcheck property and the name of the
-- associated test.
type TestResult = (String, Result)

-- | Perform all tests.
runTests :: [TestProperty] -> IO [TestResult]
runTests tests = do
  results <- mapM snd tests
  return $ zip (map fst tests) results

-- | Find all failed tests.
getFailures :: [TestResult] -> [TestResult]
getFailures results = filter (not . isSuccess . snd) results

-- | Print the names of any failed tests.
printFailures :: [TestResult] -> IO ()
printFailures results = let
  failed = getFailures results
  failedNames = map fst failed
  in if null failed
     then return ()
     else do
       putStrLn "Failed tests:"
       mapM_ putStrLn failedNames

-- | Run all tests, report the results, and exit with the appropriate
-- exit code.
testingMain :: [TestProperty] -> IO ()
testingMain tests = do
  results <- runTests tests
  printFailures results
  unless (null (getFailures results)) exitFailure

