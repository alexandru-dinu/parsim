import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Test.HUnit

import Runner (runEval, runRaw)
import TestUtils (mkPairs, mkTest)
import Types (Prog)

testRaw :: IO Test
testRaw = do
    pairs <- mkPairs "testdata/raw/"
    tests <- mapM (uncurry (mkTest getResult)) pairs
    return $ TestList tests
  where
    getResult :: String -> String
    getResult = show . runRaw

testAdt :: IO Test
testAdt = do
    pairs <- mkPairs "testdata/adt/"
    tests <- mapM (uncurry (mkTest getResult)) pairs
    return $ TestList tests
  where
    getResult :: String -> String
    getResult inp = (show . runEval) (read inp :: Prog)

main :: IO ()
main = do
    resRaw <- testRaw >>= runTestTT
    resAdt <- testAdt >>= runTestTT
    if all (== 0) [f x | f <- [errors, failures], x <- [resRaw, resAdt]]
        then exitSuccess
        else exitWith (ExitFailure 1)
