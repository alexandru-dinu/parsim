import Data.List (sort)
import qualified Data.Text as T
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath.Glob (glob)
import Test.HUnit

import Runner (runRaw)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

mkPairs :: IO [(FilePath, FilePath)]
mkPairs = do
    inpFiles <- glob "testdata/raw/*.in"
    outFiles <- glob "testdata/raw/*.out"
    return $ zip (sort inpFiles) (sort outFiles)

mkTest :: FilePath -> FilePath -> IO Test
mkTest inputFile outputFile = do
    input <- readFile inputFile
    expected <- readFile outputFile
    let actual = runRaw input
    let res =
            assertEqual
                "Output does not match expected value"
                (strip expected)
                (show actual)
    return $ TestCase res

testPairs :: IO Test
testPairs = do
    pairs <- mkPairs
    tests <- mapM (uncurry mkTest) pairs
    return $ TestList tests

main :: IO ()
main = do
    res <- testPairs >>= runTestTT
    if errors res + failures res == 0
        then exitSuccess
        else exitWith (ExitFailure 1)
