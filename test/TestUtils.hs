module TestUtils
    ( mkPairs
    , mkTest
    ) where

import Data.List (sort)
import qualified Data.Text as T
import System.FilePath.Glob (glob)
import Test.HUnit

strip :: String -> String
strip = T.unpack . T.strip . T.pack

mkPairs :: String -> IO [(FilePath, FilePath)]
mkPairs root = do
    inpFiles <- glob $ root ++ "*.in"
    outFiles <- glob $ root ++ "*.out"
    return $ zip (sort inpFiles) (sort outFiles)

mkTest :: (String -> String) -> FilePath -> FilePath -> IO Test
mkTest runner inputFile outputFile = do
    input <- readFile inputFile
    expected <- readFile outputFile
    let actual = runner input
    let res = assertEqual "Output does not match expected value" (strip expected) actual
    return $ TestCase res
