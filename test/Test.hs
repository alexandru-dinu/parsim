import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Test.HUnit

import Interpreter (Possibly(P))
import Runner (runRaw)

spec :: Test
spec =
    TestCase $
    assertEqual "should be 3" (P (Right 3)) (runRaw "x = 1 + 2;\nreturn x;")

main :: IO ()
main = do
    res <- runTestTT spec
    if errors res + failures res == 0
        then exitSuccess
        else exitWith (ExitFailure 1)
