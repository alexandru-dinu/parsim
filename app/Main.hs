module Main where

import System.Environment (getArgs)

import Runner (runRaw)

main :: IO ()
main = do
    a <- getArgs
    p <- readFile $ head a
    print $ runRaw p
