module Runner where

import Control.Applicative (Alternative((<|>)))

import Interpreter (Possibly, evalProg, getFrom)
import Parser (Parser(parseWith), progp, seqp)
import Types (Prog)

-- parser main
runParser :: String -> Possibly Prog
runParser s =
    case parseWith (seqp <|> progp) s of
        [(prog, "")] -> pure prog
        _ -> fail "Syntax error"

-- eval main
runEval :: Prog -> Possibly Int
runEval p = do
    c' <- evalProg p []
    getFrom c' "$"

-- raw (parse -> eval)
runRaw :: String -> Possibly Int
runRaw s = do
    p <- runParser s
    runEval p
