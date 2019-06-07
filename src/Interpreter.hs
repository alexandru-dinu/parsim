module Interpreter where

import Types


type Context = [Variable]
type Variable = (String, Int)

newtype Possibly a = P (Either String a) deriving Show

instance Functor Possibly where
    fmap f (P res) = case res of
        Left m  -> P $ Left m
        Right r -> P $ Right $ f r

instance Applicative Possibly where
    pure e = P $ Right e
    (P res) <*> x = case res of
        Right f -> fmap f x
        Left m  -> P $ Left m

instance Monad Possibly where
    return = pure
    fail m = P $ Left m
    (P res) >>= f = case res of
        Left m  -> fail m
        Right r -> f r


getFrom :: Context -> String -> Possibly Int
getFrom [] "$" = fail "Missing return"
getFrom [] _ = fail "Uninitialized variable"
getFrom ((x, v):cs) s = if x == s then pure v else getFrom cs s

-- evaluate expressions
evalExpr :: Expr -> Context -> Possibly Int

evalExpr (Value v) _ = pure v
evalExpr (Symbol s) context = getFrom context s

evalExpr (Add e1 e2) context = do
    r1 <- evalExpr e1 context
    r2 <- evalExpr e2 context
    return $ r1 + r2

evalExpr (Sub e1 e2) context = do
    r1 <- evalExpr e1 context
    r2 <- evalExpr e2 context
    return $ r1 - r2

evalExpr (Mult e1 e2) context = do
    r1 <- evalExpr e1 context
    r2 <- evalExpr e2 context
    return $ r1 * r2

evalExpr (Equal e1 e2) context = do
    r1 <- evalExpr e1 context
    r2 <- evalExpr e2 context
    return $ if r1 == r2 then 1 else 0

evalExpr (Smaller e1 e2) context = do
    r1 <- evalExpr e1 context
    r2 <- evalExpr e2 context
    return $ if r1 < r2 then 1 else 0


-- evaluate programs
evalProg :: Prog -> Context -> Possibly Context

evalProg (Eq var expr) context = do
    res <- evalExpr expr context
    return $ (var, res):context

evalProg (If cond pt pe) context = do
    c <- evalExpr cond context
    if c == 1 then evalProg pt context else evalProg pe context

evalProg (While cond p) context = do
    c <- evalExpr cond context
    case c of
        1 -> do
            c' <- evalProg p context
            evalProg (While cond p) c'
        _ -> return context

evalProg (Return expr) context = do
    res <- evalExpr expr context
    return $ ("$", res):context -- put a special string (not in the grammar)

evalProg (Seq p1 p2) context = do
    c' <- evalProg p1 context
    evalProg p2 c'
