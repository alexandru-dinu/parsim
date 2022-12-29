module Types (Expr(..), Prog(..)) where

data Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Equal Expr Expr
    | Smaller Expr Expr
    | Symbol String
    | Value Int
    deriving (Show, Read)

data Prog
    = Eq String Expr
    | Seq Prog Prog
    | If Expr Prog Prog
    | While Expr Prog
    | Return Expr
    deriving (Show, Read)
