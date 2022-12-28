# Monadic Parser

[![Build](https://github.com/alexandru-dinu/monadic-parser/actions/workflows/build.yml/badge.svg)](https://github.com/alexandru-dinu/monadic-parser/actions/workflows/build.yml)

Monadic parser for a simple imperative language. Mainly implemented to study functors, applicatives, monads.

## Grammar
```
<expr>      ::= <expr> <op> <expr> | <symbol> | <value>
<symbol>    ::= [a-zA-Z]+
<value>     ::= [1-9][0-9]* | 0
<op>        ::= + | - | * | == | <
<prog>      ::= <symbol> = <expr>;
            | <prog> <prog>
            | if (<expr>) then {<prog>} else {<prog>}
            | while (<expr>) {<prog>}
            | return <expr>;
```

## ADT
```
data Expr = Add     Expr   Expr
          | Sub     Expr   Expr
          | Mult    Expr   Expr
          | Equal   Expr   Expr
          | Smaller Expr   Expr
          | Symbol  String
          | Value   Int


data Prog = Eq     String Expr
          | Seq    Prog   Prog
          | If     Expr   Prog Prog
          | While  Expr   Prog
          | Return Expr
```

### Resources

[G. Hutton, E. Meijer - Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
