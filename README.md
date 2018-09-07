# haskell-imp

Monadic parser for a simple imperative language. Mainly implemented to study functors, applicatives, monads.

### Grammar
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
