# haskell-imp

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