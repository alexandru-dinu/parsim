# Monadic Parser

[![Build](https://github.com/alexandru-dinu/monadic-parser/actions/workflows/build.yml/badge.svg)](https://github.com/alexandru-dinu/monadic-parser/actions/workflows/build.yml)

Monadic parser for a simple imperative language.
Implemented mainly to study {functors, applicatives, monads} and to set up a proper haskell project.

## Grammar
```
<expr>   ::= <expr> <op> <expr> | <symbol> | <value>
<symbol> ::= [a-zA-Z]+
<value>  ::= [1-9][0-9]* | 0
<op>     ::= + | - | * | == | <
<prog>   ::= <symbol> = <expr>;
           | <prog> <prog>
           | if (<expr>) then {<prog>} else {<prog>}
           | while (<expr>) {<prog>}
           | return <expr>;
```
The corresponding **ADT** is defined in [Types.hs](./src/Types.hs).

## Example
Given the following program (defined in `testdata/raw/test1-10.in`):
```
y = 1 * 2;
x = 4 + y;
if (y == 2) then {
    z = 0;
    while (z < 3) {
        x = x + 1;
        y = x + 2;
        z = z + 1;
    }
    return y;
} else {
    return x + y;
}
```
you can use `make run INPUT=testdata/raw/test1-10.in` to run it, obtaining the following output:
```
P (Right 11)
```
which indicates a successful run.

## Errors

### Syntax error
E.g. missing `;`
```
a = 15;
b = 25;

while (0 < b) {
    if (b < a) then {
        a = a - b
    } else {
        b = b - a;
    }
}

c = a;
return c;
```
Output:
```
P (Left "Syntax error")
```

### Uninitialized variable
Variable `x` is uninitialized:
```
z = 0;
while (z < 3) {
    x = x + z;
    z = z + 1;
}
return z;
```
Output:
```
P (Left "Uninitialized variable")
```

### Missing return
No return path:
```
x = 10;
y = 20;
z = x + y;
```
Output:
```
P (Left "Missing return")
```

## Resources

[G. Hutton, E. Meijer - Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
