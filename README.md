# Arithmetic Expression Parser

Simple arithmetic expression parser using Ocaml

## Overview

This project uses Ocaml to parse and evaluate basic arithmetic expression. The
operator and expression of this arithmetic expression can be defined as

```ocaml
type binop = Add | Sub | Mul | Pow
```

and

```ocaml
type expression =
    | Num of float
    | Var
    | Binop of binop * expression * expression
    | Neg of expression
```

where `Num n` represents the floating-point number `n`. `Var` represents the
only variable, which is `x`. `Binop (o, e1, e2)` represents a binary operation
`o` between expression `e1` and expression `e2` where `o` can be either `Add`,
`Sub`, `Mul`, or `Pow`. And `Neg e` is the negation of expression `e` (in this
parser, we will implement negation as `~` symbol to remove ambiguity in the
language while parsing expression string).

For example, we could represent `-3.0x^2 + 2.0` as

```
Binop (Add, Binop (Mul, Binop (Pow, Var, Num 2.0)), Num 2.0)
```

Parsing and evalutating only support functions with at most one variable `x`. 
Including different variables other than `x` would raise `ParseError`.

## Installation

This project requires `opam` Ocaml package management and `ocaml` installed in
your local machine. For more information, visit [https://ocaml.org/learn/tutorials/up_and_running.html](https://ocaml.org/learn/tutorials/up_and_running.html)
for instruction on how to install `opam` and `ocaml`.

Clone this repository to your local machine

```
git clone https://github.com/b72u68/arithmetic-expressions
```

## Running

In your shell, run the following command

```bashscript
ocaml
```
to open the Ocaml shell installed in your local machine. In the Ocaml shell,
run `#use 'expressions.ml';;` to import the file and use it.
