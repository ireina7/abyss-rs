# abyss-rs
Rust implementation of Abyss programming language

![Made in abyss](./img/made-in-abyss.jpg)

## Features
- Typed dynamic programming language
- Highly configurable and scalable
- Experimental for the next generation

## Expressions
- S-Expressions
- Scalable front-end syntax with parser combinator

## Evaluation
- Optional evaluation strategy: eager or lazy
- Flexible GC strategy

## Data abstraction
- Haskell-like `data` declaration
- Pattern matching everywhere

## Type system
- Dependent type system

## Examples
- Factorial
```scheme
(let (((fact n) (case n ((0 1) (n (* n (fact (- n 1)))))))) (fact 5))
;==> 120
```

- Generating list
```scheme
(let (((gen s n) (case n ((0 ()) (n (:: s (gen s (- n 1)))))))) (gen 'T_T 10))
; strict ==> (T_T T_T T_T T_T T_T T_T T_T T_T T_T T_T)
; lazy   ==> (:: T_T <thunk>)
```