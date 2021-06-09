# abyss-rs
Rust implementation of Abyss programming language

![Made in abyss](./img/made-in-abyss.jpg)

## Getting started
- Download or compile source code.
- Run the program and enter repl automatically (default lazy mode).

### REPL functions
- `quit`: Quit repl
- `help`: Get help
- `eager`: Turn on eager mode (where strict evaluation is the default)
- `lazy`: Turn on lazy mode (where lazy evaluation is the default)
- `debug`: Turn on debug mode

### Expressions
- *Function application*: `(f x)`
- *Let bindings*: `(let ((x value) (y value) ...) expression)`
- *Lambda abstraction*: `(lambda (x y z ...) expression)`
- *If branches*: `(if expression true-branch false-branch)`
- *Pattern matching*: `(case expression ((case1 value) (case2 value) ...))`

### Declarations
- *Definition*: `(define x expression)` | `(define (f x y ...) expression)`
- *Data abstraction*: `(data x)` | `(data (cons x y ..))`

### Environment
- Integers
- Symbols
- Strings
- Real numbers
- `::`: List constructor
- `+-*/`: Arithmetic operators


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