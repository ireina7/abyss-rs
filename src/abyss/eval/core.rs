use std::fmt;
use crate::abyss;
use abyss::object::{Object, Env, EvalError};
use std::rc::Rc;

/// The main Eval trait, should be able to display and debug (Clone to store in Env)
/// 
/// The generic type `Output` is exposed in order to evaluate abyss expression to various
/// target output in the future.
pub trait Eval<Output>: fmt::Display + fmt::Debug + Clone {
    type Error;
    fn eval(&self, env: &Env) -> Result<Output, Self::Error>;
}


/// The default environment for evaluation
#[allow(dead_code)]
pub fn env() -> Env {
    //use Object::*;
    let env = Env::new();
    let f = |s: &str| Rc::new(s.parse::<Object>().unwrap().eval(&env).unwrap()); // You need to make sure no panic!
    let env = vec![
        ("fix", "(lambda (f) ((lambda (x) (f (lambda (v) (x x v)))) (lambda (x) (f (lambda (v) (x x v))))))"),
        //("fix", "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"),
        ("lazy", "(lambda (x) (lazy x))"),
        ("+", "(lambda (x y) (+ x y))"),
        ("-", "(lambda (x y) (- x y))"),
        ("*", "(lambda (x y) (* x y))"),
        ("/", "(lambda (x y) (/ x y))"),
        ("cons", "(lambda (x xs) (cons x xs))"),
        ("head", "(lambda (xs) (head xs))"),
        ("tail", "(lambda (xs) (tail xs))"),
    ];
    Env::new_from(env.into_iter().map(|(str, src)| (str.to_string(), f(src))).collect())
}


pub fn wrap(name: Option<String>, expr: Object, env: Env) -> Result<Object, EvalError> {
    use Object::*;
    match expr {
        Nil         => Ok(Nil),
        Var(s)      => env.get(&s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr),
        Integer(_)  => Ok(expr),
        Real(_)     => Ok(expr),
        Str(_)      => Ok(expr),
        Thunk(_, _, _) => Ok(expr),
        _ => Ok(Object::Thunk(name, Box::new(expr), env))
    }
}

