//! The eval module to evaluate abyss expressions
#![allow(dead_code)]

use crate::abyss;
use abyss::object::Object;
use abyss::object::Env;
use crate::logic::Unifiable;
//use super::parser;
use std::rc::Rc;
use abyss::config::HashMap;
use abyss::eval::core::*;
pub use abyss::object::EvalError;

//(let ((fact (lambda (n) (case n ((0 1) (n (+ n (fact (- n 1))))))))) (fact 10000))




/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}


/// Evaluate arithmetic expressions.
fn eval_arith(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    
    let binary_integer: Vec<(&str, Box<dyn Fn(i64, i64) -> i64>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_integer: HashMap<&str, Box<dyn Fn(i64, i64) -> i64>> = binary_integer.into_iter().collect();

    let binary_real: Vec<(&str, Box<dyn Fn(f64, f64) -> f64>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_real: HashMap<&str, Box<dyn Fn(f64, f64) -> f64>> = binary_real.into_iter().collect();
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), x, y] if is_arith(&op) => {
                let x = evaluate(x, env)?;
                let y = evaluate(y, env)?;
                match (x, y) {
                    (Integer(x), Integer(y)) => Ok(Integer((binary_integer[&op[..]])(x, y))),
                    (Real(x), Real(y)) => Ok(Real((binary_real[&op[..]])(x, y))),
                    others => Err(EvalError { msg: format!("Arith error: evaluating {:?}", others) })
                }
            },
            _ => Err(EvalError { msg: format!("eval arithmetic error!") })
        },
        _ => Err(EvalError { msg: format!("eval arithmetic error!") })
    }
}


/// Evaluate arithmetic expressions.
fn eval_if(cond: &Object, x: &Object, y: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;

    let cond = evaluate(cond, env)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}



/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    //println!("\ncases");
    use Object::*;
    let expr = evaluate(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    let bind = pat.unify(&expr);
                    if let Ok(mut bind) = bind {
                        let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                        env.extend(bind);
                        return evaluate(res, env)
                    } else {
                        continue;
                    }
                },
                _ => return Err(EvalError { 
                    msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
                }),
            },
            _ => return Err(EvalError { 
                msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
            }),
        }
    }
    Err(EvalError { msg: format!("Case expression error") })
}


/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<(), EvalError> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [Var(s), expr] => {
                    let v = evaluate(expr, env)?;
                    //let v = wrap(expr.clone(), env.clone());
                    let v = match v {
                        Closure(None, ps, expr, env) => {
                            Closure(Some(s.clone()), ps, expr, env)
                        },
                        others => others,
                    };
                    env.insert(s.clone(), Rc::new(v.clone()));
                }
                _ => todo!() //unsupported yet!
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    //println!(">> {:?}", env);
    Ok(())
}
//(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen s (- n 1))))))))) (gen 'T_T 200))
/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated!
fn apply(f: Object, x: Object) -> Result<Object, EvalError> {
    use Object::*;
    
    match f {
        Closure(name, params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env;
            
            match *params {
                List(ps) => match &ps[..] {
                    [ ] => evaluate(&*expr, &mut env),
                    [pat] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
                            if let Some(name) = name {
                                env.insert(
                                    name.clone(), 
                                    Rc::new(Closure(Some(name), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            //println!("{:?}", ps);
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            evaluate(&*expr, &mut env)
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    },
                    [pat, ss @ ..] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
                            if let Some(name) = name.clone() {
                                env.insert(
                                    name.clone(),
                                    Rc::new(Closure(Some(name), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            Ok(Closure(None, Box::new(List(ss.to_vec())), expr, env))
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    }
                },
                _ => todo!()
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}

/// Check if variable is atom (normal form)
fn is_atom(s: &str) -> bool {
    let atoms = ["True", "False"];
    atoms.iter().any(|&x| x == s)
}

/// Handle cons expression
fn eval_cons(x: &Object, xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    let xs = evaluate(xs, env)?;
    match xs {
        //Object::Nil => Ok(Object::List(vec![evaluate(x, env)?])),
        Object::List(xs) => Ok(Object::List(vec![evaluate(x, env)?].into_iter().chain(xs.into_iter()).collect())),
        _ => Err(EvalError {msg: format!("Cons error: {:?}", xs)})
    }
}

/// Handle list constructions
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    let mut v = vec![];
    for x in xs {
        let x = evaluate(x, env)?;
        v.push(x);
    }
    Ok(Object::List(v))
}


fn eval_head(xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [x, ..] => Ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    //println!("{:?}", xs);
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [_, xs @ ..] => Ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}


fn eval_thunk(thunk: &Object) -> Result<Object, EvalError> {
    //println!("\nthunk: {:?}", thunk);
    use Object::*;
    match thunk.clone() {
        Thunk(None, expr, mut env) => {
            //println!("{:?}", env);
            evaluate(&expr, &mut env)
        },
        Thunk(Some(name), expr, mut env) => {
            env.insert(name.clone(), Rc::new(thunk.clone()));
            evaluate(&expr, &mut env)
        },
        _ => Err(EvalError { msg: format!("Error while evaluating thunk: {:?}", thunk) })
    }
}







/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    //println!("\neval: {} in {:?}", expr, env);
    let expr = expr.clone();
    let ans = match expr {
        Nil         => Ok(Nil),
        Var(s) if is_atom(&s) => Ok(Var(s)),
        Var(s)      => env.get(&s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr),
        Integer(_)  => Ok(expr),
        Real(_)     => Ok(expr),
        Str(_)      => Ok(expr),
        Thunk(_, _, _) => eval_thunk(&expr),
        List(ref xs)    => match &xs[..] {
            // Empty list
            [] => Ok(List(xs.clone())),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Force evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                wrap(None, x.clone(), env.clone())
            }
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(&expr, env),

            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
                //println!("{:?}", env);
                evaluate(expr, env)
            },

            // Case (Match) expression
            [Var(op), expr, List(cases)] if &op[..] == "case" => eval_cases(expr, cases, env),

            // List operations
            [Var(op), x, xs]   if &op[..] == "cons" => eval_cons(x, xs, env),
            [Var(op), xs]      if &op[..] == "head" => eval_head(xs, env),
            [Var(op), xs]      if &op[..] == "tail" => eval_tail(xs, env),
            [Var(op), xs @ ..] if &op[..] == "list" => eval_list(xs, env),

            // Normal function application
            [f, xs @ .. ] => {
                let mut fv = evaluate(f, env)?;
                for x in xs {
                    let xv = evaluate(x, env)?;
                    //let xv = wrap(None, x.clone(), env.clone())?;
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    };
    ans
}













/*
//! The eval module to evaluate abyss expressions

use super::object::Object;
use super::object::Env;
use crate::logic::Unifiable;
use std::fmt;
use std::borrow::Borrow;
use std::borrow::BorrowMut;
//use super::parser;
use std::rc::Rc;
use super::config::HashMap;
pub use super::object::EvalError;

//(let ((fact (lambda (n) (case n ((0 1) (n (+ n (fact (- n 1))))))))) (fact 10000))
/// The main Eval trait, should be able to display and debug (Clone to store in Env)
/// 
/// The generic type `Output` is exposed in order to evaluate abyss expression to various
/// target output in the future.
pub trait Eval<Output>: fmt::Display + fmt::Debug + Clone {
    type Error;
    fn eval(&self, env: &Env) -> Result<Output, Self::Error>;
}

impl Eval<Object> for Object {
    type Error = EvalError;
    fn eval(&self, env: &Env) -> Result<Object, EvalError> {
        let mut env = env.clone();
        strict::evaluate(self, &mut env).map(|rc| (*rc).clone())
    }
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


fn wrap(name: Option<String>, expr: Object, env: Env) -> Result<Object, EvalError> {
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












#[allow(dead_code)]
pub mod strict {

use super::*;

/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}

fn ok(x: Object) -> Result<Rc<Object>, EvalError> {
    Ok(Rc::new(x))
}

fn wrap(name: Option<String>, expr: Object, env: Env) -> Result<Rc<Object>, EvalError> {
    use Object::*;
    match expr {
        Nil         => ok(Nil),
        Var(s)      => env.get(&s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => ok(expr),
        Integer(_)  => ok(expr),
        Real(_)     => ok(expr),
        Str(_)      => ok(expr),
        Thunk(_, _, _) => ok(expr),
        _ => ok(Object::Thunk(name, Box::new(expr), env))
    }
}

/// Evaluate arithmetic expressions.
fn eval_arith(expr: &Object, env: &mut Env) -> Result<Rc<Object>, EvalError> {
    use Object::*;
    
    let binary_integer: Vec<(&str, Box<dyn Fn(i32, i32) -> i32>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_integer: HashMap<&str, Box<dyn Fn(i32, i32) -> i32>> = binary_integer.into_iter().collect();

    let binary_real: Vec<(&str, Box<dyn Fn(f64, f64) -> f64>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_real: HashMap<&str, Box<dyn Fn(f64, f64) -> f64>> = binary_real.into_iter().collect();
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), x, y] if is_arith(&op) => {
                let x = evaluate(x, env)?;
                let y = evaluate(y, env)?;
                match (x.borrow(), y.borrow()) {
                    (Integer(x), Integer(y)) => ok(Integer((binary_integer[&op[..]])(*x, *y))),
                    (Real(x), Real(y)) => ok(Real((binary_real[&op[..]])(*x, *y))),
                    others => Err(EvalError { msg: format!("Arith error: evaluating {:?}", others) })
                }
            },
            _ => Err(EvalError { msg: format!("eval arithmetic error!") })
        },
        _ => Err(EvalError { msg: format!("eval arithmetic error!") })
    }
}


/// Evaluate arithmetic expressions.
fn eval_if(cond: &Object, x: &Object, y: &Object, env: &mut Env) -> Result<Rc<Object>, EvalError> {
    use Object::*;

    let cond = evaluate(cond, env)?;
    match cond.borrow() {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}

//(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen (- n 1))))))))) (circle 'TT 7))

/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &mut Env) -> Result<Rc<Object>, EvalError> {
    //println!("\ncases");
    use Object::*;
    let expr = evaluate(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    let bind = pat.unify(&expr);
                    if let Ok(mut bind) = bind {
                        let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                        env.extend(bind);
                        return evaluate(res, env)
                    } else {
                        continue;
                    }
                },
                _ => return Err(EvalError { 
                    msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
                }),
            },
            _ => return Err(EvalError { 
                msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
            }),
        }
    }
    Err(EvalError { msg: format!("Case expression error") })
}


/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<(), EvalError> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [Var(s), expr] => {
                    let v = evaluate(expr, env)?;
                    //let v = wrap(expr.clone(), env.clone());
                    let v = match v.borrow() {
                        Closure(None, ps, expr, env) => {
                            Closure(Some(s.clone()), ps.clone(), expr.clone(), env.clone())
                        },
                        others => others.clone(),
                    };
                    env.insert(s.clone(), Rc::new(v.clone()));
                }
                _ => todo!() //unsupported yet!
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    //println!(">> {:?}", env);
    Ok(())
}
//(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen s (- n 1))))))))) (gen 'T_T 200))
/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated!
fn apply(f: Rc<Object>, x: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    use Object::*;
    
    match f.borrow() {
        Closure(name, params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env.clone();
            
            match params.borrow() {
                List(ps) => match &ps[..] {
                    [ ] => evaluate(&*expr, &mut env),
                    [pat] => {
                        let bind = pat.unify(&x);
                        if let Ok(mut bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                            if let Some(name) = name {
                                env.insert(
                                    name.clone(), 
                                    Rc::new(Closure(Some(name.clone()), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            //println!("{:?}", ps);
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            evaluate(&*expr, &mut env)
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    },
                    [pat, ss @ ..] => {
                        let bind = pat.unify(&x);
                        if let Ok(mut bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                            if let Some(name) = name.clone() {
                                env.insert(
                                    name.clone(),
                                    Rc::new(Closure(Some(name.clone()), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            ok(Closure(None, Box::new(List(ss.to_vec())), expr.clone(), env.clone()))
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    }
                },
                _ => todo!()
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}

/// Check if variable is atom (normal form)
fn is_atom(s: &str) -> bool {
    let atoms = ["True", "False"];
    atoms.iter().any(|&x| x == s)
}

/// Handle cons expression
fn eval_cons(x: &Object, xs: &Object, env: &mut Env) -> Result<Rc<Object>, EvalError> {
    let xs = evaluate(xs, env)?;
    match &*xs {
        //Object::Nil => Ok(Object::List(vec![evaluate(x, env)?])),
        Object::List(xs) => {
            let xv = evaluate(x, env)?;
            ok(Object::List(vec![(*xv).clone()].into_iter().chain((*xs).clone().into_iter()).collect()))
        },
        _ => Err(EvalError {msg: format!("Cons error: {:?}", xs)})
    }
}

/// Handle list constructions
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Rc<Object>, EvalError> {
    let mut v = vec![];
    for x in xs {
        let x = evaluate(x, env)?;
        v.push((*x).clone());
    }
    ok(Object::List(v))
}


fn eval_head(xs: &Object, env: &mut Env) -> Result<Rc<Object>, EvalError> {
    match evaluate(xs, env)?.borrow() {
        Object::List(xs) => match &xs[..] {
            [x, ..] => ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Rc<Object>, EvalError> {
    //println!("{:?}", xs);
    match evaluate(xs, env)?.borrow() {
        Object::List(xs) => match &xs[..] {
            [_, xs @ ..] => ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}


fn eval_thunk(thunk: &Object) -> Result<Rc<Object>, EvalError> {
    //println!("\nthunk: {:?}", thunk);
    use Object::*;
    match thunk.clone() {
        Thunk(None, expr, mut env) => {
            //println!("{:?}", env);
            evaluate(&expr, &mut env)
        },
        Thunk(Some(name), expr, mut env) => {
            env.insert(name.clone(), Rc::new(thunk.clone()));
            evaluate(&expr, &mut env)
        },
        _ => Err(EvalError { msg: format!("Error while evaluating thunk: {:?}", thunk) })
    }
}







/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: &Object, env: &mut Env) -> Result<Rc<Object>, EvalError> {
    use Object::*;
    let ok = |x| Ok(Rc::new(x));
    //println!("\neval: {} in {:?}", expr, env);
    let expr = expr.clone();
    let ans: Result<Rc<Object>, EvalError> = match expr {
        Nil         => ok(Nil),
        Var(s) if is_atom(&s) => ok(Var(s)),
        Var(s)      => env.get(&s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => ok(expr),
        Integer(_)  => ok(expr),
        Real(_)     => ok(expr),
        Str(_)      => ok(expr),
        Thunk(_, _, _) => eval_thunk(&expr),
        List(ref xs)    => match &xs[..] {
            // Empty list
            [] => ok(List(xs.to_vec())),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Force evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                wrap(None, x.clone(), env.clone())
            }
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(&expr, env),

            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
                //println!("{:?}", env);
                evaluate(expr, env)
            },

            // Case (Match) expression
            [Var(op), expr, List(cases)] if &op[..] == "case" => eval_cases(expr, cases, env),

            // List operations
            [Var(op), x, xs]   if &op[..] == "cons" => eval_cons(x, xs, env),
            [Var(op), xs]      if &op[..] == "head" => eval_head(xs, env),
            [Var(op), xs]      if &op[..] == "tail" => eval_tail(xs, env),
            [Var(op), xs @ ..] if &op[..] == "list" => eval_list(xs, env),

            // Normal function application
            [f, xs @ .. ] => {
                let mut fv = evaluate(f, env)?;
                for x in xs {
                    let xv = evaluate(x, env)?;
                    //let xv = wrap(None, x.clone(), env.clone())?;
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    };
    ans
}

}//end mod strict













#[allow(dead_code)]
pub mod lazy {

use super::*;

/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}


/// Evaluate arithmetic expressions.
fn eval_arith(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    
    let binary_integer: Vec<(&str, Box<dyn Fn(i32, i32) -> i32>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_integer: HashMap<&str, Box<dyn Fn(i32, i32) -> i32>> = binary_integer.into_iter().collect();

    let binary_real: Vec<(&str, Box<dyn Fn(f64, f64) -> f64>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_real: HashMap<&str, Box<dyn Fn(f64, f64) -> f64>> = binary_real.into_iter().collect();
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), x, y] if is_arith(&op) => {
                let x = force(x, env)?;
                let y = force(y, env)?;
                match (x, y) {
                    (Integer(x), Integer(y)) => Ok(Integer((binary_integer[&op[..]])(x, y))),
                    (Real(x), Real(y)) => Ok(Real((binary_real[&op[..]])(x, y))),
                    others => Err(EvalError { msg: format!("Arith error: evaluating {:?}", others) })
                }
            },
            _ => Err(EvalError { msg: format!("eval arithmetic error!") })
        },
        _ => Err(EvalError { msg: format!("eval arithmetic error!") })
    }
}


/// Evaluate arithmetic expressions.
fn eval_if(cond: &Object, x: &Object, y: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;

    let cond = force(cond, env)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}

//(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen (- n 1))))))))) (circle 'TT 7))

/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    //println!("\ncases");
    use Object::*;
    let expr = force(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    let bind = pat.unify(&expr);
                    if let Ok(mut bind) = bind {
                        let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                        env.extend(bind);
                        return evaluate(res, env)
                    } else {
                        continue;
                    }
                },
                _ => return Err(EvalError { 
                    msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
                }),
            },
            _ => return Err(EvalError { 
                msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
            }),
        }
    }
    Err(EvalError { msg: format!("Case expression error") })
}


/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<(), EvalError> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [Var(s), expr] => {
                    let v = match expr {
                        List(xs) => match &xs[..] {
                            [Var(op), ..] if &op[..] == "lambda" => evaluate(expr, env)?,
                            others => List(others.to_vec())
                        }
                        others => others.clone()
                    };
                    //let v = wrap(expr.clone(), env.clone());
                    let v = match v {
                        Closure(None, ps, expr, env) => {
                            Closure(Some(s.clone()), ps, expr, env)
                        },
                        others => {
                            let thunk = wrap(Some(s.clone()), others.clone(), env.clone())?;
                            //env.insert(s.clone(), thunk.clone());
                            thunk
                        }
                    };
                    env.insert(s.clone(), Rc::new(v.clone()));
                }
                _ => todo!() //unsupported yet!
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    //println!(">> {:?}", env);
    Ok(())
}
//(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen s (- n 1))))))))) (gen 'T_T 200))
/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated!
fn apply(f: Object, x: Object) -> Result<Object, EvalError> {
    use Object::*;
    
    match f {
        Closure(name, params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env;
            
            match *params {
                List(ps) => match &ps[..] {
                    [ ] => evaluate(&*expr, &mut env),
                    [pat] => {
                        let bind = pat.unify(&x);
                        if let Ok(mut bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                            if let Some(name) = name {
                                env.insert(
                                    name.clone(), 
                                    Rc::new(Closure(Some(name.clone()), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            //println!("{:?}", ps);
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            evaluate(&*expr, &mut env)
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    },
                    [pat, ss @ ..] => {
                        let bind = pat.unify(&x);
                        if let Ok(mut bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                            if let Some(name) = name.clone() {
                                env.insert(
                                    name.clone(),
                                    Rc::new(Closure(Some(name.clone()), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            Ok(Closure(None, Box::new(List(ss.to_vec())), expr, env))
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    }
                },
                _ => todo!()
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}

/// Check if variable is atom (normal form)
fn is_atom(s: &str) -> bool {
    let atoms = ["True", "False"];
    atoms.iter().any(|&x| x == s)
}

/// Handle cons expression
fn eval_cons(x: &Object, xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    let xs = evaluate(xs, env)?;
    match xs {
        //Object::Nil => Ok(Object::List(vec![evaluate(x, env)?])),
        Object::List(xs) => Ok(Object::List(vec![evaluate(x, env)?].into_iter().chain(xs.into_iter()).collect())),
        _ => Err(EvalError {msg: format!("Cons error: {:?}", xs)})
    }
}

/// Handle list constructions
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    let mut v = vec![];
    for x in xs {
        let x = evaluate(x, env)?;
        v.push(x);
    }
    Ok(Object::List(v))
}


fn eval_head(xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [x, ..] => Ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    //println!("{:?}", xs);
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [_, xs @ ..] => Ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}


fn eval_thunk(thunk: &Object) -> Result<Object, EvalError> {
    //println!("\nthunk: {:?}", thunk);
    use Object::*;
    match thunk.clone() {
        Thunk(None, expr, mut env) => {
            //println!("{:?}", env);
            evaluate(&expr, &mut env)
        },
        Thunk(Some(name), expr, mut env) => {
            env.insert(name.clone(), Rc::new(thunk.clone()));
            evaluate(&expr, &mut env)
        },
        _ => Err(EvalError { msg: format!("Error while evaluating thunk: {:?}", thunk) })
    }
}


fn force(thunk: &Object, env: &mut Env) -> Result<Object, EvalError> {
    //println!("\nforce: {} in {:?}", thunk, env);
    use Object::*;
    let thunk_ = thunk.clone();
    //println!("\n{:?} ==> {:?}", thunk, env);
    match thunk_ {
        Thunk(_, _, _) => eval_thunk(thunk),
        Var(s) => {
            let v = env.get(&s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) })?;
            match &v {
                Thunk(_, _, _) => eval_thunk(&v),
                _ => Ok(v)
            }
        }
        _ => evaluate(&thunk, env)
    }
}




/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    //println!("\neval: {} in {:?}", expr, env);
    let ans = match expr {
        Nil         => Ok(Nil),
        Var(s) if is_atom(s) => Ok(expr.clone()),
        Var(_)      => force(expr, env),//env.get(s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr.clone()),
        Integer(_)  => Ok(expr.clone()),
        Real(_)     => Ok(expr.clone()),
        Str(_)      => Ok(expr.clone()),
        Thunk(_, _, _) => Ok(expr.clone()),
        List(xs)    => match &xs[..] {
            // Empty list
            [] => Ok(expr.clone()),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Force evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                wrap(None, x.clone(), env.clone())
            }
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(expr, env),

            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
                //println!("{:?}", env);
                evaluate(expr, env)
            },

            // Case (Match) expression
            [Var(op), expr, List(cases)] if &op[..] == "case" => eval_cases(expr, cases, env),

            // List operations
            [Var(op), x, xs]   if &op[..] == "cons" => eval_cons(x, xs, env),
            [Var(op), xs]      if &op[..] == "head" => eval_head(xs, env),
            [Var(op), xs]      if &op[..] == "tail" => eval_tail(xs, env),
            [Var(op), xs @ ..] if &op[..] == "list" => eval_list(xs, env),

            // Normal function application
            [f, xs @ .. ] => {
                let mut fv = force(f, env)?;
                for x in xs {
                    //let xv = evaluate(x, env)?;
                    let xv = wrap(None, x.clone(), env.clone())?;
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    };
    ans
}

}//end mod lazy




*/