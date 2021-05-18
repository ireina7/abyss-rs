//! The eval module to evaluate abyss expressions

use super::object::Object;
use super::object::Env;
use crate::logic::Unifiable;
use std::fmt;
//use super::parser;
//use std::rc::Rc;
use super::config::HashMap;
pub use super::object::EvalError;


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
        evaluate(self, &mut env)
    }
}


/// The default environment for evaluation
#[allow(dead_code)]
pub fn env() -> Env {
    //use Object::*;
    let env = Env::new();
    let f = |s: &str| s.parse::<Object>().unwrap().eval(&env).unwrap(); // You need to make sure no panic!
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


/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    let expr = force(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    let bind = pat.unify(&expr);
                    if let Ok(bind) = bind {
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
                    env.insert(s.clone(), v.clone());
                }
                _ => todo!() //unsupported yet!
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    Ok(())
}

/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated!
fn apply(f: Object, x: Object) -> Result<Object, EvalError> {
    use Object::*;
    match f {
        Closure(params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env;
            match *params {
                List(ps) => match &ps[..] {
                    [ ] => evaluate(&*expr, &mut env),
                    [pat] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            env.extend(bind);
                            evaluate(&*expr, &mut env)
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    },
                    [pat, ss @ ..] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            env.extend(bind);
                            Ok(Closure(Box::new(List(ss.to_vec())), expr, env))
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
        _ => Err(EvalError {msg: format!("")})
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

fn force(thunk: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    let thunk = thunk.clone();
    match thunk {
        Thunk(expr, mut env) => evaluate(&expr, &mut env),
        Var(s) => {
            let v = env.get(&s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) })?;
            match v {
                Thunk(expr, mut env) => {
                    //println!("{:?}", env);
                    let xv = evaluate(&expr, &mut env)?;
                    env.insert(s.clone(), xv.clone());
                    Ok(xv)
                },
                _ => Ok(v)
            }
        }
        _ => evaluate(&thunk, env)
    }
}

fn wrap(expr: Object, env: Env) -> Object {
    Object::Thunk(Box::new(expr), env)
}




/// The main evaluate function to calculate all abyss expressions
fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    //println!("eval: {}", expr);
    match expr {
        Nil         => Ok(Nil),
        Var(s) if is_atom(s) => Ok(expr.clone()),
        Var(_)      => force(expr, env),//env.get(s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr.clone()),
        Integer(_)  => Ok(expr.clone()),
        Real(_)     => Ok(expr.clone()),
        Str(_)      => Ok(expr.clone()),
        Thunk(_, _) => Ok(expr.clone()),
        List(xs)    => match &xs[..] {
            // Empty list
            [] => Ok(expr.clone()),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Force evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                Ok(wrap(x.clone(), env.clone()))
            }
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(expr, env),

            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
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
                    let xv = evaluate(x, env)?;
                    //let xv = wrap(x.clone(), env.clone());
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    }
}

