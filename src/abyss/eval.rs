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
    fn eval(&self, env: &Env) -> Result<Output, EvalError>;
}

impl Eval<Object> for Object {
    fn eval(&self, env: &Env) -> Result<Object, EvalError> {
        evaluate(self, env)
    }
}




/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}


/// Evaluate arithmetic expressions.
fn eval_arith(expr: &Object, env: &Env) -> Result<Object, EvalError> {
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
                match (x, y) {
                    (Integer(x), Integer(y)) => Ok(Integer((binary_integer[&op[..]])(x, y))),
                    (Real(x), Real(y)) => Ok(Real((binary_real[&op[..]])(x, y))),
                    _ => Err(EvalError { msg: format!("Arith error") })
                }
            },
            _ => Err(EvalError { msg: format!("eval arithmetic error!") })
        },
        _ => Err(EvalError { msg: format!("eval arithmetic error!") })
    }
}


/// Evaluate arithmetic expressions.
fn eval_if(cond: &Object, x: &Object, y: &Object, env: &Env) -> Result<Object, EvalError> {
    use Object::*;

    let cond = evaluate(cond, env)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}


/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &Env) -> Result<Object, EvalError> {
    use Object::*;
    let expr = evaluate(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    let bind = pat.unify(&expr);
                    if let Ok(bind) = bind {
                        let mut env = env.clone();
                        env.extend(bind);
                        return evaluate(res, &env)
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
fn bindings(bindings: &[Object], env: &Env) -> Result<Env, EvalError> {
    use Object::*;
    let mut env = env.clone();
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [Var(s), expr] => {
                    let v = evaluate(expr, &env)?;
                    env.insert(s.clone(), v);
                }
                _ => todo!() //unsupported yet!
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    Ok(env)
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
                    [ ] => evaluate(&*expr, &env),
                    [Var(s)] => {
                        env.insert(s.clone(), x);
                        evaluate(&*expr, &env)
                    },
                    [Var(s), ss @ ..] => {
                        env.insert(s.clone(), x);
                        Ok(Closure(Box::new(List(ss.to_vec())), expr, env))
                    }
                    _ => todo!()
                },
                _ => todo!()
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}


fn is_atom(s: &str) -> bool {
    let atoms = ["True", "False"];
    atoms.iter().any(|&x| x == s)
}





/// The main evaluate function to calculate all abyss expressions
fn evaluate(expr: &Object, env: &Env) -> Result<Object, EvalError> {
    use Object::*;

    match expr {
        Nil        => Ok(Nil),
        Var(s) if is_atom(s) => Ok(expr.clone()),
        Var(s)     => env.get(s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)  => Ok(expr.clone()),
        Integer(_) => Ok(expr.clone()),
        Real(_)    => Ok(expr.clone()),
        Str(_)     => Ok(expr.clone()),
        List(xs)   => match &xs[..] {
            // Empty list
            [] => Ok(Nil),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(expr, env),

            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                let env = bindings(bs, env)?;
                evaluate(expr, &env)
            },

            // Case (Match) expression
            [Var(op), expr, List(cases)] if &op[..] == "case" => eval_cases(expr, cases, env),

            // Normal function application
            [f, xs @ .. ] => {
                let mut fv = evaluate(f, env)?;
                for x in xs {
                    let xv = evaluate(x, env)?;
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    }
}
