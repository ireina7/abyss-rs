//! The eval module to evaluate abyss expressions

use super::object::Object;
use super::object::Env;
use super::object::EvalError;
use std::fmt;
use super::config::HashMap;



/// The main Eval trait, should be able to display and debug (Clone to store in Env)
/// 
/// The generic type `Output` is exposed in order to evaluate abyss expression to various
/// target output in the future.
pub trait Eval<Output>: fmt::Display + fmt::Debug + Clone {
    fn eval(&self, env: &mut Env) -> Result<Output, EvalError>;
}

impl Eval<Object> for Object {
    fn eval(&self, env: &mut Env) -> Result<Object, EvalError> {
        evaluate(self, env)
    }
}

/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}


/// Evaluate arithmetic expressions.
/// Real numbers has not been supported yet!
fn eval_arith(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    
    let binary_integer: Vec<(&str, Box<dyn Fn(i32, i32) -> i32>)> = vec![
        ("+", Box::new(move |a, b| a + b)),
        ("-", Box::new(move |a, b| a - b)),
        ("*", Box::new(move |a, b| a * b)),
        ("/", Box::new(move |a, b| a / b)),
    ];
    let binary_integer: HashMap<&str, Box<dyn Fn(i32, i32) -> i32>> = binary_integer.into_iter().collect();
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), x, y] if is_arith(&op) => {
                let x = evaluate(x, env)?;
                let y = evaluate(y, env)?;
                if let (Integer(x), Integer(y)) = (x, y) {
                    Ok(Integer((binary_integer[&op[..]])(x, y)))
                } else {
                    Err(EvalError { msg: format!("Arith error") })
                }
            },
            _ => Err(EvalError { msg: format!("eval arithmetic error!") })
        },
        _ => Err(EvalError { msg: format!("eval arithmetic error!") })
    }
}

/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<(), EvalError> {
    for binding in bindings {
        match binding {
            Object::List(xs) => match &xs[..] {
                _ => todo!()
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    Ok(())
}


/// The main evaluate function to calculate all abyss expressions
fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;

    match expr {
        Nil        => Ok(Nil),
        Var(s)     => env.get(s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)  => Ok(expr.clone()),
        Integer(_) => Ok(expr.clone()),
        Real(_)    => Ok(expr.clone()),
        Str(_)     => Ok(expr.clone()),
        List(xs)   => match &xs[..] {
            // Empty list
            [] => Ok(Nil),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => 
                Ok(Closure(Box::new(ps.clone()), Box::new(expr.clone()), env.clone())),
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(expr, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
                evaluate(expr, env)
            },

            // Normal function application
            _ => {
                Ok(Nil)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    }
}
