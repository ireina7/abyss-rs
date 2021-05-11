use super::object::Object;
use super::object::Env;
use super::object::EvalError;
use std::fmt;
use super::config::HashMap;


pub trait Eval: fmt::Display + fmt::Debug + Clone {
    fn eval(&self, env: &mut Env) -> Result<Self, EvalError>;
}

impl Eval for Object {
    fn eval(&self, env: &mut Env) -> Result<Self, EvalError> {
        evaluate(self, env)
    }
}

fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}



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


fn bindings(bs: &[Object], env: &mut Env) -> Result<(), EvalError> {
    for binding in bs {
        match binding {
            Object::List(xs) => match &xs[..] {
                _ => todo!()
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    Ok(())
}


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
            [] => Ok(Nil),
            [Var(op), ps, expr] if &op[..] == "lambda" => Ok(Closure(Box::new(ps.clone()), Box::new(expr.clone()), env.clone())),
            [Var(op), _, _] if is_arith(&op) => eval_arith(expr, env),
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
                evaluate(expr, env)
            },
            _ => {
                Ok(Nil)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    }
}
