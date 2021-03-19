use super::object::Object;
use super::object::Env;
use super::object::EvalError;
use std::fmt;


pub trait Eval: fmt::Display + fmt::Debug + Clone {
    fn eval(&self, env: &mut Env) -> Result<Self, EvalError>;
}

impl Eval for Object {
    fn eval(&self, env: &mut Env) -> Result<Self, EvalError> {
        evaluate(self, env)
    }
}


fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;

    match expr {
        Nil => Ok(Nil),
        Var(s) => env.get(s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_) => Ok(expr.clone()),
        Integer(_) => Ok(expr.clone()),
        Real(_) => Ok(expr.clone()),
        Str(_) => Ok(expr.clone()),
        List(_) => Ok(Nil),
        _ => Ok(expr.clone())
    }
}
