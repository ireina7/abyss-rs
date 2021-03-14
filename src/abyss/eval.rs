use super::object::Object;
use super::object::Env;
use super::object::EvalError;
use std::fmt;


pub trait SExpr: fmt::Display + fmt::Debug + Clone {
    fn eval(&self, env: &mut Env) -> Result<Self, EvalError>;
}

impl SExpr for Object {
    fn eval(&self, env: &mut Env) -> Result<Self, EvalError> {
        use Object::*;
        match self {
            Nil => Ok(self.clone()),
            Var(s) => Ok(env.get(s).unwrap().clone()),
            Symbol(_) => Ok(self.clone()),
            Integer(_) => Ok(self.clone()),
            Real(_) => Ok(self.clone()),
            Str(_) => Ok(self.clone()),
            List(__) => Ok(Nil),
            _ => Ok(self.clone())
        }
    }
}
