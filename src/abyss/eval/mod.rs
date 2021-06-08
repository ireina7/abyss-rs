pub mod core;
pub mod strict;
pub mod lazy;
pub mod decl;

pub use super::object::{
    Object, Env, EvalError
};
pub use self::core::*;
//use std::rc::Rc;
//use lazy as interpreter;
use lazy as interpreter;


impl Eval<Object> for Object {
    type Error = EvalError;
    fn eval(&self, env: &Env) -> std::result::Result<Object, EvalError> {
        let mut env = env.clone();
        let expr = self.clone();
        interpreter::evaluate(expr, &mut env)
    }
}


pub fn bind(left: Object, right: Object, env: &mut Env) -> Result<()> {
    interpreter::bind(left, right, env)
}

