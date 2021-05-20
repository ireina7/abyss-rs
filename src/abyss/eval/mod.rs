pub mod core;
pub mod strict;
pub mod lazy;

pub use super::object::{
    Object, Env, EvalError
};
pub use self::core::*;
//use std::rc::Rc;


impl Eval<Object> for Object {
    type Error = EvalError;
    fn eval(&self, env: &Env) -> std::result::Result<Object, EvalError> {
        let mut env = env.clone();
        lazy::evaluate(self, &mut env)
    }
}
