pub mod core;
pub mod strict;
pub mod lazy;
pub mod decl;

pub use super::object::{
    Object, Env, EvalError
};
pub use self::core::*;
use crate::utils::error::Backtrace;
use lazy as interpreter;
//use strict as interpreter;


impl Eval<Object> for Object {
    type Error = EvalError;
    fn eval(&self, env: &Env) -> std::result::Result<Object, EvalError> {
        let mut env = env.clone();
        let mut log = Backtrace::new();
        let result = interpreter::evaluate(self.clone(), &mut env, &mut log);
        //println!("{}", log);
        match result {
            Ok(res) => Ok(res),
            Err(err) => Err(err.log(log))
        }
    }
}


pub fn bind(left: Object, right: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<()> {
    interpreter::bind(left, right, env, backtrace)
}

