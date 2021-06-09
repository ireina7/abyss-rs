//! Declaration of Abyss language

use crate::utils::error::Backtrace;
use super::{ Object, Result, Env, bind, EvalError };
//use std::rc::Rc;


pub fn is_decl(obj: &Object) -> bool {
    use Object::*;
    match obj {
        List(xs) => match &xs[..] {
            [Var(op), _, _] if op == "define" => true,
            [Var(op), _, _] if op == "data" => todo!(),
            _ => false
        }
        _ => false
    }
}

pub fn eval_decl(decl: &Object, env: &mut Env) -> Result<()> {
    use Object::*;
    let mut log = Backtrace::new();
    match decl {
        List(xs) => match &xs[..] {
            [Var(op), def, expr] if op == "define" => bind(def.clone(), expr.clone(), env, &mut log),
            [Var(op), _, _] if op == "data" => todo!(),
            _ => Err(EvalError::msg(format!("Evaluation error: wrong format of declarations: {:?}", decl)))
        }
        _ => Err(EvalError::msg(format!("Evaluation error: wrong format of declarations: {:?}", decl)))
    }
}