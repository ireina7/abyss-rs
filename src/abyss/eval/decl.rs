//! Declaration of Abyss language

use super::{Object, Result, Env, bind};
//use std::rc::Rc;


pub fn is_decl(obj: &Object) -> bool {
    use Object::*;
    match obj {
        List(xs) => match &xs[..] {
            [Var(op), _, _] if op == "define" => true,
            _ => false
        }
        _ => false
    }
}

pub fn eval_decl(decl: &Object, env: &mut Env) -> Result<()> {
    use Object::*;
    match decl {
        List(xs) => match &xs[..] {
            [Var(op), def, expr] if op == "define" => bind(def, expr, env)?,
            _ => todo!()
        }
        _ => todo!()
    }
    Ok(())
}