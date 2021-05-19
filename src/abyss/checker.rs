//! The type checker!

use super::object::Object;
use super::object::Env;
use super::eval::Eval;




#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub struct Type {
    val: Object
}

#[derive(Debug)]
pub struct CheckerError {
    msg: String
}

impl Eval<Type> for Object {
    type Error = CheckerError;
    fn eval(&self, env: &Env) -> Result<Type, CheckerError> {
        let mut env = env.clone();
        let mut tnv = Env::new();
        Ok(Type { val: check(self, &mut env, &mut tnv)? })
    }
}


fn check(expr: &Object, env: &mut Env, tnv: &mut Env) -> Result<Object, CheckerError> {
    use Object::*;
    //println!("eval: {}", expr);
    let tag = |s: &str| Var(s.to_string());
    match expr {
        Nil         => Ok(Nil),
        Var(s)      => tnv.get(s).map(|x| x.clone()).ok_or(CheckerError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(tag("Symbol")),
        Integer(_)  => Ok(tag("Int")),
        Real(_)     => Ok(tag("Real")),
        Str(_)      => Ok(tag("String")),
        Thunk(_, _, _) => Ok(expr.clone()),
        List(xs)    => match &xs[..] {

            // Empty list
            [] => Ok(List(vec![tag("_")])),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            _ => todo!()
        }
        _ => Err(CheckerError { msg: format!("Unknow expression: {:?}", expr) })
    }
}