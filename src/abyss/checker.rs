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
        Ok(Type { val: check(self.clone(), &mut env, &mut tnv)? })
    }
}


#[inline]
fn arrow(types: Vec<Object>) -> Object {
    use Object::*;
    List(vec![Cons("->".into())].into_iter().chain(types.into_iter()).collect())
}

fn check(expr: Object, env: &mut Env, tnv: &mut Env) -> Result<Object, CheckerError> {
    use Object::*;
    //println!("eval: {}", expr);
    #[inline] fn tag(s: &str) -> Object {
        Var(s.to_string())
    }
    match expr {
        Nil         => Ok(Nil),
        Var(ref s)  => tnv.get(s).map(|x| (**x).clone()).ok_or(CheckerError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(tag("Symbol")),
        Integer(_)  => Ok(tag("Int")),
        Real(_)     => Ok(tag("Real")),
        Str(_)      => Ok(tag("String")),
        Thunk(_, expr, env) => {
            let mut env = env.clone();
            check(expr.value(), &mut env, tnv)
        },
        List(xs)    => match &xs[..] {

            // Empty list
            [] => Ok(List(vec![tag("Unit")])),

            // Lambda abstraction
            [Var(op), List(ps), expr] if &op[..] == "lambda" => {
                let mut pts = vec![];
                for p in ps {
                    let pt = check(p.clone(), env, tnv)?;
                    pts.push(pt);
                }
                let et = check(expr.clone(), env, tnv)?;
                pts.push(et);
                Ok(arrow(pts))
            },

            _ => todo!()
        }
        _ => Err(CheckerError { msg: format!("Unknow expression: {:?}", expr) })
    }
}



