//! The type checker!

use super::object::Object;
use super::object::Env;
use super::eval::Eval;
use std::rc::Rc;



#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub struct Type {
    val: Object
}

#[derive(Debug, PartialEq)]
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

pub fn env() -> Env {
    let f = |s: &str| Rc::new(s.parse::<Object>().unwrap()); // You need to make sure no panic!
    let env = vec![
        //("fix", "(lambda (f) ((lambda (x) (f (lambda (v) (x x v)))) (lambda (x) (f (lambda (v) (x x v))))))"),
        //("fix", "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"),
        //("lazy", "(lambda (x) (lazy x))"),
        //("!", "(lambda (x) (! x))"),
        ("+",  "(-> Int Int Int)"),
        ("-",  "(-> Int Int Int)"),
        ("*",  "(-> Int Int Int)"),
        ("/",  "(-> Int Int Int)"),
        ("<",  "(-> Int Int Bool)"),
        (">",  "(-> Int Int Bool)"),
        ("==", "(-> Int Int Bool)"),
        ("/=", "(-> Int Int Bool)"),
        ("<=", "(-> Int Int Bool)"),
        (">=", "(-> Int Int Bool)"),
        //("::", "(-> Int Int Bool)"),
        //("head", "(lambda (xs) (head xs))"),
        //("tail", "(lambda (xs) (tail xs))"),
    ];
    Env::new_from(env.into_iter().map(|(str, src)| (str.to_string(), f(src))).collect())
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

            [f, xs @ ..] => {
                let ft = check(f.clone(), env, tnv)?;
                let mut xt = vec![];
                for x in xs {
                    xt.push(check(x.clone(), env, tnv)?);
                }
                if let List(ts) = ft {
                    for (ht, xt) in ts.into_iter().zip(xt.into_iter()) {
                        if ht == xt { continue }
                        else { return Err(CheckerError { msg: format!("Type error, expect: {}, found: {}", ht, xt) }) }
                    }
                }
                todo!()
            }

            //_ => todo!()
        }
        _ => Err(CheckerError { msg: format!("Unknow expression: {:?}", expr) })
    }
}



