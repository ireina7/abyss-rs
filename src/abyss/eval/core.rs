use std::fmt;
use crate::abyss;
use abyss::object::{Object, Env, EvalError};
use std::rc::Rc;
use std::ops::Deref;


pub type Result<T> = std::result::Result<T, EvalError>;

/// The main Eval trait, should be able to display and debug (Clone to store in Env)
/// 
/// The generic type `Output` is exposed in order to evaluate abyss expression to various
/// target output in the future.
pub trait Eval<Output>: fmt::Display + fmt::Debug + Clone {
    type Error;
    fn eval(&self, env: &Env) -> std::result::Result<Output, Self::Error>;
}

/// The default environment for evaluation
#[allow(dead_code)]
pub fn env() -> Env {
    //use Object::*;
    let env = Env::new();
    let f = |s: &str| Rc::new(s.parse::<Object>().unwrap().eval(&env).unwrap()); // You need to make sure no panic!
    let env = vec![
        ("fix", "(lambda (f) ((lambda (x) (f (lambda (v) (x x v)))) (lambda (x) (f (lambda (v) (x x v))))))"),
        //("fix", "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"),
        ("lazy", "(lambda (x) (lazy x))"),
        ("!", "(lambda (x) (! x))"),
        ("+", "(lambda (x y) (+ x y))"),
        ("-", "(lambda (x y) (- x y))"),
        ("*", "(lambda (x y) (* x y))"),
        ("/", "(lambda (x y) (/ x y))"),
        ("<", "(lambda (x y) (< x y))"),
        (">", "(lambda (x y) (> x y))"),
        ("==", "(lambda (x y) (== x y))"),
        ("/=", "(lambda (x y) (/= x y))"),
        ("<=", "(lambda (x y) (<= x y))"),
        (">=", "(lambda (x y) (>= x y))"),
        ("cons", "(lambda (x xs) (cons x xs))"),
        ("head", "(lambda (xs) (head xs))"),
        ("tail", "(lambda (xs) (tail xs))"),
    ];
    Env::new_from(env.into_iter().map(|(str, src)| (str.to_string(), f(src))).collect())
}


pub fn wrap(name: Option<String>, expr: Object, env: Env) -> Result<Object> {
    use Object::*;
    match expr {
        Nil         => Ok(Nil),
        Var(s)      => env.get(&s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr),
        Cons(_)     => Ok(expr),
        Integer(_)  => Ok(expr),
        Real(_)     => Ok(expr),
        Str(_)      => Ok(expr),
        Thunk(_, _, _) => Ok(expr),
        _ => Ok(Object::Thunk(name, Rc::new(expr), env))
    }
}

/// Weak a term, i.e. term => lambda ps. term
#[inline]
pub fn weak(ps: Vec<Object>, expr: Object) -> Object {
    use Object::*;
    List(vec![Var("lambda".into()), List(ps), expr])
}


#[derive(Debug, PartialEq, Eq)]
struct Pattern {
    expr: Object
}

impl From<Object> for Pattern {
    fn from(obj: Object) -> Self {
        use Object::*;
        fn pack(obj: Object) -> Pattern {
            Pattern { expr: obj }
        }
        match obj {
            Nil            => pack(Nil),
            Var(_)         => pack(obj),
            Symbol(_)      => pack(obj),
            Cons(_)        => pack(obj),
            Integer(_)     => pack(obj),
            Real(_)        => pack(obj),
            Str(_)         => pack(obj),
            Thunk(_, _, _) => pack(obj),
            List(xs) => match &xs[..] {
                [Var(_op), _xs @ ..] => todo!(),
                _ => todo!()
            }
            _ => todo!()
        }
    }
}

impl Deref for Pattern {
    type Target = Object;
    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}






/// Module for all atom evaluations
/// Including:
/// + - * / == /= < > <= >=
pub mod atom {

    use super::*;
    use std::collections::HashMap;

    fn add_i(a: i64, b: i64) -> i64 { a + b }
    fn sub_i(a: i64, b: i64) -> i64 { a - b }
    fn mul_i(a: i64, b: i64) -> i64 { a * b }
    fn div_i(a: i64, b: i64) -> i64 { a / b }

    pub const BINARY_ARITH_INTEGER: [(&str, fn(i64, i64) -> i64); 4] = [
        ("+", add_i),
        ("-", sub_i),
        ("*", mul_i),
        ("/", div_i),
    ];

    fn add_f(a: f64, b: f64) -> f64 { a + b }
    fn sub_f(a: f64, b: f64) -> f64 { a - b }
    fn mul_f(a: f64, b: f64) -> f64 { a * b }
    fn div_f(a: f64, b: f64) -> f64 { a / b }

    pub const BINARY_ARITH_REAL: [(&str, fn(f64, f64) -> f64); 4] = [
        ("+", add_f),
        ("-", sub_f),
        ("*", mul_f),
        ("/", div_f),
    ];

    /// Test if the arithmetic operator is valid
    #[inline]
    pub fn is_arith(op: &str) -> bool {
        ["+", "-", "*", "/"].iter().any(|&x| x == op)
    }

    #[inline]
    pub fn is_atom_op(op: &str) -> bool {
        is_arith(op) || ["==", "/=", "<", "<=", ">", ">="].iter().any(|&x| x == op)
    }

    /// Check if variable is atom (normal form)
    #[inline]
    pub fn is_atom(s: &str) -> bool {
        let atoms = ["True", "False"];
        atoms.iter().any(|&x| x == s)
    }


    /// Evaluate arithmetic expressions.
    pub fn eval_arith(op: &str, ps: &[Object]) -> Result<Object> {
        use Object::*;

        let binary_integer: HashMap<&str, &fn(i64, i64) -> i64> = 
            atom::BINARY_ARITH_INTEGER.iter().map(|(k, v)| (*k, v)).collect();
        let binary_real: HashMap<&str, &fn(f64, f64) -> f64> = 
            atom::BINARY_ARITH_REAL.iter().map(|(k, v)| (*k, v)).collect();
        
        match ps {
            [Integer(x), Integer(y)] => Ok(Integer((binary_integer[op])(*x, *y))),
            [Real(x), Real(y)] => Ok(Real((binary_real[op])(*x, *y))),
            others => Err(EvalError { msg: format!("Arith error: evaluating {:?}", others) })
        }
    }

    /// Evaluate atom eq
    #[inline]
    pub fn eval_eq(ps: &[Object]) -> Result<Object> {
        use Object::*;
        match ps {
            [x, y] => Ok(if x == y { Var("True".into()) } else { Var("False".into()) }),
            _ => Err(EvalError { msg: format!("eval_eq error: {:?}", ps) })
        }
        
    }

    /// Evaluate atom ne
    #[inline]
    pub fn eval_ne(ps: &[Object]) -> Result<Object> {
        use Object::*;
        match ps {
            [x, y] => Ok(if x != y { Var("True".into()) } else { Var("False".into()) }),
            _ => Err(EvalError { msg: format!("eval_ne error: {:?}", ps) })
        }
        
    }

    #[inline]
    pub fn eval_lt(ps: &[Object]) -> Result<Object> {
        use Object::*;
        match ps {
            [Integer(x), Integer(y)] => Ok(if x < y { Var("True".into()) } else { Var("False".into()) }),
            _ => Err(EvalError { msg: format!("eval_lt error: {:?}", ps) })
        }
    }

    #[inline]
    pub fn eval_le(ps: &[Object]) -> Result<Object> {
        use Object::*;
        match ps {
            [Integer(x), Integer(y)] => Ok(if x <= y { Var("True".into()) } else { Var("False".into()) }),
            _ => Err(EvalError { msg: format!("eval_le error: {:?}", ps) })
        }
    }

    #[inline]
    pub fn eval_gt(ps: &[Object]) -> Result<Object> {
        use Object::*;
        match ps {
            [Integer(x), Integer(y)] => Ok(if x > y { Var("True".into()) } else { Var("False".into()) }),
            _ => Err(EvalError { msg: format!("eval_gt error: {:?}", ps) })
        }
    }

    #[inline]
    pub fn eval_ge(ps: &[Object]) -> Result<Object> {
        use Object::*;
        match ps {
            [Integer(x), Integer(y)] => Ok(if x >= y { Var("True".into()) } else { Var("False".into()) }),
            _ => Err(EvalError { msg: format!("eval_ge error: {:?}", ps) })
        }
    }
}
