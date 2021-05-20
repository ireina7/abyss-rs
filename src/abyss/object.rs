use std::fmt;
use std::cmp;
use std::hash::{ Hash, Hasher };
//use super::config::*;
use super::env::Environment;
use std::rc::Rc;
//use std::cell::RefCell;


pub type Env = Environment<String, Rc<Object>>;

#[derive(Clone, Debug)]
pub struct EvalError {
    pub msg: String
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

pub trait CustomObj: fmt::Display + fmt::Debug + CustomObjClone {
    fn eval(&self, env: &mut Env) -> Result<Object, EvalError>;
    fn hash_dyn(&self) -> i64;
}



pub trait CustomObjClone {
    fn clone_boxed_obj(&self) -> Box<dyn CustomObj>;
}

impl<T> CustomObjClone for T
    where T: 'static + CustomObj + Clone {
    fn clone_boxed_obj(&self) -> Box<dyn CustomObj> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CustomObj> {
    fn clone(&self) -> Box<dyn CustomObj> {
        self.clone_boxed_obj()
    }
}

impl Hash for Box<dyn CustomObj> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.hash_dyn().hash(state)
    }
}



#[allow(dead_code)]
pub enum Object {
    Nil,
    Var(String),
    Symbol(String),
    Integer(i64),
    Real(f64),
    Str(String),
    List(Vec<Object>),
    Closure(Option<String>, Rc<Object>, Rc<Object>, Env),
    Thunk(Option<String>, Rc<Object>, Env),
    //Fix() need fixpoint here
    Custom(Box<dyn CustomObj>)
}

impl Object {
    pub fn closure(ps: Object, expr: Object, env: Env) -> Self {
        Self::Closure(None, Rc::new(ps), Rc::new(expr), env.clone())
    }
}


impl Clone for Object {
    fn clone(&self) -> Self {
        use Object::*;
        match self {
            Nil => Nil,
            Var(s) => Var(s.clone()),
            Symbol(s) => Symbol(s.clone()),
            Integer(i) => Integer(*i),
            Real(n) => Real(*n),
            Str(s) => Str(s.clone()),
            List(os) => List(os.clone()),
            Closure(name, params, expr, env) => Closure(name.clone(), params.clone(), expr.clone(), env.clone()),
            Thunk(name, x, env) => Thunk(name.clone(), x.clone(), env.clone()),
            Custom(obj) => Custom(obj.clone())
        }
    }
}

impl cmp::PartialEq for Object {
    #[allow(dead_code)]
    #[allow(unused_variables)]
    fn eq(&self, other: &Self) -> bool {
        use Object::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Var(x),     Var(y))     => x == y,
            (Symbol(x),  Symbol(y))  => x == y,
            (Integer(x), Integer(y)) => x == y,
            (Real(x),    Real(y))    => x == y,
            (Str(x),     Str(y))     => x == y,
            (List(xs),   List(ys))   => xs == ys,
            (Custom(x),  Custom(y))  => false,
            _ => false
        }
    }
}

impl cmp::Eq for Object {}



impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Object::*;
        match self {
            Nil        => write!(f, "Nil"),
            Var(s)     => write!(f, "Var({})", s),
            Symbol(s)  => write!(f, "Symbol({})", s),
            Integer(i) => write!(f, "Integer({})", i),
            Real(n)    => write!(f, "Real({})", n),
            Str(s)     => write!(f, "Str({})", s),
            List(xs)   => write!(f, "{}", format!("{}{}{}", "(", &xs.iter().map(|o| format!("{:?}", o)).collect::<Vec<_>>().join(" "), ")")),
            Closure(name, ps, expr, _) => write!(f, "[closure: {:?}{} => {}]", name, ps, expr),
            Thunk(name, x, _env) => write!(f, "Thunk: {:?}({:?})", name, x),
            Custom(o)  => write!(f, "{}", o)
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object::*;
        match self {
            Nil        => write!(f, "()"),
            Var(s)     => write!(f, "{}", s),
            Symbol(s)  => write!(f, "{}", s),
            Integer(i) => write!(f, "{}", i),
            Real(n)    => write!(f, "{}", n),
            Str(s)     => write!(f, "\"{}\"", s),
            List(xs)   => write!(f, "{}", format!("{}{}{}", "(", &xs.iter().map(|o| format!("{}", o)).collect::<Vec<_>>().join(" "), ")")),
            Closure(name, _, _, _) => 
                write!(f, "[closure{}{}]", if let Some(_) = name {": "} else {""}, name.as_ref().unwrap_or(&"".to_string())),
            Thunk(name, _, _) => 
                write!(f, "[thunk{}{}]",   if let Some(_) = name {": "} else {""}, name.as_ref().unwrap_or(&"".to_string())),
            Custom(o)  => write!(f, "{}", o),
        }
    }
}

/// This `std::hash::Hash` implementation for `Object` is not safe.
/// Some specification on custom variant should be done.
impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(format!("{:?}", self).as_bytes());
    }
}












#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug)]
    struct DummyCustomObj;
    impl fmt::Display for DummyCustomObj {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "example user defined object")
        }
    }
    impl CustomObj for DummyCustomObj {
        fn eval(&self, _env: &mut Env) -> Result<Object, EvalError> {
            Ok(Object::Nil)
        }
        fn hash_dyn(&self) -> i64 {
            0_i64
        }
    }
    #[test]
    fn test_object_debug_print() {
        let nil = Object::Nil;
        let int = Object::Integer(7);
        let num = Object::Real(0.7);
        let sss = Object::Str("test_str".into());
        let osx = Object::List(vec![Object::Str("+".into()), Object::Real(0.7), Object::Real(2.0)]);
        let obj = Object::Custom(Box::new(DummyCustomObj));

        assert_eq!(format!("{:?}", nil), "Nil");
        assert_eq!(format!("{:?}", int), "Integer(7)");
        assert_eq!(format!("{:?}", num), "Real(0.7)");
        assert_eq!(format!("{:?}", sss), "Str(test_str)");
        assert_eq!(format!("{:?}", osx), "(Str(+) Real(0.7) Real(2))");
        assert_eq!(format!("{:?}", obj), "example user defined object");
    }
    #[test]
    fn test_object_display_print() {
        let nil = Object::Nil;
        let int = Object::Integer(7);
        let num = Object::Real(0.7);
        let sss = Object::Str("test_str".into());
        let osx = Object::List(vec![Object::Var("+".into()), Object::Real(0.7), Object::Real(2.0)]);
        let obj = Object::Custom(Box::new(DummyCustomObj));

        assert_eq!(format!("{}", nil), "()");
        assert_eq!(format!("{}", int), "7");
        assert_eq!(format!("{}", num), "0.7");
        assert_eq!(format!("{}", sss), "\"test_str\"");
        assert_eq!(format!("{}", osx), "(+ 0.7 2)");
        assert_eq!(format!("{}", obj), "example user defined object");
    }
}
