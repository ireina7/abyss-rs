use std::fmt;
use super::config::*;


pub type Env = HashMap<String, Object>;

pub struct EvalError {
    msg: String
}
pub trait CustomObj: fmt::Display + fmt::Debug + CustomObjClone {
    fn eval(&self, env: &mut Env) -> Result<Object, EvalError>;
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



#[allow(dead_code)]
pub enum Object {
    Nil,
    Var(String),
    Symbol(String),
    Integer(i32),
    Real(f64),
    Str(String),
    List(Vec<Object>),
    Custom(Box<dyn CustomObj>)
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
            Custom(obj) => Custom(obj.clone())
        }
    }
}




impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Object::*;
        match self {
            Nil        => write!(f, "Nil"),
            Var(s)     => write!(f, "Var({})", s),
            Symbol(s)  => write!(f, "{}", s),
            Integer(i) => write!(f, "Integer({})", i),
            Real(n)    => write!(f, "Real({})", n),
            Str(s)     => write!(f, "Str({})", s),
            List(xs)   => write!(f, "{}", format!("{}{}{}", "(", &xs.iter().map(|o| format!("{:?}", o)).collect::<Vec<_>>().join(" "), ")")),
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
            Custom(o)  => write!(f, "{}", o),
        }
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
