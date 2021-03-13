use std::fmt;



pub type Env = std::collections::HashMap<String, Object>;

pub trait CustomObj {
    fn about(&self) -> String;
}

pub trait SExpr {
    fn eval(&self, env: &mut Env) -> Self;
}



trait CustomObjClone {
    fn clone_boxed_obj(&self) -> Box<dyn CustomObj>;
}

impl<T> CustomObjClone for T
    where T: 'static + CustomObj + Clone {
    fn clone_boxed_obj(&self) -> Box<dyn CustomObj> {
        Box::new(self.clone())
    }
}


//#[derive(Clone)]
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
            Custom(_) => self.clone()
        }
    }
}

impl SExpr for Object {
    fn eval(&self, _env: &mut Env) -> Self {
        use Object::*;
        match self {
            Nil => self,
            Var(_) => self,
            Symbol(_) => self,
            Integer(_) => self,
            Real(_) => self,
            Str(_) => self,
            _ => self
        }.clone()
    }
}



impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Nil        => write!(f, "Nil"),
            Object::Var(s)     => write!(f, "Var({})", s),
            Object::Symbol(s)  => write!(f, "{}", s),
            Object::Integer(i) => write!(f, "Integer({})", i),
            Object::Real(n)    => write!(f, "Real({})", n),
            Object::Str(s)     => write!(f, "Str({})", s),
            Object::List(xs)   => write!(f, "{}", format!("{}{}{}", "(", &xs.iter().map(|o| format!("{:?}", o)).collect::<Vec<_>>().join(" "), ")")),
            Object::Custom(o)  => write!(f, "{}", o.about())
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Nil        => write!(f, "()"),
            Object::Var(s)     => write!(f, "{}", s),
            Object::Symbol(s)  => write!(f, "{}", s),
            Object::Integer(i) => write!(f, "{}", i),
            Object::Real(n)    => write!(f, "{}", n),
            Object::Str(s)     => write!(f, "\"{}\"", s),
            Object::List(xs)   => write!(f, "{}", format!("{}{}{}", "(", &xs.iter().map(|o| format!("{}", o)).collect::<Vec<_>>().join(" "), ")")),
            Object::Custom(o)  => write!(f, "{}", o.about()),
        }
    }
}










#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone)]
    struct DummyCustomObj;
    impl CustomObj for DummyCustomObj {
        fn about(&self) -> String {
            "Custom object example".into()
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
        assert_eq!(format!("{:?}", obj), "Custom object example");
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
        assert_eq!(format!("{}", obj), "Custom object example");
    }
}
