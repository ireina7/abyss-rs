//use super::config::*;
use std::fmt;

pub trait CustomObj {
    fn about(&self) -> String;
}

pub enum Object {
    Nil,
    Integer(i32),
    Real(f64),
    Str(String),
    Custom(Box<dyn CustomObj>)
}


impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Nil        => write!(f, "Nil"),
            Object::Integer(i) => write!(f, "Integer({})", i),
            Object::Real(n)    => write!(f, "Real({})", n),
            Object::Str(s)     => write!(f, "Str({})", s),
            _                  => write!(f, "<unkown object>")
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Nil        => write!(f, "()"),
            Object::Integer(i) => write!(f, "{}", i),
            Object::Real(n)    => write!(f, "{}", n),
            Object::Str(s)     => write!(f, "\"{}\"", s),
            _                  => write!(f, "<unkown object>")
        }
    }
}
