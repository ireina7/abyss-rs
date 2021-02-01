//use super::config::*;


pub trait Value {
    fn eval(&self) -> Self;
}

#[derive(Debug)]
pub enum Object {
    Nil,
    Integer(i32),
    Real(f64),
    Str(String),
}
