//use super::config::*;


trait Value {

}

pub enum Object {
    Nil,
    Integer(i32),
    Real(f64),
    Str(String),
}
