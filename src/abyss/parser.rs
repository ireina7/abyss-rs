use super::object::Object;
use std::str::FromStr;


#[derive(PartialEq, Debug)]
pub struct ParseError {
    msg: &'static str,
    line: i32
}

impl FromStr for Object {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_to_object(s)
    }
}

fn parse_to_object(src: &str) -> Result<Object, ParseError> {
    let _ = src.trim();

    Ok(Object::Nil)
}
#[allow(dead_code)]
#[allow(unused_variables)]
fn parse_var(src: &str) -> Result<Object, ParseError> {
    let ss = src.trim();
    let of = |i| ss.chars().nth(i).unwrap();
    if !(ss.len() > 0 && of(0) != '(' && of(0) != ')') {
        return Err(ParseError { msg: "", line: 0 });
    }
    Ok(Object::Nil)
}
#[allow(dead_code)]
#[allow(unused_variables)]
fn parse_int(src: &str) -> Result<Object, ParseError> {
    unimplemented!()
}
#[allow(dead_code)]
#[allow(unused_variables)]
fn parse_num(src: &str) -> Result<Object, ParseError> {
    unimplemented!()
}
#[allow(dead_code)]
#[allow(unused_variables)]
fn parse_sym(src: &str) -> Result<Object, ParseError> {
    unimplemented!()
}
#[allow(dead_code)]
#[allow(unused_variables)]
fn parse_str(src: &str) -> Result<Object, ParseError> {
    unimplemented!()
}








#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!("".parse(), Ok(Object::Nil));
    }
}
