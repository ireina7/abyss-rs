use super::object::Object;
use crate::do_parse;
use crate::parser::*;
use crate::parser::combinators::*;
use std::str::{ FromStr };



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
        return Err(ParseError { msg: "".into(), pos: Pos { row: 0, col: 0 } });
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
        let mut src = ParseState::new("abcdefghijklmn");
        assert_eq!(char('a').parse(&mut src), Ok('a'));
        assert_eq!(satisfy_of("Should equal 'b'", |&c| c == 'b').parse(&mut src), Ok('b'));
        assert_eq!(satisfy(|&c| c == 'c').parse(&mut src), Ok('c'));
        assert_eq!(char('i').or(char('d')).parse(&mut src), Ok('d'));
        assert_eq!(char('e').and(char('f')).parse(&mut src), Ok('f'));
        assert_eq!(char('g').and_then(|g| satisfy(move |&c| c == 'h' || c == g)).parse(&mut src), Ok('h'));
        assert_eq!("()".parse(), Ok(Object::Nil));
    }
}
