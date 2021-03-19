use super::object::Object;
//use crate::do_parse;
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
    let mut src = ParseState::new(src.trim());
    let parser = identifier().map(|s| Object::Var(s)) | fix(|f| Box::new (
        char('(') >> move |_|
        many((identifier() >> move |s| blank() >> move |_| pure(Object::Var(s.clone()))) | f.clone()) >> move |xs|
        char(')').and(blank()) >> move |_|
        pure(Object::List(xs.clone()))
    ));

    parser.parse(&mut src)
}





#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        use Object::*;
        let src = "(hello world)";
        assert_eq!(src.parse::<Object>(), Ok(List(vec![Var("hello".into()), Var("world".into())])));
    }

    #[test]
    fn test_psc() {
        let mut src = ParseState::new("a b (d g) () (add x (v) d) ())");
        let _id_or_blank = identifier() >> move |s| blank() >> move |_| pure(Object::Var(s.clone()));

        let parser = fix(|f| Box::new (
            char('(') >> move |_|
            many((identifier() >> move |s| blank() >> move |_| pure(Object::Var(s.clone()))) | f.clone()) >> move |xs|
            char(')').and(blank()) >> move |_|
            pure(Object::List(xs.clone()))
        ));
        assert_eq!(parser.parse(&mut src).ok(), None);
    }
}
