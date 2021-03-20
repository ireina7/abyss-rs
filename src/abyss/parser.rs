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



fn variable() -> Wrapper<impl Parser<Output=Object> + Clone> {
    identifier().map(|s| Object::Var(s))
        .info("Parsing variable")
}

fn number() -> Wrapper<impl Parser<Output=Object> + Clone> {
    (real() | integer()).info("Parsing number")
}

fn integer() -> Wrapper<impl Parser<Output=Object> + Clone> {
    many1(digit()).map(|s| Object::Integer(s.into_iter().collect::<String>().parse().unwrap()))
        .info("Parsing integer")
}

fn real() -> Wrapper<impl Parser<Output=Object> + Clone> {
    (many1(digit()) >> move |l|
     char('.')      >> move |_| {
         let a = l.clone().into_iter().collect::<String>();
         many1(digit()) >> move |r| {
             let b = r.clone().into_iter().collect::<String>();
             pure(Object::Real(format!("{}.{}", a, b).parse().unwrap()))
         }
     }).info("Parsing real number")
}

fn symbol() -> Wrapper<impl Parser<Output=Object> + Clone> {
    char('\'').and(identifier()).map(|s| Object::Symbol(s)).info("Parsing symbol")
}

fn string() -> Wrapper<impl Parser<Output=Object> + Clone> {
    (
        char('"').and(many(satisfy(|&c| c != '"'))) >> move |s| {
        char('"') >> move |_|
        pure(Object::Str(s.clone().into_iter().collect()))
    }).info("Parsing string")
}

fn atom() -> Wrapper<impl Parser<Output=Object> + Clone> {
    (variable() | number() | symbol() | string()).wrap()
}






/**
  * The main parser function
  */
fn parse_to_object(src: &str) -> Result<Object, ParseError> {

    let mut src = ParseState::new(src.trim());
    fn item<P: Parser + Clone>(p: P) -> Wrapper<impl Parser<Output=P::Output> + Clone> where
        P::Output : Clone {

        (p.and_then(move |x: P::Output| blank().and_then(move |_| pure(x.clone())))).wrap()
    }
    let parser = atom() | fix(|list| Box::new (
        char('(') >> move |_|
        many(
            item(number()) | item(variable()) | item(symbol()) | item(string()) | list.clone()
        ) >> move |xs|
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

    #[test]
    fn test_string() {
        let mut src = ParseState::new("\"str\"");
        let parser = string();
        assert_eq!(parser.parse(&mut src).ok(), Some(Object::Str("str".into())));
    }
}
