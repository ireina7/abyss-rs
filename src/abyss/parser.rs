use super::object::Object;
//use crate::do_parse;
use crate::parser::*;
use crate::parser::combinators::*;
use std::str::{ FromStr };
use std::ops::Deref;


impl FromStr for Object {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_to_object(s)
    }
}



pub fn magma() -> Wrapper<impl Parser<Output=String> + Clone> {

    (satisfy(|&c| 
        !c.is_digit(10) && "'( )\"".chars().all(|x| c != x))
                           >> move |x_|
     many(except("( )\""))   >> move |xs|
     pure(vec![x_].into_iter().chain(xs.into_iter()).collect::<String>()))
        .info("Parsing magma identifier")
}

fn variable() -> Wrapper<impl Parser<Output=Object> + Clone> {
    magma()
        .map(|s| Object::Var(s))
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





#[derive(Debug, PartialEq, Eq)]
pub struct Pattern {
    expr: Object
}

impl Pattern {
    pub fn unwrap(self) -> Object {
        self.expr
    }
}


impl From<Object> for Pattern {
    fn from(obj: Object) -> Self {
        use Object::*;
        fn pack(obj: Object) -> Pattern {
            Pattern { expr: obj }
        }
        match obj {
            Nil            => pack(Nil),
            Var(_)         => pack(obj),
            Symbol(_)      => pack(obj),
            Cons(_)        => pack(obj),
            Integer(_)     => pack(obj),
            Real(_)        => pack(obj),
            Str(_)         => pack(obj),
            Thunk(_, _, _) => pack(obj),
            List(xs) => match &xs[..] {
                // Only when an var occurs on the head of a list should it be converted into a Cons object.
                [Var(op), xs @ ..] => {
                    let xs = xs.iter().map(|x| Pattern::from(x.clone()).unwrap());
                    let list = vec![Cons(op.clone())].into_iter().chain(xs.into_iter()).collect();
                    pack(List(list))
                },
                xs => pack(List(xs.iter().map(|x| Pattern::from(x.clone()).unwrap()).collect()))
            },
            _ => pack(obj)
        }
    }
}

impl Deref for Pattern {
    type Target = Object;
    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}



fn convert_pattern(expr: Object) -> Object {
    use Object::*;
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), expr, List(cases)] if &op[..] == "case" => {
                let cases = List(cases.iter().map(|case| match case {
                    List(xs) => match &xs[..] {
                        [pat, result] => {
                            let pat = Pattern::from(pat.clone()).unwrap();
                            List(vec![pat, result.clone()])
                        },
                        others => List(others.to_vec())
                    },
                    others => others.clone()
                }).collect());
                List(vec![Var(op.clone()), expr.clone(), cases])
            },
            _ => List(xs)
        },
        others => others
    }
}

fn conversions(expr: Object) -> Object {
    let ans = convert_pattern(expr);
    ans
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

    let ans = parser.parse(&mut src)?;
    Ok(conversions(ans))
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

    #[test]
    fn test_pattern_conversion() {
        use Object::*;
        let pat_raw = String::from("(cons x (cons y rest))").parse::<Object>().unwrap();
        let pat = Pattern::from(pat_raw).unwrap();

        let label = List(vec![
            Cons("cons".into()), 
            Var("x".into()), 
            List(vec![
                Cons("cons".into()),
                Var("y".into()), 
                Var("rest".into())
            ])
        ]);
        assert_eq!(pat, label);
    }
}
