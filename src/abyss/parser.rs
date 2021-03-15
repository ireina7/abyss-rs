use super::object::Object;
use std::str::{ FromStr, Chars };




#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Pos {
    pub col: usize,
    pub row: usize
}

#[derive(PartialEq, Debug)]
pub struct ParseError {
    pub msg: String,
    pub pos: Pos
}



#[derive(Clone, Debug)]
pub struct ParseState<'a> {
    src: Chars<'a>,
    pub pos: Pos
}
impl<'a> ParseState<'a> {
    fn new(s: &'a str) -> Self {
        ParseState {
            src: s.chars(),
            pos: Pos { col: 0, row: 0 },
        }
    }
}
impl<'a> Iterator for ParseState<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.src.next()?;
        self.pos = match ch {
            '\n' => Pos {
                row: self.pos.row + 1,
                col: 0
            },
            '\t' => Pos {
                col: self.pos.col + 8 - (self.pos.col - 1) % 8,
                ..self.pos
            },
            _ => Pos {
                col: self.pos.col + 1,
                ..self.pos
            },
        };
        Some(ch)
    }
}


pub trait Parser {
    type Output;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError>;

    fn and<P: Parser>(self, other: P) -> And<Self, P> where
        Self: Sized {

        And { a: self, b: other }
    }
    fn or<P: Parser>(self, other: P) -> Or<Self, P> where
        Self: Sized, P: Parser<Output=Self::Output> {

        Or { a: self, b: other }
    }
    fn and_then<B, F>(self, f: F) -> AndThen<Self, F> where
        Self: Sized, F: Fn(Self::Output) -> B {

        AndThen { parser: self, f: f }
    }
}


impl FromStr for Object {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_to_object(s)
    }
}



pub struct Satisfy<F> {
    satisfy: F,
    msg: String
}

impl<F> Parser for Satisfy<F> where
    F: Fn(&char) -> bool {

    type Output = char;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        state.next()
            .filter(&self.satisfy)
            .ok_or(ParseError {
                msg: self.msg.clone(),
                pos: state.pos
            })
    }
}

pub fn satisfy_of<F>(msg: &str, f: F) -> Satisfy<F> where
    F: Fn(&char) -> bool {

    Satisfy { satisfy: f, msg: msg.into() }
}
pub fn satisfy<F>(f: F) -> Satisfy<F> where
    F: Fn(&char) -> bool {

    Satisfy { satisfy: f, msg: "unknown".into() }
}


pub struct And<A, B> {
    a: A,
    b: B
}

impl<A: Parser, B: Parser> Parser for And<A, B> {
    type Output = B::Output;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        self.a.parse(state)?;
        self.b.parse(state)
    }
}

pub struct Or<A, B> {
    a: A,
    b: B
}

impl<A, B> Parser for Or<A, B> where
    A: Parser,
    B: Parser<Output=A::Output> {

    type Output = A::Output;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        let tmp = state.clone();
        let a = self.a.parse(state);
        if let Ok(_) = a {
            return a;
        }
        *state = tmp;
        self.b.parse(state)
    }
}

pub struct AndThen<P, F> {
    parser: P,
    f: F
}

impl<A, B, F> Parser for AndThen<A, F> where
    A: Parser,
    B: Parser,
    F: Fn(A::Output) -> B {

    type Output = B::Output;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        let f = &self.f;
        let x = self.parser.parse(state)?;
        f(x).parse(state)
    }
}




#[derive(Clone, Debug)]
pub struct Char {
    ch: char
}

impl Parser for Char {
    type Output = char;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        state.next()
            .filter(|&c| self.ch == c)
            .ok_or(ParseError {
                msg: format!("error while parsing char: {}", self.ch),
                pos: state.pos
            })
    }
}

pub fn char(ch: char) -> Char {
    Char { ch }
}


pub fn digit() -> Satisfy<impl Fn(&char) -> bool> {
    satisfy(|&c| ('0'..'9').any(|d| d == c))
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


#[allow(unused_macros)]
#[macro_use]
macro_rules! do_parse {
    ($x:pat = $e:expr, => $exp:expr) => {
        $e.and_then(move |$x| $exp)
    };
    ($x:pat = $e:expr, $($y:pat = $es:expr),+, => $exp:expr) => {
        $e.and_then(move |$x| do_parse!($($y = $es),+, => $exp))
    };
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

    #[test]
    fn test_parser_monad() {
        let mut src = ParseState::new("a0bcdefghijklmn");
        let ans = char('a')
            .and_then(|_| digit())
            .and_then(|_| char('b'));
        assert_eq!(ans.parse(&mut src), Ok('b'));
    }

    #[test]
    fn test_parser_monad_do_notation() {
        let mut src = ParseState::new("a0bcdefghijklmn");
        let ans = do_parse! {
            a = char('a'),
            _ = digit(),
            b = char('b'),

            => satisfy(move |&c| c == a || c == b || c == 'c')
        };
        assert_eq!(ans.parse(&mut src), Ok('c'));
    }

}
