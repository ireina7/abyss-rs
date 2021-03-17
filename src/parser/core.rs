use std::str::Chars;


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
    pub fn new(s: &'a str) -> Self {
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

    /* Utilities */
    fn and<P: Parser>(self, other: P) -> And<Self, P> where
        Self: Sized {

        And { a: self, b: other }
    }
    fn or<P: Parser>(self, other: P) -> Or<Self, P> where
        Self: Sized, P: Parser<Output=Self::Output> {

        Or { a: self, b: other }
    }
    fn map<B, F>(self, f: F) -> Map<Self, F> where
        Self: Sized, F: Fn(Self::Output) -> B {

        Map { parser: self, f: f }
    }
    fn and_then<B, F>(self, f: F) -> AndThen<Self, F> where
        Self: Sized, F: Fn(Self::Output) -> B {

        AndThen { parser: self, f: f }
    }
    fn info(self, msg: &str) -> Logger<Self> where
        Self: Sized {
        Logger { parser: self, msg: msg.into() }
    }
}





#[derive(Clone, Debug)]
pub struct Logger<P> {
    parser: P,
    msg: String
}

impl<P: Parser> Parser for Logger<P> {

    type Output = P::Output;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        match self.parser.parse(state) {
            ok @ Ok(_) => ok,
            Err(ParseError { msg: _, pos }) => Err(ParseError { msg: self.msg.clone(), pos: pos })
        }
    }
}



#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
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
        let x = self.parser.parse(state)?;
        (self.f)(x).parse(state)
    }
}

#[derive(Clone, Debug)]
pub struct Map<P, F> {
    parser: P,
    f: F
}

impl<A, B, F> Parser for Map<A, F> where
    A: Parser,
    F: Fn(A::Output) -> B {

    type Output = B;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        let x = self.parser.parse(state)?;
        Ok((self.f)(x))
    }
}






#[derive(Clone, Copy, Debug)]
pub struct Char {
    pub ch: char
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



#[derive(Clone, Debug)]
pub struct Pure<A: Clone> {
    x: A
}

impl<A: Clone> Parser for Pure<A> {
    type Output = A;
    fn parse<'a>(&self, _state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        Ok(self.x.clone())
    }
}

pub fn pure<A: Clone>(x: A) -> Pure<A> {
    Pure { x: x }
}


#[derive(Clone, Debug)]
pub struct Many<P> {
    parser: P,
}

impl<P> Parser for Many<P> where
    P: Parser {

    type Output = Vec<P::Output>;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        let mut vec = vec![];
        let mut tmp = state.clone();
        while let Ok(a) = self.parser.parse(state) {
            vec.push(a);
            tmp = state.clone();
        }
        *state = tmp;
        Ok(vec)
    }
}

pub fn many<P: Parser>(p: P) -> Many<P> {
    Many { parser: p }
}

#[derive(Clone, Debug)]
pub struct Many1<P> {
    parser: P
}

impl<P: Parser> Parser for Many1<P> {

    type Output = Vec<P::Output>;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        let mut vec = vec![];
        let a = self.parser.parse(state)?;
        vec.push(a);
        let mut tmp = state.clone();
        while let Ok(a) = self.parser.parse(state) {
            vec.push(a);
            tmp = state.clone();
        }
        *state = tmp;
        Ok(vec)
    }
}


pub fn at_least_1<P: Parser>(p: P) -> Many1<P> {
    Many1 { parser: p }
}

pub fn many1<P: Parser>(p: P) -> Many1<P> {
    at_least_1(p)
}

#[derive(Clone, Debug)]
pub struct ParseString {
    s: String
}

impl Parser for ParseString {

    type Output = String;
    fn parse<'a>(&self, state: &mut ParseState<'a>) -> Result<Self::Output, ParseError> {
        match state.zip(self.s.chars()).all(|(a, b)| a == b) { //this is unimplemented since the iterator will assume one extra char!
            true => Ok(self.s.clone()),
            _ => Err(ParseError { msg: "Parsing string".into(), pos: Pos { col: 0, row: 0 } }) //unimplemented for pos!
        }
    }
}

//Should not use currently
pub fn string(s: &str) -> ParseString {
    ParseString { s: s.into() }
}
