use super::core::*;
//use super::ops::*;
//use crate::do_parse;


#[allow(dead_code)]
pub fn wrap<P: Parser>(p: P) -> Wrapper<P> {
    Wrapper::new(p)
}

#[allow(dead_code)]
pub fn pure<A: Clone>(x: A) -> Pure<A> {
    Pure::new(x)
}


#[allow(dead_code)]
pub fn satisfy<F>(f: F) -> Satisfy<F> where
    F: Fn(&char) -> bool {

    Satisfy::new(f)
}

#[allow(dead_code)]
pub fn fix<'a, A, F>(fix: F) -> Fix<'a, A> where
    F: for<'f> Fn(&'f Fix<'a, A>) -> Box<dyn Parser<Output=A> + 'f> + 'a {

    Fix::new(fix)
}

#[allow(dead_code)]
pub fn many<P: Parser>(p: P) -> Many<P> {
    Many::new(p)
}

#[allow(dead_code)]
pub fn at_least_1<P: Parser>(p: P) -> Many1<P> {
    Many1::new(p)
}

#[allow(dead_code)]
pub fn many1<P: Parser>(p: P) -> Many1<P> {
    at_least_1(p)
}





#[allow(dead_code)]
pub fn char(ch: char) -> Char {
    Char { ch }
}

#[allow(dead_code)]
pub fn digit() -> Wrapper<impl Parser<Output=char> + Clone> {

    satisfy(|&c| c.is_digit(10)).info("Parsing single digit")
}

#[allow(dead_code)]
pub fn digits() -> Wrapper<impl Parser<Output=Vec<char>> + Clone> {

    many(digit()).info("Parsing many digits")
}

#[allow(dead_code)]
pub fn letter() -> Wrapper<impl Parser<Output=char> + Clone> {
    satisfy(|&c| c.is_alphabetic()).info("Parsing single letter")
}

#[allow(dead_code)]
pub fn letters() -> Wrapper<impl Parser<Output=Vec<char>> + Clone> {
    many(letter()).info("Parsing many letters")
}

#[allow(dead_code)]
pub fn blank() -> Wrapper<impl Parser<Output=String> + Clone> {
    many(char(' ') | char('\t') | char('\n')).map(|xs| xs.into_iter().collect())
        .info("Parsing blanks")
}

#[allow(dead_code)]
pub fn identifier() -> Wrapper<impl Parser<Output=String> + Clone> {

    (letter()                                 >> move |x_|
     many(letter().or(digit()).or(char('_'))) >> move |xs|
     pure(vec![x_].into_iter().chain(xs.into_iter()).collect::<String>()))
        .info("Parsing identifier")
}



#[allow(dead_code)]
pub fn any() -> Wrapper<impl Parser<Output=char> + Clone> {
    satisfy(|_| true).info("Parsing any char")
}

#[allow(dead_code)]
pub fn one_of(cs: &str) -> Wrapper<impl Parser<Output=char> + Clone> {
    let ss = cs.to_string();
    satisfy(move |&c| ss.chars().any(|x| x == c))
        .info(&format!("Parsing char of one of {}", cs))
}

#[allow(dead_code)]
pub fn except(cs: &str) -> Wrapper<impl Parser<Output=char> + Clone> {
    let ss = cs.to_string();
    satisfy(move |&c| !ss.chars().any(|x| x == c))
        .info(&format!("Parsing char except one of {}", cs))
}

#[allow(dead_code)]
pub fn identifiers_sep_by_blank() -> Wrapper<impl Parser<Output=Vec<String>> + Clone> {

    many(
        identifier() >> move |x|
        blank()      >> move |_|
        pure(x.clone()))
        .info("Parsing identifiers")
}

#[allow(dead_code)]
pub fn list_of_identifiers_sep_by_blank() -> Wrapper<impl Parser<Output=Vec<String>> + Clone> {

    (char('(')                  >> move |_|
     identifiers_sep_by_blank() >> move |s|
     char(')')                  >> move |_|
     pure(s.clone()))
        .info("Parsing list of identifiers")
}
/*
pub fn list<P: Parser<Output=Vec<String>> + Clone>(p: Wrapper<P>) -> Wrapper<impl Parser<Output=Vec<String>> + Clone> {

    wrap(
        char('(') >> move |__|
        many(identifier()) >> move |xs|
        char(')') >> move |__|
        pure(xs.clone())
    )
}
*/






#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_monad() {
        let mut src = ParseState::new("a0bcdefghijklmn");
        let ans = char('a')
            .and_then(|_| digit())
            .and_then(|_| char('b'));
        assert_eq!(ans.parse(&mut src), Ok('b'));
    }
    /*
    #[test]
    fn test_parser_monad_do_notation() {
        let mut src = ParseState::new("a0bcdefghijklmn");
        let parser = do_parse! {
            a =o char('a'),
            _ =o digit()  ,
            b =o char('b'),

            =o satisfy(move |&c| c == a || c == b || c == 'c')
        };
        assert_eq!(parser.parse(&mut src), Ok('c'));
    }*/

    #[test]
    fn test_parser_many() {
        let mut src = ParseState::new("aa0bcdefghijklmn");
        let parser = many(char('a'));
        assert_eq!(parser.parse(&mut src), Ok(vec!['a', 'a']));
    }

    #[test]
    fn test_parser_many1() {
        let mut src = ParseState::new("aa01bcdefghijklmn");
        let parser0 = at_least_1(char('a'));
        let parser1 = digits();
        let parser2 = many1(char('a'));
        assert_eq!(parser0.parse(&mut src), Ok(vec!['a', 'a']));
        assert_eq!(parser1.parse(&mut src), Ok(vec!['0', '1']));
        assert_eq!(parser2.parse(&mut src).ok(), None);
    }

    #[test]
    fn test_parser_identifier() {
        let mut src = ParseState::new("hello0)");
        let parser = identifier();
        assert_eq!(parser.parse(&mut src), Ok("hello0".into()));
    }
    /*
    #[test]
    fn test_parser_string() {
        let mut src = ParseState::new("hello0%");
        let parser = string("hell");
        assert_eq!(parser.parse(&mut src).ok(), Some("hell".into()));
        //assert_eq!(src.next(), Some('o'));
    }*/

    #[test]
    fn test_parser_map() {
        let mut src = ParseState::new("hello0%");
        let parser = letters().map(|cs| cs.into_iter().map(|c| if c == 'l' { 'x' } else { c }).collect::<String>());
        assert_eq!(parser.parse(&mut src).ok(), Some("hexxo".into()));
    }

    #[test]
    fn test_parse_list() {
        let mut src = ParseState::new("(Hello world)");
        let parser = list_of_identifiers_sep_by_blank();
        assert_eq!(parser.parse(&mut src), Ok(vec!["Hello", "world"].into_iter().map(|s| s.into()).collect()));
    }

    #[test]
    fn test_parser_blank() {
        let mut src = ParseState::new("(   )");
        let parser = char('(').and(blank());
        assert_eq!(parser.parse(&mut src).ok(), Some("   ".into()));
    }

    #[test]
    fn test_parser_fix() {
        let mut src = ParseState::new("....@");
        let parser = fix(|f| Box::new(
            char('.') >> move |_|
            f.clone().or(char('@')) >> move |xs|
            pure(xs.clone())
        ));
        assert_eq!(parser.parse(&mut src).ok(), Some('@'));
    }
}
