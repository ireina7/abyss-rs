use super::core::*;
use crate::do_parse;


pub fn char(ch: char) -> Char {
    Char { ch }
}

pub fn digit() -> impl Parser<Output=char> + Clone {

    satisfy(|&c| ('0'..'9').any(|d| d == c)).info("Parsing single digit")
}
pub fn digits() -> impl Parser<Output=Vec<char>> + Clone {

    many(digit()).info("Parsing many digits")
}

pub fn letter() -> impl Parser<Output=char> + Clone {
    satisfy(|&c| c.is_alphabetic()).info("Parsing single letter")
}

pub fn letters() -> impl Parser<Output=Vec<char>> + Clone {
    many(letter()).info("Parsing many letters")
}

pub fn identifier() -> impl Parser<Output=String> + Clone {

    letter()
        .and_then(move |x_| many(letter().or(digit()).or(char('_')))
                  .and_then(move |xs| pure(vec![x_].into_iter().chain(xs.into_iter()).collect::<String>())))
        .info("Parsing identifier")
}

pub fn identifiers_sep_by_blank() -> impl Parser<Output=Vec<String>> + Clone {

    many( do_parse! {
        x =o identifier(),
        _ =o many(char(' ').or(char('\t')).or(char('\n'))),
          =o pure(x.clone())
    }).info("Parsing identifiers")
}

pub fn list_of_identifiers_sep_by_blank() -> impl Parser<Output=Vec<String>> + Clone {

    (do_parse! {
        _ =o char('('),
        s =o identifiers_sep_by_blank(),
        _ =o char(')'),
          =o pure(s.clone())
    }).info("Parsing list of identifiers")
}








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
    }

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

    #[test]
    fn test_parser_string() {
        let mut src = ParseState::new("hello0%");
        let parser = string("hell");
        assert_eq!(parser.parse(&mut src).ok(), Some("hell".into()));
        //assert_eq!(src.next(), Some('o'));
    }

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
}
