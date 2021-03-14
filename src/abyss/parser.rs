use super::object::Object;


pub fn parse(src: String) -> Object {
    let _ = src;
    Object::Nil
}





#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse("".into()), Object::Nil);
    }
}
