//! The module for all error abstractions

use std::convert::Into;
use std::fmt;


pub trait Error: std::error::Error {
    fn backtrace(&self) -> Option<&Backtrace>;
    fn into<T>(self) -> T where Self: Into<T> {
        Into::into(self)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct Backtrace {
    trace: Vec<String>,
}

impl Backtrace {
    pub fn new() -> Self {
        Backtrace { trace: vec![] }
    }
    pub fn trace(&self) -> &[String] {
        &self.trace
    }
    pub fn push(&mut self, item: String) {
        self.trace.push(item);
    }
}


impl fmt::Display for Backtrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.trace {
            write!(f, "{}\n", &item)?;
        }
        Ok(())
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backtrace() {
        let mut bt = Backtrace::new();
        bt.push("0".into());
        bt.push("1".into());
        bt.push("2".into());
        assert_eq!(format!("{}", bt), String::from("0\n1\n2\n"));
    }
}