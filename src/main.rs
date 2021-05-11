mod abyss;
#[macro_use] mod parser;
mod logic;

use std::io::prelude::*;
use std::io::{self, BufRead};
use abyss::eval::Eval;



fn prompt(s: &str) {
    print!("{} < ", s);
    io::stdout().flush().ok().expect("Could not flush stdout");
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    //let mut input = String::new();
    prompt("abyss");
    for line in stdin.lock().lines() {

        let line = line.unwrap();
        if line == "quit" {
            break;
        }
        let ast = line.parse::<abyss::Object>();
        //println!("{:?} =>", ast);
        let mut env = std::collections::HashMap::new();
        let res = match ast {
            Ok(src) => src.eval(&mut env),
            Err(err) => Err(abyss::object::EvalError { msg: err.msg })
        };
        println!("{:?}\n", res);
        prompt("abyss");
    }
    Ok(())
}
