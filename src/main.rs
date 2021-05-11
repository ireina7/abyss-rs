mod abyss;
#[macro_use] mod parser;
mod logic;

use std::io::prelude::*;
use std::io::{self, BufRead};
use std::fmt;
use abyss::eval::Eval;



fn prompt(s: &str) {
    print!("{} < ", s);
    io::stdout().flush().ok().expect("Could not flush stdout");
}

fn bye() {
    println!("Bye bye.");
}

fn result<V, E>(res: Result<V, E>) -> String 
    where
        V: fmt::Debug + fmt::Display,
        E: fmt::Debug
{
    match res {
        Ok(v)    => format!("{:?}", v  ),
        Err(err) => format!("{:?}", err),
    }
}


fn main() -> io::Result<()> {

    let stdin = io::stdin();
    //let mut input = String::new();
    prompt("abyss");
    for line in stdin.lock().lines() {

        let line = line?;
        if line == "quit" {
            break;
        }
        let ast = line.parse::<abyss::Object>();
        //println!("{:?} =>", ast);
        let mut env = std::collections::HashMap::new();
        let res = ast
            .map_err(|parser::ParseError {msg, ..}| abyss::eval::EvalError { msg })
            .and_then(|src| src.eval(&mut env));
        /*
        let res = match ast {
            Ok(src) => src.eval(&mut env),
            Err(err) => Err(abyss::object::EvalError { msg: err.msg })
        };
        */
        println!("{}\n", result(res));
        prompt("abyss");
    }
    bye();
    Ok(())
}
