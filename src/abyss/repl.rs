//! Repl of the Abyss programming language

use std::io::prelude::*;
use std::io::{self, BufRead};
use std::fmt;
use super::object::Object;
use super::eval::Eval;



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

pub fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    //let mut input = String::new();
    prompt("abyss");
    for line in stdin.lock().lines() {

        let line = line?;
        if line == "quit" {
            break;
        }
        if line == "" {
            prompt("abyss");
            continue;
        }
        let ast = line.parse::<Object>();
        //println!("{:?} =>", ast);
        let mut env = std::collections::HashMap::new();
        let y = "(lambda (f) ((lambda (x) (f (lambda (v) (x x v)))) (lambda (x) (f (lambda (v) (x x v))))))"
            .parse::<Object>().unwrap();
        let y = y.eval(&env).unwrap();
        env.insert(String::from("fix"), y);
        let res = ast
            .map_err(|crate::parser::ParseError {msg, ..}| super::eval::EvalError { msg })
            .and_then(|src| src.eval(&env));
        
        println!("{}\n", result(res));
        prompt("abyss");
    }
    bye();
    Ok(())
}

