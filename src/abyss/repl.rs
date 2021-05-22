//! Repl of the Abyss programming language

use std::io::prelude::*;
use std::io::{self, BufRead};
use std::fmt;
//use super::env::Environment;
use super::object::Object;
use super::eval::{self, Eval};



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
        E: fmt::Debug + fmt::Display,
{
    match res {
        Ok(v)    => format!("{}", v  ),
        Err(err) => format!("{}", err),
    }
}

pub fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    //let mut input = String::new();
    let mut env = eval::env();
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
        if line == "help" {
            println!("The Abyss programming language developed by Ireina.\n");
            prompt("abyss");
            continue;
        }
        let ast = line.parse::<Object>();
        //println!("{:?} =>", ast);
        let res: Result<Object, _> = ast
            .map_err(|crate::parser::ParseError {msg, ..}| super::eval::EvalError { msg })
            .and_then(|src| {
                if eval::decl::is_decl(&src) { 
                    eval::decl::eval_decl(&src, &mut env).unwrap();
                    Ok(Object::Nil)
                } else { 
                    src.eval(&env) 
                }
            });
        
        println!("{}\n", result(res));
        prompt("abyss");
    }
    bye();
    Ok(())
}

