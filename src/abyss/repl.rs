//! Repl of the Abyss programming language

use std::io::prelude::*;
use std::io::{self, BufRead};
use std::fmt;
//use super::env::Environment;
use crate::utils::error::Backtrace;
use super::object::Object;
use super::eval::{self, Eval, Env};



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

fn result_debug<V, E>(res: Result<V, E>) -> String 
    where
        V: fmt::Debug + fmt::Display,
        E: fmt::Debug + fmt::Display,
{
    match res {
        Ok(v)    => format!("{:?}", v  ),
        Err(err) => format!("{:?}", err),
    }
}



pub fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    //let mut input = String::new();
    let mut env = eval::env();
    let mut log = Backtrace::new();
    let env_repl = vec![
        ("quit" , "(lambda () (quit))"),
        ("help" , "(lambda () (help))"),
        ("lazy" , "(lambda () (lazy))"),
        ("debug", "(lambda () (debug))"),
        ("eager", "(lambda () (eager))"),
    ];
    let f = |s: &str| std::rc::Rc::new(s.parse::<Object>().unwrap().eval(&env).unwrap()); // You need to make sure no panic!
    let env_repl = Env::new_from(env_repl.into_iter().map(|(str, src)| (str.to_string(), f(src))).collect());
    env.extend(env_repl.env.into_iter());
    let label = "abyss";
    let mut debug_mode = false;
    let mut eager_mode = false;
    prompt(label);
    for line in stdin.lock().lines() {

        let line = line?;
        if line == "(quit)" {
            break;
        }
        if line == "" {
            prompt(label);
            continue;
        }
        if line == "(help)" {
            println!("The Abyss programming language developed by Ireina.\n");
            prompt(label);
            continue;
        }
        if line == "(debug)" {
            debug_mode = !debug_mode;
            prompt(label);
            continue;
        }
        if line == "(eager)" {
            eager_mode = true;
            prompt(label);
            continue;
        }
        if line == "(lazy)" {
            eager_mode = false;
            prompt(label);
            continue;
        }
        let ast = line.parse::<Object>();
        //println!("{:?} =>", ast);
        let res: Result<Object, _> = ast
            .map_err(|crate::parser::ParseError {msg, ..}| super::eval::EvalError::msg(msg))
            .and_then(|src| {
                if eval::decl::is_decl(&src) { 
                    eval::decl::eval_decl(&src, &mut env).unwrap();
                    Ok(Object::Nil)
                } else { 
                    if eager_mode { eval::strict::evaluate(src, &mut env, &mut log) } else { src.eval(&env) }
                }
            });
        
        println!("{}\n", if debug_mode { result_debug(res) } else { result(res) });
        prompt(label);
    }
    bye();
    Ok(())
}

