mod abyss;
#[macro_use] mod parser;

use std::io::prelude::*;
use std::io::{self, Read, BufRead};


fn main() -> io::Result<()> {
    let stdin = io::stdin();
    //let mut input = String::new();
    print!("abyss < ");
    io::stdout().flush().ok().expect("Could not flush stdout");
    for line in stdin.lock().lines() {

        let line = line.unwrap();
        if line == "quit" {
            break;
        }
        let ast = line.parse::<abyss::Object>();
        println!("{:?}\n", ast);
        print!("abyss < ");
        io::stdout().flush().ok().expect("Could not flush stdout");
    }
    Ok(())
}
