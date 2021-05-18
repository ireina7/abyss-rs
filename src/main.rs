mod abyss;
mod parser;
mod logic;

use abyss::repl;
use std::thread;



const STACK_SIZE: usize = 100 * 1024 * 1024;

fn run() -> std::io::Result<()> {
    repl()
} 

fn main() -> std::io::Result<()> {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap();

    // Wait for thread to join
    child.join().unwrap()
}

