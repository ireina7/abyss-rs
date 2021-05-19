mod abyss;
mod parser;
mod logic;

use abyss::repl;
use abyss::config;
use std::thread;



fn run() -> std::io::Result<()> {
    repl()
} 

fn main() -> std::io::Result<()> {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new()
        .stack_size(config::STACK_SIZE)
        .spawn(run)
        .unwrap();

    // Wait for thread to join
    child.join().unwrap()
}

