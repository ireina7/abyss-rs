mod abyss;
mod parser;
mod logic;


use abyss::repl;

/// The main entry
fn main() -> std::io::Result<()> {
    repl()
}
