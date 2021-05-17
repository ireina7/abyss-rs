mod abyss;
#[macro_use] mod parser;
mod logic;


use abyss::repl::repl;

/// The main entry
fn main() -> std::io::Result<()> {
    repl()
}
