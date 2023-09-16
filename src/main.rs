mod evaluator;
mod lexer;
mod parser;
mod repl;
mod tracer;

use repl::repl::Repl;

fn main() {
    println!("Welcome to the MechyLang REPL!");

    let mut repl = Repl::new();

    repl.run();
}
