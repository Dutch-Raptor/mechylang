mod compiler;
mod lexer;
mod parser;
mod repl;
mod tracer;

use compiler::compiler::Compiler;
use compiler::compiler::X86_64Intel;
use lexer::lexer::Lexer;
use repl::repl::Repl;

fn main() {
    println!("Welcome to the MechyLang REPL!");

    let mut repl = Repl::new();

    repl.run();
}
