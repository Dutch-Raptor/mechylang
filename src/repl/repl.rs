use std::io::{self, Write};

use color_print::cprintln;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;

pub struct Repl;

impl Repl {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&mut self) {
        loop {
            print!(">> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();

            let lexer = Lexer::new(input);

            let mut parser = Parser::new(lexer);

            let program = parser.parse();

            println!("");

            println!("AST: {:#?}", program.statements);

            if program.errors.len() > 0 {
                cprintln!(
                    "<red>Parsing completed with <strong>{}</strong> errors</red>",
                    program.errors.len()
                );
                for (i, error) in program.errors.iter().enumerate() {
                    cprintln!("<r>{}:</> {}", i + 1, error);
                }
                continue;
            } else {
                cprintln!("<green>Parsing completed successfully</green>");
            }

            println!("TEXT: {}", program);
        }
    }
}
