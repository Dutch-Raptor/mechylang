use std::io::{self, Write};

use color_print::cprintln;

use mechylang::{Environment, EvalConfig, Evaluator, Object};

pub struct Repl;

impl Repl {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&mut self) {
        let mut env = Environment::new();
        loop {
            print!(">> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();

            let evaluated = Evaluator::eval(input, &mut env, EvalConfig::default());

            match evaluated {
                Ok(Object::Unit) => {} // Don't print unit
                Ok(evaluated) => {
                    println!("{}", evaluated);
                }
                Err(errors) => {
                    cprintln!("{}", errors)
                }
            }
        }
    }
}
