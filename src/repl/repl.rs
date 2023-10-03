use std::io::{self, Write};

use color_print::cprintln;

use crate::evaluator::{
    environment::Environment,
    eval::{EvalConfig, Evaluator},
};

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

            let evaluated = Evaluator::eval(
                input,
                &mut env,
                EvalConfig::default().with_output(io::stdout()),
            );

            match evaluated {
                Ok(evaluated) => {
                    println!("Evaluated: {}", evaluated);
                }
                Err(errors) => {
                    for error in errors.iter() {
                        cprintln!("{}", error);
                    }
                }
            }
        }
    }
}
