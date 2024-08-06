use color_print::cprintln;

use mechylang::{Environment, EvalConfig, Evaluator, Object};
use rustyline::{error::ReadlineError, DefaultEditor};

pub struct Repl {
    pub print_ast: bool,
    pub print_tokens: bool,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            print_ast: false,
            print_tokens: false,
        }
    }

    pub fn with_print_ast(mut self, print_ast: bool) -> Self {
        self.print_ast = print_ast;
        self
    }

    pub fn with_print_tokens(mut self, print_tokens: bool) -> Self {
        self.print_tokens = print_tokens;
        self
    }

    pub fn run(&self) -> rustyline::Result<()> {
        let mut env = Environment::new();
        let mut rl = DefaultEditor::new()?;

        let mut exit_pressed = false;

        cprintln!("Welcome to mechylang!");

        cprintln!("Type `exit()` to exit");

        loop {
            let readline = rl.readline(">> ");

            // Reset exit_pressed if readline is successful
            if !matches!(
                readline,
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof)
            ) {
                exit_pressed = false;
            }

            match readline {
                Ok(line) => {
                    if line.trim() == "exit()" {
                        break;
                    }

                    rl.add_history_entry(line.as_str())?;
                    let evaluated = Evaluator::eval(
                        line, &mut env,
                        EvalConfig::default()
                            .with_print_ast(self.print_ast)
                            .with_print_tokens(self.print_tokens),
                    );
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
                Err(ReadlineError::Interrupted) => {
                    if exit_pressed {
                        break;
                    }
                    cprintln!("Press Ctrl-C again to quit");
                    exit_pressed = true;
                    continue;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }

        cprintln!("Bye!");
        Ok(())
    }
}
