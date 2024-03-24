use color_print::cprintln;

use mechylang::{Environment, EvalConfig, Evaluator, Object};
use rustyline::{error::ReadlineError, DefaultEditor};

pub struct Repl {
    exit_pressed: bool,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            exit_pressed: false,
        }
    }

    pub fn run(&mut self) -> rustyline::Result<()> {
        let mut env = Environment::new();
        let mut rl = DefaultEditor::new()?;

        cprintln!("Welcome to mechylang!");

        cprintln!("Type `exit()` to exit");

        loop {
            let readline = rl.readline(">> ");

            // Reset exit_pressed if readline is successful
            if !matches!(
                readline,
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof)
            ) {
                self.exit_pressed = false;
            }

            match readline {
                Ok(line) => {
                    if line.trim() == "exit()" {
                        break;
                    }

                    rl.add_history_entry(line.as_str())?;
                    let evaluated = Evaluator::eval(line, &mut env, EvalConfig::default());
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
                    if self.exit_pressed {
                        break;
                    }
                    cprintln!("Press Ctrl-C again to quit");
                    self.exit_pressed = true;
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
