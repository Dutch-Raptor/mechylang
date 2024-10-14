use ariadne::Source;
use color_print::cprintln;

use mechylang::{Environment, EvalConfig, Evaluator, Lexer, Object, Parser, TokenKind};
use rustyline::{error::ReadlineError, DefaultEditor};
use mechylang::pretty_errors::PrettyError;

pub struct Repl {
    pub print_ast: bool,
    pub print_tokens: bool,
    pub print_tokens_with_span: bool,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            print_ast: false,
            print_tokens: false,
            print_tokens_with_span: false,
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

    pub fn with_print_tokens_with_span(mut self, print_tokens_with_span: bool) -> Self {
        self.print_tokens_with_span = print_tokens_with_span;
        self
    }

    pub fn run(&self) -> Result<(), Box<dyn std::error::Error>> {


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

                    if self.print_tokens {
                        println!("Tokens: {:?}\n", Lexer::new(&line).map(|result| result.map(|t| t.kind)).collect::<Vec<mechylang::lexer::Result<TokenKind>>>());
                    }

                    if self.print_tokens_with_span {
                        println!("Tokens: {:#?}\n", Lexer::new(&line).map(|token| token.map(|t| format!("{:>24?} {:?}", t.kind, t.span))).collect::<Vec<mechylang::lexer::Result<String>>>());
                    }

                    if self.print_ast {
                        if let Ok(parsed) = Parser::from_source(&line).parse() {
                            println!("Parsed: {}\n", parsed.statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join("\n"));
                            println!("AST: {:#?}\n", parsed.statements);
                        }
                    }


                    let evaluated = Evaluator::eval(
                        &line, &mut env,
                        EvalConfig::default(),
                    );
                    match evaluated {
                        Ok(Object::Unit) => {} // Don't print unit
                        Ok(evaluated) => {
                            println!("{}", evaluated);
                        }
                        Err(error) => {
                            error
                                .as_pretty_error("repl")
                                .eprint(("repl", Source::from(line + " ")))
                                .expect("Expected to be able to print error");
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
