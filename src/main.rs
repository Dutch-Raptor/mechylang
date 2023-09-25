mod errors;
mod evaluator;
mod lexer;
mod parser;
mod repl;
mod tracer;

use std::path::PathBuf;

use repl::repl::Repl;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, about, version)]
struct CommandArgs {
    /// The command to run (will default to REPL)
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Clone)]
enum Command {
    /// Run a file
    File {
        /// The file to run
        file: Option<PathBuf>,
    },
    /// Run the REPL
    Repl,
}

fn main() {
    let args = CommandArgs::parse();

    if let Some(command) = args.command {
        match command {
            Command::File { file } => {
                let file = file.expect("No file specified");
                let file = file.to_str().expect("Invalid file path");

                evaluator::eval_file(file);
            }
            Command::Repl => repl(),
        }
    } else {
        let mut repl = Repl::new();
        repl.run();
    }
}

fn repl() {
    let mut repl = Repl::new();
    repl.run();
}
