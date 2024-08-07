use std::path::PathBuf;

mod repl;
use repl::Repl;

use clap::{Parser, Subcommand};
use mechylang::eval_file;

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
        file: PathBuf,
    },
    /// Run the REPL
    Repl {
        #[arg(long)]
        print_tokens: bool,
        
        #[arg(long)]
        print_ast: bool,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = CommandArgs::parse();

    if let Some(command) = args.command {
        match command {
            Command::File { file } => {
                let file = file.to_str().expect("Invalid file path");

                let res = eval_file(file);

                if let Err(e) = res {
                    for e in e {
                        color_print::cprint!("{}", e);
                    }
                }
            }
            Command::Repl { print_tokens, print_ast } => {
                Repl::new().with_print_ast(print_ast)
                    .with_print_tokens(print_tokens)
                    .run()?;
            }
        }
    } else {
        Repl::new().run()?;
    };

    Ok(())
}
