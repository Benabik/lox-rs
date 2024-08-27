use clap::{Parser, Subcommand};
use codecrafters_interpreter as imp;
use miette::{IntoDiagnostic, WrapErr};

use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let lexer = imp::Lexer::new(&file_contents);
            let mut error = false;
            for token in lexer {
                match token {
                    Ok(token) => println!("{token}"),
                    Err(e) => {
                        error = true;
                        eprintln!("{e}"); // Simple for test requirements
                        eprintln!("{e:?}"); // Miette details
                    }
                }
            }
            println!("EOF  null");
            if error {
                std::process::exit(65);
            }
            Ok(())
        }
    }
}
