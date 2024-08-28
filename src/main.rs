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
    Parse { filename: PathBuf },
    Tokenize { filename: PathBuf },
}

fn read_file(filename: &PathBuf) -> miette::Result<String> {
    fs::read_to_string(filename)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filename.display()))
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::Parse { filename } => {
            let file_contents = read_file(&filename)?;
            let mut lexer = imp::Lexer::new(&file_contents);
            let mut parser = imp::Parser::new(&mut lexer);

            match parser.expression() {
                Ok(expr) => println!("{expr}"),
                Err(e) => {
                    eprintln!("{e:?}");
                    std::process::exit(65);
                }
            }

            parser.expect_eof()?;
        }

        Commands::Tokenize { filename } => {
            let file_contents = read_file(&filename)?;
            let lexer = imp::Lexer::new(&file_contents);

            let mut error = false;
            for token in lexer {
                match token {
                    Ok(token) => println!("{token}"),
                    Err(e) => {
                        error = true;
                        eprintln!("{e}"); // Simple for test requirements
                    }
                }
            }
            println!("EOF  null");
            if error {
                std::process::exit(65);
            };
        }
    }
    Ok(())
}
