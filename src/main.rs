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

impl Args {
    fn filename(&self) -> &PathBuf {
        match &self.command {
            Commands::Evaluate { filename } => filename,
            Commands::Parse { filename } => filename,
            Commands::Tokenize { filename } => filename,
        }
    }
}

#[derive(Subcommand, Debug)]
enum Commands {
    Evaluate { filename: PathBuf },
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

    // Read file
    let filename = args.filename();
    let file_contents = read_file(filename)?;

    let mut lexer = imp::Lexer::new(&file_contents);
    if matches!(args.command, Commands::Tokenize { .. }) {
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
        std::process::exit(if error { 65 } else { 0 });
    }

    let mut parser = imp::Parser::new(&mut lexer);
    let expr = match parser.expression() {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(65);
        }
    };

    if matches!(args.command, Commands::Parse { .. }) {
        println!("{expr}");
        std::process::exit(0);
    }

    match imp::evaluate(expr) {
        Ok(val) => println!("{val}"),
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(70);
        }
    }

    parser.expect_eof()?;

    /*
    if matches!(args.command, Commands::Evaluate { .. }) {
        std::process::exit(0);
    }
    */

    Ok(())
}
