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
            Commands::Program { filename } => filename,
            Commands::Tokenize { filename } => filename,
            Commands::Run { filename } => filename,
        }
    }
}

#[derive(Subcommand, Debug)]
enum Commands {
    Evaluate { filename: PathBuf },
    Parse { filename: PathBuf },
    Program { filename: PathBuf },
    Tokenize { filename: PathBuf },
    Run { filename: PathBuf },
}

fn read_file(filename: &PathBuf) -> miette::Result<String> {
    fs::read_to_string(filename)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", filename.display()))
}

fn unwrap_or_exit<T, E: std::fmt::Debug>(value: Result<T, E>, code: i32) -> T {
    match value {
        Ok(value) => value,
        Err(error) => {
            eprintln!("{error:?}");
            std::process::exit(code);
        }
    }
}

fn main() -> miette::Result<()> {
    env_logger::init();

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

    match args.command {
        Commands::Tokenize { .. } => unreachable!("exited earlier"),
        Commands::Evaluate { .. } | Commands::Parse { .. } => {
            let expr = unwrap_or_exit(parser.expression(), 65);

            if matches!(args.command, Commands::Parse { .. }) {
                println!("{expr}");
                std::process::exit(0);
            }

            let analysis = unwrap_or_exit(imp::Analyzer::new_expression(&expr), 65);
            let val = unwrap_or_exit(imp::Interpreter::new(analysis).expression(&expr), 70);
            println!("{val}");
        }
        Commands::Program { .. } | Commands::Run { .. } => {
            let prog = unwrap_or_exit(parser.program(), 65);

            if matches!(args.command, Commands::Program { .. }) {
                println!("{prog}");
                std::process::exit(0);
            }

            let analysis = unwrap_or_exit(imp::Analyzer::new_block(&prog), 65);
            unwrap_or_exit(imp::Interpreter::new(analysis).block(&prog), 70);
        }
    }

    parser.expect_eof()?;

    Ok(())
}
