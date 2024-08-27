use clap::{Parser, Subcommand};

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

fn main() {
    let args = Args::parse();
    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename.display());
                String::new()
            });

            if !file_contents.is_empty() {
                panic!("Scanner not implemented");
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
    }
}
