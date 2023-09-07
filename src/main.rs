pub mod compiler;

use std::{fs, os::unix::process::CommandExt, process::Command};

use clap::{Parser, Subcommand};
use compiler::{backend::Backend, lexer::Lexer};

#[derive(Parser)]
#[command(about = "YAJF compiler")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    #[command(
        about = "Compiles a [FILE] and it's dependencies into javascript"
    )]
    Transpile {
        /// Entry file path
        file: String,
        /// Output path
        #[arg(short, long)]
        output: Option<String>,
    },
}

fn read_file_as_string(path: String) -> String {
    fs::read_to_string(path).unwrap()
}

fn write_file(path: String, content: String) {
    fs::write(path, content).unwrap();
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Transpile { file, output }) => {
            let text = read_file_as_string(file.to_string());
            let lexer = Lexer::new(text);
            let parser = compiler::parser::Parser::new(lexer);
            let module = parser.parse();
            let mut backend = Backend::new(module);
            let javascript = backend.transpile();

            let output = output
                .as_ref()
                .unwrap_or(&"bundle.js".to_string())
                .to_string();
            let ir = format!("_o_.{}", output);

            write_file(ir.clone(), javascript);

            Command::new("java")
                .args([
                    "-jar",
                    "./bin/closure.jar",
                    "-O",
                    "ADVANCED",
                    "--js_output_file",
                    &output,
                    &ir,
                ])
                .exec();
        }
        _ => {}
    }
}
