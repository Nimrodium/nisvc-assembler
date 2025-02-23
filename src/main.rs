use std::process::exit;

use assembler::Assembler;
use colorize::AnsiColor;
use constant::{DEFAULT_BINARY_NAME, NAME};
use data::{AssemblyError, AssemblyErrorCode};

mod assembler;
mod constant;
mod data;
mod package;
mod program_parser;

// will add line number later maybe
fn handle_fatal_assembly_err(err: AssemblyError) -> ! {
    println!(
        "{}: {} {} :: {}",
        NAME,
        "error:".red(),
        format!("{:?}", err.code).yellow(),
        err.reason
    );
    exit(1)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let flags = [
        "-o", // output file name
        "--output",
        "-v", // verbose
        "--verbose",
    ];
    let mut output_file;
    for (i, arg) in args.iter().enumerate() {
        match arg.as_str() {
            "-o" | "--output" => {
                output_file = match args.get(i + 1) {
                    Some(file_name) => file_name,
                    None => handle_fatal_assembly_err(AssemblyError {
                        code: AssemblyErrorCode::CLIArgParseError,
                        reason: format!("missing filename after '{arg}'"),
                    }),
                }
            }
            _ => continue,
        }
    }

    let assembler = Assembler::new();
}
