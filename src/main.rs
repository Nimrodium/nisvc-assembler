use std::{alloc::handle_alloc_error, process::exit};

use assembler::Assembler;
use colorize::AnsiColor;
use constant::{DEFAULT_BINARY_NAME, NAME, SIGNATURE};
use data::{AssemblyError, AssemblyErrorCode};

mod assembler;
mod constant;
mod data;
mod package;
mod parser;

static mut VERBOSE_FLAG: bool = false;

// will add line number later maybe
fn handle_fatal_assembly_err(err: AssemblyError) -> ! {
    println!(
        "{NAME}: {} {} :: {}",
        "error:".red(),
        format!("{:?}", err.code).yellow(),
        err.reason
    );
    exit(1)
}

fn _verbose_println(msg: &str) {
    unsafe {
        if VERBOSE_FLAG {
            println!("{NAME}: {} {}", "verbose:".yellow(), msg)
        }
    }
}

#[macro_export]
macro_rules! verbose_println {
    ($($arg:tt)*) => (crate::_verbose_println(&format!($($arg)*)));
}

fn write_binary(
    binary: Vec<u8>,
    program_length: u64,
    data_length: u64,
) -> Result<(), AssemblyError> {
    let mut nisvc_ef_img: Vec<u8> = vec![];
    nisvc_ef_img.extend_from_slice(SIGNATURE);
    let data_length_bytes = data_length.to_le_bytes();
    let program_length_bytes = program_length.to_le_bytes();
    nisvc_ef_img.extend_from_slice(&program_length_bytes);
    nisvc_ef_img.extend_from_slice(&data_length_bytes);
    nisvc_ef_img.extend(binary.as_slice());
    Ok(())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut output_file = DEFAULT_BINARY_NAME;
    let mut skips = 1;
    let mut input_files: Vec<&String> = vec![];
    for (i, arg) in args.iter().enumerate() {
        if skips > 0 {
            skips -= 1;
            continue;
        }
        match arg.as_str() {
            "-o" | "--output" => {
                output_file = match args.get(i + 1) {
                    Some(file_name) => file_name,
                    None => handle_fatal_assembly_err(AssemblyError {
                        code: AssemblyErrorCode::CLIArgParseError,
                        reason: format!("missing filename after '{arg}'"),
                    }),
                };
                // verbose_println!("output file set to {output_file}");
                skips += 1;
            }
            "-v" | "--verbose" => unsafe {
                if VERBOSE_FLAG == true {
                    handle_fatal_assembly_err(AssemblyError {
                        code: AssemblyErrorCode::CLIArgParseError,
                        reason: format!("verbose flag set twice'{arg}'"),
                    })
                }
                VERBOSE_FLAG = true
            },
            _ => input_files.push(arg),
        }
    }

    verbose_println!("output file: {output_file}");
    // println!("g");
    if input_files.len() < 1 {
        handle_fatal_assembly_err(AssemblyError {
            code: AssemblyErrorCode::CLIArgParseError,
            reason: "no input files".to_string(),
        })
    }

    let mut assembler = Assembler::new();
    for file in input_files {
        verbose_println!("adding source file {file} to assembler");
        match assembler.load_file(file) {
            Ok(()) => (),
            Err(err) => handle_fatal_assembly_err(err),
        };
    }
    match assembler.parse() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    };
    verbose_println!("parsed program");
    match assembler.resolve() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    }
    verbose_println!("resolve");
    let (binary, program_length, data_length) = match assembler.package() {
        Ok(i) => i,
        Err(err) => handle_fatal_assembly_err(err),
    };
    verbose_println!("package");
    match write_binary(binary, program_length, data_length) {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    }
    verbose_println!("wrote binary file {output_file}");
}
