use assembler::Assembler;
use colorize::AnsiColor;
use constant::{DEFAULT_BINARY_NAME, NAME, SIGNATURE};
use data::{AssemblyError, AssemblyErrorCode};
use std::process::exit;

mod assembler;
mod constant;
mod data;
mod package;
mod parser;

static mut VERBOSE_FLAG: bool = false;
static mut VERY_VERBOSE_FLAG: bool = false;
static mut VERY_VERY_VERBOSE_FLAG: bool = false;

// will add line number later maybe
fn handle_fatal_assembly_err(err: AssemblyError) -> ! {
    println!("{err}");
    exit(1)
}

fn _verbose_println(msg: &str) {
    unsafe {
        if VERBOSE_FLAG {
            println!("{NAME}: {} {}", "verbose:".yellow(), msg)
        }
    }
}

fn _very_verbose_println(msg: &str) {
    unsafe {
        if VERY_VERBOSE_FLAG {
            println!("{NAME}: {} {}", "very-verbose:".yellow(), msg)
        }
    }
}

fn _very_very_verbose_println(msg: &str) {
    unsafe {
        if VERY_VERY_VERBOSE_FLAG {
            println!("{NAME}: {} {}", "very-very-verbose:".yellow(), msg)
        }
    }
}

#[macro_export]
macro_rules! verbose_println {
    ($($arg:tt)*) => (crate::_verbose_println(&format!($($arg)*)));
}
#[macro_export]
macro_rules! very_verbose_println {
    ($($arg:tt)*) => (crate::_very_verbose_println(&format!($($arg)*)));
}
#[macro_export]
macro_rules! very_very_verbose_println {
    ($($arg:tt)*) => (crate::_very_very_verbose_println(&format!($($arg)*)));
}

fn write_binary(
    binary: Vec<u8>,
    program_length: u64,
    data_length: u64,
    entry_point_address: u64,
) -> Result<(), AssemblyError> {
    let mut nisvc_ef_img: Vec<u8> = vec![];
    nisvc_ef_img.extend_from_slice(SIGNATURE);
    let data_length_bytes = data_length.to_le_bytes();
    let program_length_bytes = program_length.to_le_bytes();
    let entry_point_bytes = entry_point_address.to_le_bytes();
    nisvc_ef_img.extend_from_slice(&program_length_bytes);
    nisvc_ef_img.extend_from_slice(&data_length_bytes);
    nisvc_ef_img.extend_from_slice(&entry_point_bytes);
    nisvc_ef_img.extend(binary.as_slice());
    Ok(())
}
fn help() -> ! {
    println!("Usage: {NAME} [options...] [nisvc-asmfile...]\n\
        Options:\n\
        \t-h,     --help              -- print this message\n\
        \t-v,     --verbose           -- enable verbose printing\n\
        \t-vv,    --very-verbose      -- enable very verbose printing\n\
        \t-vvv,   --very-very-verbose -- enable very very verbose printing\n\
        \t-o,     --output <outfile>  -- output file (when not specified {DEFAULT_BINARY_NAME} will be used)"
    );
    exit(0)
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
                VERBOSE_FLAG = true;
                verbose_println!("verbose print level 1 enabled")
            },
            "-vv" | "--very-verbose" => unsafe {
                VERY_VERBOSE_FLAG = true;
                VERBOSE_FLAG = true;
                very_verbose_println!("verbose print level 2 enabled")
            },
            "-vvv" | "--very-very-verbose" => unsafe {
                VERBOSE_FLAG = true;
                VERY_VERBOSE_FLAG = true;
                VERY_VERY_VERBOSE_FLAG = true;
                very_very_verbose_println!("verbose print level 3 enabled")
            },
            "-h" | "--help" => help(),
            f if f.starts_with("--") || f.starts_with("-") => {
                println!(
                    "{}",
                    AssemblyError {
                        code: AssemblyErrorCode::CLIArgParseError,
                        reason: format!("unrecognized flag :: {f}"),
                    }
                );
                help()
            }
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
        very_verbose_println!("adding source file {file} to assembler");
        match assembler.load_file(file) {
            Ok(()) => (),
            Err(err) => handle_fatal_assembly_err(err),
        };
    }
    match assembler.is_entry_point_located() {
        Ok(()) => verbose_println!(
            "entry point label defined [ {} ]",
            assembler.entry_point.as_ref().unwrap().name
        ),
        Err(err) => handle_fatal_assembly_err(err),
    }
    // generate IR
    match assembler.parse() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    };

    // resolve IR placeholder labels
    match assembler.resolve() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    }
    match assembler.resolve_entry_point() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    };
    // generate nisvc machine code
    let (binary, program_length, data_length) = match assembler.package() {
        Ok(i) => i,
        Err(err) => handle_fatal_assembly_err(err),
    };

    //prepare entrypoint
    let entry_point = match assembler.entry_point.as_ref().unwrap().dereference() {
        Ok(address) => address,
        Err(err) => handle_fatal_assembly_err(err),
    } as u64;

    // write machine code to file
    match write_binary(binary, program_length, data_length, entry_point) {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    }
    verbose_println!("wrote binary file {output_file}");
}
