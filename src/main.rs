use assembler::Assembler;
use colorize::AnsiColor;
use constant::{DEFAULT_BINARY_NAME, NAME};
use data::{AssemblyError, AssemblyErrorCode};
use std::{fs::File, io::Write, process::exit};

mod assembler;
mod constant;
mod data;
mod debug_symbols;
mod parser;

static mut DEBUG_SYMBOLS: bool = false;
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

fn write_file(image: &[u8], output_file: &str) -> Result<(), AssemblyError> {
    let mut outf = match File::create(output_file) {
        Ok(f) => f,
        Err(err) => {
            return Err(AssemblyError {
                code: AssemblyErrorCode::OutputWriteError,
                reason: format!("error opening file {output_file} :: {err}"),
                metadata: None,
            })
        }
    };
    match outf.write_all(image) {
        Ok(()) => (),
        Err(err) => {
            return Err(AssemblyError {
                code: AssemblyErrorCode::OutputWriteError,
                reason: format!("error writing to file {output_file} :: {err}"),
                metadata: None,
            })
        }
    };
    Ok(())
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
    let table = crate::data::RegisterTable::build_table();
    let r1b1_code = table.get_reg("r4q4").unwrap();
    println!("{r1b1_code:#x}");
    for entry in &table.table {
        println!("{entry:x?}");
    }

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
                        metadata: None,
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
                        metadata: None,
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
            "-d" | "--debug-symbols" => unsafe { DEBUG_SYMBOLS = true },
            "-h" | "--help" => help(),
            f if f.starts_with("--") || f.starts_with("-") => {
                println!(
                    "{}",
                    AssemblyError {
                        code: AssemblyErrorCode::CLIArgParseError,
                        reason: format!("unrecognized flag :: {f}"),
                        metadata: None,
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
            metadata: None,
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
    verbose_println!("passed checkpoint PARSED");
    // resolve IR placeholder labels
    match assembler.resolve() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    }

    match assembler.resolve_entry_point() {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    };
    verbose_println!("passed checkpoint RESOLVED");

    very_verbose_println!("program:\n{}", assembler.program.clone().unwrap());
    // generate nisvc machine code
    let nisvc_ef = match assembler.package() {
        Ok(i) => i,
        Err(err) => handle_fatal_assembly_err(err),
    };
    // write machine code to file
    match write_file(&nisvc_ef, output_file) {
        Ok(()) => (),
        Err(err) => handle_fatal_assembly_err(err),
    }
    println!("wrote binary file {output_file}");
}
