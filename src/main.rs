use arg_parser::{FlagArg, ParsedCLIArgs};
use assembler::Assembler;
use colorize::AnsiColor;
use constant::{DEFAULT_BINARY_NAME, NAME};
use data::{AssemblyError, AssemblyErrorCode};
use std::{fs::File, io::Write, process::exit};

mod arg_parser;
mod assembler;
mod constant;
mod data;
mod debug_symbols;
mod parser;

static mut DEBUG_SYMBOLS: bool = false;
static mut VERBOSE_FLAG: usize = 0;
// static mut VERY_VERBOSE_FLAG: bool = false;
// static mut VERY_VERY_VERBOSE_FLAG: bool = false;

// will add line number later maybe
fn handle_fatal_assembly_err(err: AssemblyError) -> ! {
    println!("{err}");
    exit(1)
}

fn _verbose_println(msg: &str) {
    unsafe {
        if VERBOSE_FLAG >= 1 {
            println!("{NAME}: {} {}", "verbose:".yellow(), msg)
        }
    }
}
fn _very_verbose_println(msg: &str) {
    unsafe {
        if VERBOSE_FLAG >= 2 {
            println!("{NAME}: {} {}", "very-verbose:".yellow(), msg)
        }
    }
}

fn _very_very_verbose_println(msg: &str) {
    unsafe {
        if VERBOSE_FLAG >= 3 {
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

    let cli_args: Vec<String> = std::env::args().collect();
    let flags = &[
        FlagArg::new("output", 'o', 1),
        FlagArg::new("help", 'h', 0),
        FlagArg::new("verbose", 'v', 0),
    ];
    let flag_definitions: arg_parser::Flags = arg_parser::Flags::new(flags);
    let parsed_args = match ParsedCLIArgs::parse_arguments(&flag_definitions, &cli_args) {
        Ok(args) => args,
        Err(why) => handle_fatal_assembly_err(AssemblyError {
            code: AssemblyErrorCode::CLIArgParseError,
            reason: why,
            metadata: None,
        }),
    };
    println!("{parsed_args:#?}");
    let input_files = parsed_args.raw;
    let mut output_file = DEFAULT_BINARY_NAME;
    let mut verbosity = 0;
    for arg in parsed_args.flags {
        match arg.name {
            "output" => output_file = arg.data[0],
            "help" => help(),
            "verbose" => verbosity += 1,
            _ => panic!("invalid flag snuck past parser"),
        }
    }
    unsafe { VERBOSE_FLAG = verbosity }
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
