use assembler::Assembler;
use clap::Parser;
use colorize::AnsiColor;
use emmitter::package;
use std::{fmt, fs::File, io::Write, process::exit};
use tokenizer::{Lexeme, Source};

mod assembler;
mod emmitter;
mod instruction;
mod tokenizer;
const DEFAULT_STDLIB: &str = "~/.nisvc/stdlib/";
#[derive(Debug)]
pub struct AssembleError {
    error: String,
    lexeme: Option<Lexeme>,
    trace: Option<String>,
}
impl AssembleError {
    pub fn new(error: String) -> Self {
        Self {
            error,
            lexeme: None,
            trace: None,
        }
    }
    pub fn attach_lexeme(mut self, lexeme: &Lexeme) -> Self {
        self.lexeme = Some(lexeme.clone());
        self
    }
    pub fn traceback(mut self, source: &Source) -> Self {
        if let Some(l) = &self.lexeme {
            self.trace = Some(source.traceback(l));
            self
        } else {
            self
        }
    }
}

impl fmt::Display for AssembleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let error = format!(
        //     "{} {}\n{}",
        //     "error:".redb(),
        //     self.error,
        //     if let Some(trace) = self.trace {
        //         &trace
        //     } else {
        //         ""
        //     }
        // );
        write!(
            f,
            "{} {}\n{}",
            "error:".redb(),
            self.error.clone().yellow(),
            if let Some(trace) = &self.trace {
                trace
            } else {
                ""
            }
        )
    }
}
fn main() {
    match real_main() {
        Ok(()) => (),
        Err(e) => {
            println!("{e}");
            exit(1)
        }
    }
}

#[derive(Parser, Debug)]
#[command(version,about,long_about=None)]
struct Args {
    #[arg(short, long,default_value_t = String::from("nisvc.out"))]
    output_file: String,
    #[arg(short, long, default_value_t = 0)]
    verbosity: usize,
    #[arg()]
    sources: Vec<String>,
    #[arg(short, long)]
    stdlib: Option<String>,
}

fn real_main() -> Result<(), AssembleError> {
    let args = Args::parse();
    // println!("{args:?}");
    let path = args.output_file;
    // let mut sources = Source::new();
    // for file in args.sources {
    //     sources.open_file(&file)?;
    // }
    // let tokens = tokenize(&sources)?;
    // let (entry_point, data, program, debug_symbols) =
    //     Assembler::assemble(tokens).map_err(|e| e.traceback(&sources))?;
    if args.sources.is_empty() {
        return Err(AssembleError::new("no source files provided".to_string()));
    }
    let mut assembler = Assembler::new(args.stdlib);
    for file in args.sources {
        assembler.parse_file(&file)?;
    }
    assembler
        .resolve()
        .map_err(|e| e.traceback(&assembler.sources))?;
    let entry_point = if let Some(ep) = assembler.entry_point {
        ep
    } else {
        return Err(AssembleError::new("entry point not declared".to_string()));
    };
    let program = assembler.emit();
    let debug_symbols = assembler.build_debug_symbol_table();
    let exe_package = package(entry_point, assembler.data, program, debug_symbols);
    let mut out = File::create(&path)
        .map_err(|e| AssembleError::new(format!("failed to create {path}: {e}")))?;
    out.write_all(&exe_package)
        .map_err(|e| AssembleError::new(format!("failed to write to {path}: {e}")))?;
    println!("built nisvc executable `{}`", path);
    Ok(())
}
