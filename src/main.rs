use std::{fmt, fs::File, io::Write};

use assembler::Assembler;
use colorize::AnsiColor;
use emmitter::package;
use tokenizer::{tokenize, Lexeme, Source};

mod assembler;
mod data;
mod emmitter;
mod tokenizer;

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
            panic!()
        }
    }
}

fn real_main() -> Result<(), AssembleError> {
    let path = "nisvc.out";
    let mut sources = Source::new();
    sources.open_file("test.nsm")?;
    let tokens = tokenize(&sources)?;
    println!("{tokens:#?}");
    let (entry_point, data, program, debug_symbols) =
        Assembler::assemble(tokens).map_err(|e| e.traceback(&sources))?;
    let exe_package = package(entry_point, data, program, debug_symbols);
    let mut out = File::create(path)
        .map_err(|e| AssembleError::new(format!("failed to create {path}: {e}")))?;
    out.write_all(&exe_package)
        .map_err(|e| AssembleError::new(format!("failed to write to {path}: {e}")))?;
    Ok(())
}
