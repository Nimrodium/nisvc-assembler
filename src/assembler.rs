use std::{fs::File, io::Read};

use crate::{
    constant::{COMMENT, SEPERATOR},
    data::{AssemblyError, AssemblyErrorCode, Labels},
    parser::{self, IntermediateProgram, DEC},
    verbose_println,
};

pub struct Assembler {
    tokenized_raw_data_source: Vec<String>,
    tokenized_raw_program_source: Vec<String>,
    program: Option<IntermediateProgram>,
    data: Option<u8>, // placeholder
    labels: Option<Labels>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            tokenized_raw_data_source: vec![],
            tokenized_raw_program_source: vec![],
            program: None,
            data: None,
            labels: None,
        }
    }
    pub fn load_file(&mut self, file_path: &str) -> Result<(), AssemblyError> {
        // open file
        verbose_println!("opening {file_path}");
        let mut raw_src = String::new();
        File::open(file_path)
            .map_err(|e| AssemblyError {
                code: AssemblyErrorCode::SourceFileInitializationError,
                reason: format!("source file {file_path} could not be opened :: {e}"),
            })?
            .read_to_string(&mut raw_src)
            .map_err(|e| AssemblyError {
                code: AssemblyErrorCode::SourceFileInitializationError,
                reason: format!("source file {file_path} could not be read :: {e}"),
            })?;

        // clean file
        verbose_println!("cleaning {file_path}");
        let clean_src = clean_source(&raw_src);

        // seperate sections
        let (clean_data, clean_program) = seperate_sections(&clean_src)?;

        // merge sections with tokenized_raw_*_sources
        self.tokenized_raw_data_source
            .extend_from_slice(&clean_data);
        self.tokenized_raw_program_source
            .extend_from_slice(&clean_program);
        Ok(())
    }
    pub fn parse(&mut self) -> Result<(), AssemblyError> {
        // parse tokenized source and assemble into intermediate

        // process data
        self.data = Some(0); // placeholder

        // process program
        let intermediate_program =
            parser::IntermediateProgram::parse_program(&self.tokenized_raw_program_source)?;
        Ok(())
    }
    pub fn resolve(&mut self) -> Result<(), AssemblyError> {
        // resolve labels within source
        todo!()
    }
    pub fn package(&self) -> Result<(Vec<u8>, u64, u64), AssemblyError> {
        // generate machine code
        // package into NISVC Executable Format Binary Image
        todo!()
    }
}
fn clean_source(source: &str) -> Vec<String> {
    let mut clean_buf: Vec<String> = vec![];
    for line in source.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            continue;
        }
        let line_no_comments = match line.split(COMMENT).nth(0) {
            Some(no_comments) => no_comments,
            None => continue, // line was entirely comment
        }
        .trim();
        let instructions_of_line: Vec<String> = line_no_comments
            .split(SEPERATOR)
            .map(|s| s.to_string())
            .collect();
        for instruction in instructions_of_line {
            let clean_instruction = instruction.trim();
            if !clean_instruction.is_empty() {
                verbose_println!("{clean_instruction}");
                clean_buf.push(clean_instruction.to_string());
            }
        }
    }
    clean_buf
}

const DATA_MARKER: &str = ".data";
const PROGRAM_MARKER: &str = ".program";
enum Section {
    Data,
    Program,
    None,
}
fn seperate_sections(source: &Vec<String>) -> Result<(Vec<String>, Vec<String>), AssemblyError> {
    let mut data_section: Vec<String> = vec![];
    let mut program_section: Vec<String> = vec![];
    let mut section: Section = Section::None;
    for token in source {
        // match section {
        //     Section::Data => verbose_println!("in data"),
        //     Section::Program => verbose_println!("in program"),
        //     Section::None => verbose_println!("not in a section"),
        // }
        match token.as_str() {
            DATA_MARKER => section = Section::Data,
            PROGRAM_MARKER => section = Section::Program,
            t if t.starts_with(".") => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: format!("malformed section label {t}"),
                })
            }
            _ => match section {
                Section::Data => data_section.push(token.clone()),
                Section::Program => program_section.push(token.clone()),
                Section::None => {
                    return Err(AssemblyError {
                        code: AssemblyErrorCode::SyntaxError,
                        reason: format!("token [ {token} ] found not associated with a section"),
                    })
                }
            },
        }
    }
    Ok((data_section, program_section))
}
