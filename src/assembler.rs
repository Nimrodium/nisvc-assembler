use std::{fs::File, io::Read};

use crate::{
    constant::{COMMENT, SEPERATOR},
    data::{AssemblyError, AssemblyErrorCode, Label, Labels},
    parser::{self, IntermediateProgram, DEC},
    verbose_println, very_verbose_println, very_very_verbose_println,
};

pub struct Assembler {
    tokenized_raw_data_source: Vec<String>,
    tokenized_raw_program_source: Vec<String>,
    program: Option<IntermediateProgram>,
    data: Option<u8>, // placeholder
    labels: Option<Labels>,
    pub entry_point: Option<Label>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            tokenized_raw_data_source: vec![],
            tokenized_raw_program_source: vec![],
            program: None,
            data: None,
            labels: None,
            entry_point: None,
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
        very_verbose_println!("cleaning {file_path}");
        let clean_src = clean_source(&raw_src);

        // seperate sections
        let (clean_data, clean_program, entry_point_label) = seperate_sections(&clean_src)?;
        if entry_point_label.is_some() {
            if self.entry_point.is_none() {
                self.entry_point = Some(Label::new(&entry_point_label.unwrap()));
            } else {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: "multiple definitions of entry point".to_string(),
                });
            }
        }
        // merge sections with tokenized_raw_*_sources
        self.tokenized_raw_data_source
            .extend_from_slice(&clean_data);
        self.tokenized_raw_program_source
            .extend_from_slice(&clean_program);
        Ok(())
    }
    pub fn is_entry_point_located(&self) -> Result<(), AssemblyError> {
        if self.entry_point.is_none() {
            Err(AssemblyError {
                code: AssemblyErrorCode::SyntaxError,
                reason: "entry point never defined".to_string(),
            })
        } else {
            Ok(())
        }
    }
    pub fn parse(&mut self) -> Result<(), AssemblyError> {
        // parse tokenized source and assemble into intermediate

        // process data
        self.data = Some(0); // placeholder

        // process program
        self.program = Some(parser::IntermediateProgram::parse_program(
            &self.tokenized_raw_program_source,
        )?);
        Ok(())
    }

    pub fn resolve(&mut self) -> Result<(), AssemblyError> {
        // resolve labels within source
        let program_labels = if let Some(program) = &self.program {
            program.collect_program_labels(&self.tokenized_raw_program_source)?
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::UnexpectedError,
                reason: "program not parsed".to_string(),
            });
        };

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
                very_very_verbose_println!("{clean_instruction}");
                clean_buf.push(clean_instruction.to_string());
            }
        }
    }
    clean_buf
}

const DATA_MARKER: &str = ".data";
const PROGRAM_MARKER: &str = ".program";
const ENTRY_MAKER: &str = ".entry";

enum Section {
    Data,
    Program,
    Entry,
    None,
}
fn seperate_sections(
    source: &Vec<String>,
) -> Result<(Vec<String>, Vec<String>, Option<String>), AssemblyError> {
    let mut data_section: Vec<String> = vec![];
    let mut program_section: Vec<String> = vec![];
    let mut section: Section = Section::None;
    let mut entry_point_label: Option<String> = None;
    for token in source {
        match token.as_str() {
            DATA_MARKER => section = Section::Data,
            PROGRAM_MARKER => section = Section::Program,
            ENTRY_MAKER => section = Section::Entry,
            t if t.starts_with(".") => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: format!("malformed section label {t}"),
                })
            }
            _ => match section {
                Section::Data => data_section.push(token.clone()),
                Section::Program => program_section.push(token.clone()),
                Section::Entry => {
                    if entry_point_label.is_none() {
                        entry_point_label = Some(token[1..].to_string())
                    } else {
                        return Err(AssemblyError {
                            code: AssemblyErrorCode::SyntaxError,
                            reason: format!("multiple definitions of entry point :: attempted to set [ {token} ] as entry point when entry point was already set as [ {} ]",entry_point_label.unwrap()),
                        });
                    }
                }
                Section::None => {
                    return Err(AssemblyError {
                        code: AssemblyErrorCode::SyntaxError,
                        reason: format!("token [ {token} ] found not associated with a section"),
                    })
                }
            },
        }
    }
    // let entry_point_return = if let Some(label) = entry_point_label {
    //     label
    // } else {
    //     return Err(AssemblyError {
    //         code: AssemblyErrorCode::SyntaxError,
    //         reason: "entry point never defined".to_string(),
    //     });
    // };
    Ok((data_section, program_section, entry_point_label))
}
