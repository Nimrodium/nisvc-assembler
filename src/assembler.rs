use std::{fs::File, io::Read, ops::Index};

use crate::{
    constant::{
        COMMENT, DATA_MARKER, ENTRY_MARKER, MMIO_ADDRESS_SPACE, OPCODE_BYTES, PROGRAM_MARKER,
        SEPERATOR, SIGNATURE,
    },
    data::{AssemblyError, AssemblyErrorCode, Label, LabelLocation, Labels, MetaData},
    parser::{self, Data, IntermediateProgram},
    verbose_println, very_verbose_println, very_very_verbose_println,
};

pub struct Assembler {
    tokenized_raw_data_source: Vec<MetaData>,
    tokenized_raw_program_source: Vec<MetaData>,
    pub program: Option<IntermediateProgram>,
    pub data: Option<Data>,
    pub labels: Labels,
    pub entry_point: Option<Label>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            tokenized_raw_data_source: vec![],
            tokenized_raw_program_source: vec![],
            program: None,
            data: None,
            labels: Labels::new(),
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
                metadata: None,
            })?
            .read_to_string(&mut raw_src)
            .map_err(|e| AssemblyError {
                code: AssemblyErrorCode::SourceFileInitializationError,
                reason: format!("source file {file_path} could not be read :: {e}"),
                metadata: None,
            })?;

        // clean file
        very_verbose_println!("cleaning {file_path}");
        let clean_src = clean_source(&raw_src, file_path);

        // seperate sections
        let (clean_data, clean_program, entry_point_label) = seperate_sections(&clean_src)?;
        if entry_point_label.is_some() {
            if self.entry_point.is_none() {
                self.entry_point = Some(Label::new(
                    &entry_point_label.unwrap(),
                    LabelLocation::Program,
                    false,
                ));
            } else {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: "multiple definitions of entry point".to_string(),
                    metadata: None,
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
                metadata: None,
            })
        } else {
            Ok(())
        }
    }
    pub fn parse(&mut self) -> Result<(), AssemblyError> {
        // parse tokenized source and assemble into intermediate
        let mut data = Data::new();
        data.parse(&self.tokenized_raw_data_source)?;

        // self.labels.extend_from_self(&data.labels); // inefficient
        // verbose_println!("data slabels: {:?}", self.labels.table);
        self.data = Some(data);
        // process program

        self.program = Some(parser::IntermediateProgram::parse_program(
            &self.tokenized_raw_program_source,
        )?);
        // process data

        Ok(())
    }

    pub fn resolve(&mut self) -> Result<(), AssemblyError> {
        // resolve labels within source
        let program_length = if let Some(program) = &mut self.program {
            program.resolve_immediates(&self.data.as_ref().unwrap().labels)?;
            let size = program.estimate_program_size()?;
            program.collect_program_labels(&self.tokenized_raw_program_source)?;
            size
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::UnexpectedError,
                reason: "program not parsed".to_string(),
                metadata: None,
            });
        };

        self.program.as_mut().unwrap().resolve_program_addresses()?;
        if let Some(data) = &mut self.data {
            data.shift_addresses(MMIO_ADDRESS_SPACE + program_length)?
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::UnexpectedError,
                reason: "data not parsed".to_string(),
                metadata: None,
            });
        }
        self.program
            .as_mut()
            .unwrap()
            .resolve_ram_addresses(&self.data.as_ref().unwrap().labels)?;

        Ok(())
    }
    /// attempts to look up the entry point in the label table and resolves it,
    /// errors if label is not in program or unresolved
    pub fn resolve_entry_point(&mut self) -> Result<(), AssemblyError> {
        let label = self
            .program
            .as_ref()
            .unwrap()
            .labels
            .get_label(&self.entry_point.as_ref().unwrap().name)?;
        if !label.is_in(LabelLocation::Program) {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidEntryPoint,
                reason: format!("{} is not a valid entry point label, label was found but in an invalid location [ {:?} ]", self.entry_point.as_ref().unwrap(),label.label_location),metadata:None,
            });
        } else {
            self.entry_point
                .as_mut()
                .unwrap()
                .resolve(label.dereference()?);
        }
        Ok(())
    }

    pub fn package(&self) -> Result<Vec<u8>, AssemblyError> {
        // generate machine code
        let (program_code, debug_image) = self.program.as_ref().unwrap().to_bytes()?;
        let data_image = &self.data.as_ref().unwrap().data_image;

        // package into NISVC Executable Format Binary Image
        let mut image: Vec<u8> = vec![];
        let data_length = data_image.len();
        verbose_println!("data image length : {}", data_length);

        let data_length_bytes = ((data_length) as u64).to_le_bytes();
        let program_length_bytes =
            ((self.program.as_ref().unwrap().size.unwrap() + OPCODE_BYTES) as u64).to_le_bytes();
        let entry_point_bytes =
            (self.entry_point.as_ref().unwrap().dereference()? as u64).to_le_bytes();
        let debug_image_length_bytes = debug_image.len().to_le_bytes();
        let mut end_of_execution_code: Vec<u8> = vec![];
        for _ in 0..OPCODE_BYTES {
            end_of_execution_code.push(0xFF);
        }
        // header
        image.extend_from_slice(SIGNATURE);
        image.extend_from_slice(&program_length_bytes);
        image.extend_from_slice(&data_length_bytes);
        image.extend_from_slice(&entry_point_bytes);
        image.extend_from_slice(&debug_image_length_bytes);
        // data
        image.extend_from_slice(&program_code);
        image.extend_from_slice(&end_of_execution_code);

        image.extend_from_slice(data_image);
        image.extend_from_slice(&debug_image);
        let header = SIGNATURE.len() + (8 * 4);
        let eoe_loc = header + program_code.len();
        let end_of_exec_align_check = image.get(eoe_loc);
        if end_of_exec_align_check.is_some() {
            if *end_of_exec_align_check.unwrap() != 0xFF {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UnexpectedError,
                    reason: format!(
                        "could not find end of exec byte at [ {eoe_loc} ], read [ {:#x} ]",
                        end_of_exec_align_check.unwrap()
                    ),
                    metadata: None,
                });
            }
        }
        Ok(image)
    }

    // let mut nisvc_ef_img: Vec<u8> = vec![];
    // nisvc_ef_img.extend_from_slice(SIGNATURE);
    // let data_length_bytes = data_length.to_le_bytes();
    // let program_length_bytes = program_length.to_le_bytes();
    // let entry_point_bytes = entry_point_address.to_le_bytes();
    // nisvc_ef_img.extend_from_slice(&program_length_bytes);
    // nisvc_ef_img.extend_from_slice(&data_length_bytes);
    // nisvc_ef_img.extend_from_slice(&entry_point_bytes);
    // nisvc_ef_img.extend(binary.as_slice());
}
fn clean_source(source: &str, file_name: &str) -> Vec<MetaData> {
    let mut clean_buf: Vec<MetaData> = vec![];
    for (line_number, line) in source.lines().enumerate() {
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
                // clean_buf.push(clean_instruction.to_string());
                let metadata = MetaData {
                    text: clean_instruction.to_string(),
                    file: file_name.to_string(),
                    line: line_number + 1,
                };
                clean_buf.push(metadata);
            }
        }
    }
    clean_buf
}

enum Section {
    Data,
    Program,
    Entry,
    None,
}
fn seperate_sections(
    source: &[MetaData],
) -> Result<(Vec<MetaData>, Vec<MetaData>, Option<String>), AssemblyError> {
    let mut data_section: Vec<MetaData> = vec![];
    let mut program_section: Vec<MetaData> = vec![];
    let mut section: Section = Section::None;
    let mut entry_point_label: Option<String> = None;
    for line in source {
        match line.text.as_str() {
            DATA_MARKER => section = Section::Data,
            PROGRAM_MARKER => section = Section::Program,
            ENTRY_MARKER => section = Section::Entry,
            t if t.starts_with(".") => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: format!("malformed section label {t}"),
                    metadata: Some(line.clone()),
                })
            }
            _ => match section {
                Section::Data => data_section.push(line.clone()),
                Section::Program => program_section.push(line.clone()),
                Section::Entry => {
                    if entry_point_label.is_none() {
                        entry_point_label = Some(line.text[1..].to_string())
                    } else {
                        return Err(AssemblyError {
                            code: AssemblyErrorCode::SyntaxError,
                            reason: format!("multiple definitions of entry point :: attempted to set [ {line} ] as entry point when entry point was already set as [ {} ]",entry_point_label.unwrap()),
                            metadata:Some(line.clone()),
                        });
                    }
                }
                Section::None => {
                    return Err(AssemblyError {
                        code: AssemblyErrorCode::SyntaxError,
                        reason: format!("token [ {line} ] found not associated with a section"),
                        metadata: Some(line.clone()),
                    })
                }
            },
        }
    }
    Ok((data_section, program_section, entry_point_label))
}
