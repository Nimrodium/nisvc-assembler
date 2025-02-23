use crate::{
    data::{AssemblyError, Labels},
    program_parser::IntermediateProgram,
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
        todo!()
    }
    pub fn load_file(&mut self, file_path: &str) -> Result<(), AssemblyError> {
        // clean file
        // seperate sections
        // merge sections with tokenized_raw_*_sources
        todo!()
    }
    pub fn assemble(&mut self) -> Result<(), AssemblyError> {
        // parse tokenized source and assemble into intermediate
        todo!()
    }
    pub fn resolve() -> Result<(), AssemblyError> {
        // resolve labels within source
        todo!()
    }
    pub fn package() -> Result<Vec<u8>, AssemblyError> {
        // generate machine code
        // package into NISVC Executable Format Binary Image
        todo!()
    }
}
fn clean_source(source: &str) -> Vec<String> {
    todo!()
}

fn seperate_sections(source: &str) -> Result<(Vec<String>, Vec<String>), AssemblyError> {
    todo!()
}
