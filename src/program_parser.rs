use crate::data::{AssemblyError, InterType, Severity};

struct IntermediateObject {
    intertype: InterType,
    object: usize,
    string: Option<String>,
    size: usize,
    is_resolved: bool,
}

impl IntermediateObject {
    fn from_mnemonic(s: &str) -> Result<Self, AssemblyError> {
        todo!()
    }
    fn to_bytes(&self) -> Vec<u8> {
        todo!()
    }
}

struct IntermediateInstruction {
    objects: Vec<IntermediateObject>,
}
impl IntermediateInstruction {
    fn parse_string(s: &str) -> Result<Self, AssemblyError> {
        todo!()
    }

    fn to_bytes(&self) -> Vec<u8> {
        todo!()
    }
}

pub fn parse_program(program_raw: String) -> Result<Vec<IntermediateInstruction>, AssemblyError> {}
