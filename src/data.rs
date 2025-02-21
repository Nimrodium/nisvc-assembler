use std::collections::HashMap;
pub enum InterType {
    Reg,
    Addr,
    Imm,
    Op,
}
pub enum Severity {
    Info,
    Warning,
    Fatal,
}

pub enum AssemblyErrorCode {
    InvalidOpcodeDefinition,
}

pub struct AssemblyError {
    code: AssemblyErrorCode,
    reason: Option<String>,
    severity: Severity,
}

struct OpcodeEntry {
    code: usize,
    fields: usize,
    field_types: Vec<InterType>,
}
impl OpcodeEntry {
    fn new(code: usize, fields: usize, field_types: Vec<InterType>) -> Result<Self, AssemblyError> {
        if field_types.len() != fields {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidOpcodeDefinition,
                reason: Some(format!(
                    "opcode field count does not align with defined field type length"
                )),
                severity: Severity::Fatal,
            });
        }
        Ok(OpcodeEntry {
            code,
            fields,
            field_types,
        })
    }
}

struct OpcodeTable {
    table: HashMap<String, OpcodeEntry>,
}

impl OpcodeTable {
    fn build_table() -> Result<Self, AssemblyError> {}

    fn fetch_entry(&self, key: &str) -> Result<OpcodeEntry, AssemblyError> {}
}
