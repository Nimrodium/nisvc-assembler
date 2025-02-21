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
    UnrecognizedOpcode,
    IncorrectNumberOfOperands,
}

pub struct AssemblyError {
    pub code: AssemblyErrorCode,
    pub reason: Option<String>,
    pub severity: Severity,
}

pub struct OpcodeEntry {
    pub code: usize,
    pub fields: usize,
    pub field_types: Vec<InterType>,
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

pub struct OpcodeTable {
    table: HashMap<String, OpcodeEntry>,
}

impl OpcodeTable {
    pub fn build_table() -> Result<Self, AssemblyError> {
        let table: HashMap<String, OpcodeEntry> = HashMap::from([
            ("nop".to_string(), OpcodeEntry::new(0x00, 0, vec![])?),
            (
                "mov".to_string(),
                OpcodeEntry::new(0x01, 2, vec![InterType::Reg, InterType::Reg])?,
            ),
            (
                "movim".to_string(),
                OpcodeEntry::new(0x02, 2, vec![InterType::Reg, InterType::Imm])?,
            ),
            (
                "load".to_string(),
                OpcodeEntry::new(
                    0x03,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "store".to_string(),
                OpcodeEntry::new(
                    0x04,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "add".to_string(),
                OpcodeEntry::new(
                    0x05,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "sub".to_string(),
                OpcodeEntry::new(
                    0x06,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "mult".to_string(),
                OpcodeEntry::new(
                    0x07,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "div".to_string(),
                OpcodeEntry::new(
                    0x08,
                    3,
                    vec![
                        InterType::Reg,
                        InterType::Reg,
                        InterType::Reg,
                        InterType::Reg,
                    ],
                )?,
            ),
            (
                "or".to_string(),
                OpcodeEntry::new(
                    0x09,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "xor".to_string(),
                OpcodeEntry::new(
                    0x0a,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "and".to_string(),
                OpcodeEntry::new(
                    0x0b,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "not".to_string(),
                OpcodeEntry::new(0x0c, 2, vec![InterType::Reg, InterType::Reg])?,
            ),
            (
                "shl".to_string(),
                OpcodeEntry::new(
                    0x0d,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "shr".to_string(),
                OpcodeEntry::new(
                    0x0e,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "rotl".to_string(),
                OpcodeEntry::new(
                    0x0f,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "rotr".to_string(),
                OpcodeEntry::new(
                    0x10,
                    3,
                    vec![InterType::Reg, InterType::Reg, InterType::Reg],
                )?,
            ),
            (
                "neg".to_string(),
                OpcodeEntry::new(0x11, 2, vec![InterType::Reg, InterType::Reg])?,
            ),
            (
                "jmp".to_string(),
                OpcodeEntry::new(0x12, 1, vec![InterType::Addr])?,
            ),
            (
                "jifz".to_string(),
                OpcodeEntry::new(0x13, 2, vec![InterType::Reg, InterType::Addr])?,
            ),
            (
                "jifnz".to_string(),
                OpcodeEntry::new(0x14, 2, vec![InterType::Reg, InterType::Addr])?,
            ),
            (
                "pr".to_string(),
                OpcodeEntry::new(0x15, 1, vec![InterType::Reg])?,
            ),
            (
                "inc".to_string(),
                OpcodeEntry::new(0x16, 1, vec![InterType::Reg])?,
            ),
            (
                "dec".to_string(),
                OpcodeEntry::new(0x17, 1, vec![InterType::Reg])?,
            ),
            (
                "push".to_string(),
                OpcodeEntry::new(0x18, 1, vec![InterType::Reg])?,
            ),
            (
                "pop".to_string(),
                OpcodeEntry::new(0x19, 1, vec![InterType::Reg])?,
            ),
            (
                "call".to_string(),
                OpcodeEntry::new(0x1a, 1, vec![InterType::Addr])?,
            ),
            ("ret".to_string(), OpcodeEntry::new(0x1b, 0, vec![])?),
        ]);
        Ok(OpcodeTable { table })
    }

    pub fn fetch_entry(&self, key: &str) -> Result<&OpcodeEntry, AssemblyError> {
        let entry = match self.table.get(key).ok_or("") {
            Ok(entry) => entry,
            Err(_) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UnrecognizedOpcode,
                    reason: Some(format!("[ {key} ] is not a valid operation")),
                    severity: Severity::Fatal,
                })
            }
        };
        Ok(entry)
    }
}
