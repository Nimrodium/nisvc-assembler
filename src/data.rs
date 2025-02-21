use std::collections::HashMap;

use crate::constant::{self, Register};

pub fn get_smallest_byte_size(integer: usize) -> Result<usize, AssemblyError> {
    if integer > Register::MAX as usize {
        return Err(AssemblyError {
            code: AssemblyErrorCode::ObjectTooLarge,
            severity: Severity::Fatal,
            reason: Some(format!(
                "integer too large to fit within target registers. {integer} larger than {}",
                Register::MAX
            )),
        });
    };
    if integer > u64::MAX as usize {
        return Ok(16);
    } else if integer > u32::MAX as usize {
        return Ok(8);
    } else if integer > u16::MAX as usize {
        return Ok(4);
    } else if integer > u8::MAX as usize {
        return Ok(2);
    } else {
        return Ok(1);
    }
}
pub enum InterType {
    Reg,
    Addr,
    Imm,
    Op,
}
pub struct Labels {
    table: HashMap<String, Label>,
    head: usize,
}
impl Labels {
    pub fn insert_label(&mut self, label: Label) {
        self.table.insert(label.name.clone(), label);
    }
    pub fn resolve_immediate_labels(&mut self) {
        for label in &mut self.table {
            self.head += label.1.resolve(self.head);
        }
    }
    pub fn resolve_program_labels(&mut self) {}
    pub fn resolve_relative_labels(&mut self) {}
}
pub struct Label {
    name: String,
    resolved: Option<usize>,
    data_type: InterType,
    size: usize,
}
impl Label {
    pub fn new_addr(name: &str, data_type: InterType) -> Result<Self, AssemblyError> {
        let size = match data_type {
            InterType::Reg => constant::REGISTER_BYTES,
            InterType::Addr => constant::ADDRESS_BYTES,
            InterType::Imm => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::LiteralDefinedAsAddress,
                    reason: Some(format!(
                        "attempted to build immediate label [ {name} ] as address"
                    )),
                    severity: Severity::Fatal,
                })
            }
            InterType::Op => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UnexpectedError,
                    reason: Some(format!("attempted to create label with opcode data type")),
                    severity: Severity::Fatal,
                })
            }
        };

        Ok(Self {
            name: name.to_string(),
            data_type,
            resolved: None,
            size,
        })
    }
    pub fn new_imm(name: &str, literal: constant::Register) -> Result<Self, AssemblyError> {
        let size = get_smallest_byte_size(literal as usize)?;
        Ok(Self {
            name: name.to_string(),
            resolved: Some(literal as usize),
            size,
            data_type: InterType::Imm,
        })
    }
    /// returns bytes to move head
    pub fn resolve(&mut self, memory_head: usize) -> usize {
        self.resolved = Some(memory_head);
        self.size
    }
}

pub enum Severity {
    Info,
    Warning,
    Fatal,
}

pub enum AssemblyErrorCode {
    UnexpectedError,
    InvalidOpcodeDefinition,
    UnrecognizedOpcode,
    IncorrectNumberOfOperands,
    ObjectAlreadyResolved,
    UndefinedLabel,
    ObjectTooLarge,
    LiteralDefinedAsAddress,
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
