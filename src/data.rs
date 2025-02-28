use crate::constant::{self, Register};
use colorize::AnsiColor;
use constant::NAME;
use std::{collections::HashMap, fmt};
pub fn get_smallest_byte_size(integer: usize) -> Result<usize, AssemblyError> {
    if integer > Register::MAX as usize {
        return Err(AssemblyError {
            code: AssemblyErrorCode::ObjectTooLarge,

            reason: format!(
                "integer too large to fit within target registers. {integer} larger than {}",
                Register::MAX
            ),
        });
    };
    if integer > u64::MAX as usize {
        Ok(16)
    } else if integer > u32::MAX as usize {
        Ok(8)
    } else if integer > u16::MAX as usize {
        Ok(4)
    } else if integer > u8::MAX as usize {
        Ok(2)
    } else {
        Ok(1)
    }
}
#[derive(PartialEq, Debug)]
pub enum InterType {
    Reg,
    Addr,
    Imm,
    Op,
}
pub struct Labels {
    pub table: HashMap<String, Label>,
}
impl Labels {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }
    pub fn insert_label(&mut self, label: &Label) {
        self.table.insert(label.name.clone(), label.clone());
    }
    pub fn extend_from_labels_slice(&mut self, slice: &[Label]) {
        for label in slice {
            self.insert_label(label);
        }
    }
    pub fn get_label(&self, label: &str) -> Result<&Label, AssemblyError> {
        match self.table.get(label) {
            Some(lbl) => Ok(lbl),
            None => Err(AssemblyError {
                code: AssemblyErrorCode::UndefinedLabel,
                reason: format!("label [ {label} ] was not found in the label table"),
            }),
        }
    }
    pub fn get_mut_label(&mut self, label: &str) -> Result<&mut Label, AssemblyError> {
        match self.table.get_mut(label) {
            Some(lbl) => Ok(lbl),
            None => Err(AssemblyError {
                code: AssemblyErrorCode::UndefinedLabel,
                reason: format!("label [ {label} ] was not found in the label table"),
            }),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum LabelLocation {
    Program,
    Ram,
    Immediate,
}
#[derive(Clone)]
pub struct Label {
    pub name: String,
    resolved: Option<usize>,
    // pub is_relative_to_ram_base: bool,
    pub label_location: LabelLocation, // data_type: InterType,
                                       // size: usize,
}
impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let resolved = match self.resolved {
            Some(address) => format!("resolved [ {} ]", address.to_string()),
            None => "unresolved".to_string(),
        };
        write!(f, "[ {} ]::{}", self.name, resolved)
    }
}
impl fmt::Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = self.to_string();
        write!(f, "({label})")
    }
}
impl Label {
    pub fn is_in(&self, location: LabelLocation) -> bool {
        if location == self.label_location {
            true
        } else {
            false
        }
    }
    pub fn new(name: &str, location: LabelLocation) -> Self {
        Self {
            name: name.to_string(),
            resolved: None,
            // is_relative_to_ram_base: false,
            label_location: location,
        }
    }
    pub fn resolve(&mut self, address: usize) {
        self.resolved = Some(address)
    }

    pub fn dereference(&self) -> Result<usize, AssemblyError> {
        match self.resolved {
            Some(address) => Ok(address),
            None => Err(AssemblyError {
                code: AssemblyErrorCode::UnresolvedLabel,
                reason: format!(
                    "attempted to dereference unresolved label [ {} ]",
                    self.name
                ),
            }),
        }
    }
    pub fn shift(&mut self, offset: usize) -> Result<(), AssemblyError> {
        self.resolved = if let Some(deref_addr) = self.resolved {
            let relative = deref_addr + offset;
            Some(relative)
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::UnresolvedLabel,
                reason: format!("attempted to shift unresolved label [ {} ]  ", self.name),
            });
        };

        Ok(())
    }
}

#[derive(Debug)]
pub enum AssemblyErrorCode {
    UnexpectedError,
    InvalidOpcodeDefinition,
    UnrecognizedOpcode,
    UnrecognizedDataKeyword,
    IncorrectNumberOfOperands,
    ObjectAlreadyResolved,
    ObjectNotResolved,
    UndefinedLabel,
    ObjectTooLarge,
    LiteralDefinedAsAddress,
    InvalidRegisterName,
    EmptyLiteral,
    InvalidImmediate,
    InvalidAddress,
    CLIArgParseError,
    NotImplemented,
    SourceFileInitializationError,
    SyntaxError,
    UnresolvedLabel,
    InvalidEntryPoint,
}

pub struct AssemblyError {
    pub code: AssemblyErrorCode,
    pub reason: String,
    // pub severity: Severity,
}
impl fmt::Display for AssemblyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = format!(
            "{NAME}: {} {} :: {}",
            "error:".red(),
            format!("{:?}", self.code).yellow(),
            self.reason
        );
        write!(f, "{string}")
    }
}
#[derive(Debug)]
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
                reason: format!("opcode field count does not align with defined field type length :: in [ {code:#x} ] [ {fields} ] [ {field_types:?} ]"),
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
                    4,
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

    pub fn get_opcode(&self, key: &str) -> Result<&OpcodeEntry, AssemblyError> {
        let entry = match self.table.get(key).ok_or("") {
            Ok(entry) => entry,
            Err(_) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UnrecognizedOpcode,
                    reason: format!("[ {key} ] is not a valid operation"),
                })
            }
        };
        Ok(entry)
    }
}

pub struct RegisterTable {
    table: HashMap<String, usize>,
}
impl RegisterTable {
    pub fn build_table() -> Self {
        let table: HashMap<String, usize> = HashMap::from([
            ("r1".to_string(), 1),
            ("r2".to_string(), 2),
            ("r3".to_string(), 3),
            ("r4".to_string(), 4),
            ("r5".to_string(), 5),
            ("r6".to_string(), 6),
            ("r7".to_string(), 7),
            ("r8".to_string(), 8),
            ("r9".to_string(), 9),
            ("r10".to_string(), 10),
            ("r11".to_string(), 11),
            ("r12".to_string(), 12),
            ("r13".to_string(), 13),
            ("r14".to_string(), 14),
            ("r15".to_string(), 15),
            ("r16".to_string(), 16),
            ("r17".to_string(), 17),
            ("r18".to_string(), 18),
            ("r19".to_string(), 19),
            ("r20".to_string(), 20),
            ("pc".to_string(), 21),
            ("sp".to_string(), 22),
            ("o1".to_string(), 23),
            ("o2".to_string(), 24),
            ("o3".to_string(), 25),
            ("o4".to_string(), 26),
            ("o5".to_string(), 27),
        ]);
        Self { table }
    }
    pub fn get_reg(&self, reg_str: &str) -> Result<usize, AssemblyError> {
        let reg_id = match self.table.get(reg_str) {
            Some(id) => id,
            None => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidRegisterName,
                    reason: format!("[ {reg_str} ] is not a valid register"),
                })
            }
        };
        Ok(*reg_id)
    }
}
