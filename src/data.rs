use crate::{
    constant::{self, Register},
    verbose_println, very_verbose_println, DEBUG_SYMBOLS,
};
use colorize::AnsiColor;
use constant::NAME;
use std::{collections::HashMap, fmt};

#[derive(Clone)]
pub struct MetaData {
    pub text: String,
    pub file: String,
    pub line: usize,
}

impl fmt::Display for MetaData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let position = format!("at {} line {}:", self.file, self.line).yellow();
        write!(f, "{position}\n\t >> {}", self.text.clone().red())
    }
}
// at src/louis.nisvc_asm line 14:
//  >> add r1,r2;

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
    InvalidRegisterName,
    InvalidImmediate,
    CLIArgParseError,
    SourceFileInitializationError,
    SyntaxError,
    UnresolvedLabel,
    InvalidEntryPoint,
    OutputWriteError,
}

pub struct AssemblyError {
    pub code: AssemblyErrorCode,
    pub reason: String,
    pub metadata: Option<MetaData>,
    // pub severity: Severity,
}
impl AssemblyError {
    pub fn append_metadata(mut self, metadata: &MetaData) -> Self {
        self.metadata = Some(metadata.clone());
        self
    }
}
impl fmt::Display for AssemblyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = format!(
            "{NAME}: {} {} :: {}",
            "error:".red(),
            format!("{:?}", self.code).yellow(),
            self.reason
        );
        let metadata = if let Some(md) = self.metadata.clone() {
            md.to_string()
        } else {
            String::new()
        };
        write!(f, "{string}\n{metadata}")
    }
}

pub fn get_smallest_byte_size(integer: usize) -> Result<usize, AssemblyError> {
    if integer > Register::MAX as usize {
        return Err(AssemblyError {
            code: AssemblyErrorCode::ObjectTooLarge,

            reason: format!(
                "integer too large to fit within target registers. {integer} larger than {}",
                Register::MAX
            ),
            metadata: None,
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
#[derive(PartialEq, Debug, Clone)]
pub enum InterType {
    Reg,
    Addr,
    Imm,
    Op,
}
#[derive(Clone)]
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
    // pub fn extend_from_self(&mut self, labels: &Labels) {
    //     // self.table.extend(labels.table)
    //     self.table
    //         .extend(labels.table.iter().map(|(k, v)| (k.clone(), v.clone())));
    // }
    // pub fn extend_from_labels_slice(&mut self, slice: &[Label]) {
    //     for label in slice {
    //         self.insert_label(label);
    //     }
    // }
    pub fn get_label(&self, label: &str) -> Result<&Label, AssemblyError> {
        match self.table.get(label) {
            Some(lbl) => Ok(lbl),
            None => Err(AssemblyError {
                code: AssemblyErrorCode::UndefinedLabel,
                reason: format!("label [ {label} ] was not found in the label table"),
                metadata: None,
            }),
        }
    }
    pub fn get_mut_label(&mut self, label: &str) -> Result<&mut Label, AssemblyError> {
        match self.table.get_mut(label) {
            Some(lbl) => Ok(lbl),
            None => Err(AssemblyError {
                code: AssemblyErrorCode::UndefinedLabel,
                reason: format!("label [ {label} ] was not found in the label table"),
                metadata: None,
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
    pub label_location: LabelLocation, // data_type: InterType,
    // pub offset_multiplier: usize,
    pub is_final: bool,
}
impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let resolved = match self.resolved {
            Some(address) => format!("resolved [ {} ]", address.to_string()),
            None => "unresolved".to_string(),
        };
        let status = if self.is_final {
            format!("[ final ]")
        } else {
            // format!("[ offset multiplier {} ]", self.offset_multiplier)
            "".to_string()
        };
        write!(
            f,
            "[ {} ]::[ {} ]::[ {:?} ]::{status}",
            self.name, resolved, self.label_location
        )
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
        // let offset_multiplier = if is_relative { 1 } else { 0 };
        Self {
            name: name.to_string(),
            resolved: None,
            // is_relative_to_ram_base: false,
            label_location: location,
            // offset_multiplier,
            is_final: false,
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
                metadata: None,
            }),
        }
    }
    // pub fn shift(&mut self, offset: usize) -> Result<(), AssemblyError> {
    //     if self.is_relative {
    //         if let Some(deref_addr) = self.resolved {
    //             let relative = deref_addr + offset;
    //             self.resolve(relative);
    //             // verbose_println!("shifted {} to {:?} {relative}", self.name, self.resolved);
    //         } else {
    //             return Err(AssemblyError {
    //                 code: AssemblyErrorCode::UnresolvedLabel,
    //                 reason: format!("attempted to shift unresolved label [ {} ]  ", self.name),
    //                 metadata: None,
    //             });
    //         };
    //     }
    //     Ok(())
    // }
    //

    pub fn apply_offset(&mut self, base: usize) -> Result<(), AssemblyError> {
        let current_value = if let Some(v) = self.resolved {
            v
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::UnresolvedLabel,
                reason: format!(
                    "attempted to apply relative offset to label [ {} ] which is not resolved",
                    self.name
                ),
                metadata: None,
            });
        };
        let new_value = current_value + (base + 1); // off by one?
        verbose_println!(
            "applied offset to {} {current_value} -> {new_value}",
            self.name
        );
        self.resolved = Some(new_value);
        Ok(())
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
                metadata:None
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
            (
                "cache".to_string(),
                OpcodeEntry::new(0x1c, 1, vec![InterType::Imm])?,
            ),
            ("restore".to_string(), OpcodeEntry::new(0x1d, 0, vec![])?),
            // SPECIAL

            // set breakpoint
            ("breakpoint".to_string(), OpcodeEntry::new(0xfe, 0, vec![])?),
            // manually define end_of_exec
            ("haltexe".to_string(), OpcodeEntry::new(0xff, 0, vec![])?),
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
                    metadata: None,
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
        // let table: HashMap<String, usize> = HashMap::from([
        //     ("r1".to_string(), 1),
        //     ("r2".to_string(), 2),
        //     ("r3".to_string(), 3),
        //     ("r4".to_string(), 4),
        //     ("r5".to_string(), 5),
        //     ("r6".to_string(), 6),
        //     ("r7".to_string(), 7),
        //     ("r8".to_string(), 8),
        //     ("r9".to_string(), 9),
        //     ("r10".to_string(), 10),
        //     ("r11".to_string(), 11),
        //     ("r12".to_string(), 12),
        //     ("r13".to_string(), 13),
        //     ("r14".to_string(), 14),
        //     ("r15".to_string(), 15),
        //     ("r16".to_string(), 16),
        //     ("r17".to_string(), 17),
        //     ("r18".to_string(), 18),
        //     ("r19".to_string(), 19),
        //     ("r20".to_string(), 20),
        //     ("pc".to_string(), 21),
        //     ("sp".to_string(), 22),
        //     ("rsp".to_string(), 23),
        // ]);
        const GENERAL_REGISTER_COUNT: usize = 20;
        const REGISTER_PREFIX: char = 'r';

        let mut table: HashMap<String, usize> = HashMap::new();
        for n in 0..=GENERAL_REGISTER_COUNT {
            table.insert(REGISTER_PREFIX.to_string() + n.to_string().as_str(), n);
        }

        table.insert("pc".to_string(), GENERAL_REGISTER_COUNT + 1);
        table.insert("sp".to_string(), GENERAL_REGISTER_COUNT + 2);
        table.insert("rsp".to_string(), GENERAL_REGISTER_COUNT + 3);
        table.insert("null".to_string(), GENERAL_REGISTER_COUNT + 4);
        Self { table }
    }
    pub fn get_reg(&self, reg_str: &str) -> Result<usize, AssemblyError> {
        let reg_id = match self.table.get(reg_str) {
            Some(id) => id,
            None => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidRegisterName,
                    reason: format!("[ {reg_str} ] is not a valid register"),
                    metadata: None,
                })
            }
        };
        Ok(*reg_id)
    }
}

pub struct DebugPartition {
    map: HashMap<usize, MetaData>,
}
// parsed by the program loader
impl DebugPartition {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    pub fn insert(&mut self, program_counter: usize, metadata: &MetaData) {
        self.map.insert(program_counter, metadata.clone());
    }

    // byte format
    // 8 bytes -> how many bytes is debug partition
    // 8 bytes -> how many source files
    // for source file
    //  8 bytes -> how many bytes is this source file table
    //  key value pairs (8 bytes program counter : (2 bytes length of string, string))
    /// generate data partition image
    pub fn to_bytes(&self) -> Vec<u8> {
        let include_debug = unsafe { DEBUG_SYMBOLS };
        if include_debug {
            // seperate files
            let mut files: HashMap<String, Vec<MetaDataDebugEntry>> = HashMap::new();
            for (program_counter, metadata) in &self.map {
                let entry = MetaDataDebugEntry::new(metadata, *program_counter);
                let file = files.entry(metadata.file.clone()).or_insert_with(Vec::new);
                // file.insert(*program_counter, value);
                file.push(entry);
            }
            let file_count = files.len();
            let mut files_image: Vec<u8> = vec![];
            for (file_name, contents) in files {
                let mut file_image: Vec<u8> = vec![];
                for entry in contents {
                    file_image.extend_from_slice(&entry.to_bytes());
                }
                let file_length_bytes =
                    &file_image.len().to_le_bytes()[0..DEBUG_PARTITION_FILE_LENGTH_SIZE];
                let file_name_bytes = file_name.as_bytes();
                files_image.extend_from_slice(file_length_bytes);
                files_image.extend_from_slice(
                    &file_name_bytes.len().to_le_bytes()[0..DEBUG_PARTITION_TEXT_LENGTH_SIZE],
                );
                files_image.extend_from_slice(file_name_bytes);
                files_image.extend_from_slice(&file_image);
            }

            // let debug_partition_size = DEBUG_PARTITION_FILE_LENGTH_SIZE + files_image.len();
            let mut image: Vec<u8> = vec![];
            // let debug_partition_size_bytes =
            //     &debug_partition_size.to_le_bytes()[0..DEBUG_PARTITION_LENGTH_SIZE];
            // verbose_println!("debug partition size {debug_partition_size_bytes:?}");
            // image.extend_from_slice(&debug_partition_size_bytes);
            image.extend_from_slice(&file_count.to_le_bytes()[0..DEBUG_PARTITION_FILE_LENGTH_SIZE]);
            image.extend_from_slice(&files_image);
            image
        } else {
            let image: Vec<u8> = vec![];
            image
        }
    }
}

struct MetaDataDebugEntry {
    text: String,
    line: usize,
    location: usize,
}
impl MetaDataDebugEntry {
    fn new(metadata: &MetaData, location: usize) -> Self {
        Self {
            text: metadata.text.clone(),
            line: metadata.line,
            location,
        }
    }
    fn to_bytes(&self) -> Vec<u8> {
        let mut buffer: Vec<u8> = vec![];
        let location_bytes = &self.location.to_le_bytes()[0..DEBUG_PARTITION_PC_KEY_SIZE];
        buffer.extend_from_slice(location_bytes);
        let line_bytes = &(self.line as u64).to_le_bytes();
        buffer.extend_from_slice(line_bytes);
        let text_length_bytes = &self.text.len().to_le_bytes()[0..DEBUG_PARTITION_TEXT_LENGTH_SIZE];
        buffer.extend_from_slice(text_length_bytes);
        if !self.text.is_ascii() {
            very_verbose_println!("warning: non ascii character detected in source")
        }
        buffer.extend_from_slice(&self.text.as_bytes());
        buffer
    }
}
const DEBUG_PARTITION_PC_KEY_SIZE: usize = 2; //
const DEBUG_PARTITION_TEXT_LENGTH_SIZE: usize = 2;
const DEBUG_PARTITION_FILE_LENGTH_SIZE: usize = 8;
// const DEBUG_PARTITION_LENGTH_SIZE: usize = 8;
