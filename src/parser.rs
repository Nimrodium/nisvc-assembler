use std::{collections::HashMap, fmt};

use crate::{
    constant::{self, ABSOLUTE, LABEL, MMIO_ADDRESS_SPACE, OPCODE_BYTES, RELATIVE},
    data::{
        get_smallest_byte_size, AssemblyError, AssemblyErrorCode, InterType, Label, Labels,
        OpcodeTable, RegisterTable,
    },
    verbose_println, very_verbose_println, very_very_verbose_println,
};
pub const HEX: char = 'x';
pub const BINARY: char = 'b';
pub const DEC: char = 'd';

fn parse_value(
    immediate_str: &str,
) -> Result<(Option<usize>, Option<String>, bool), AssemblyError> {
    let is_relative = match immediate_str {
        s if s.starts_with(ABSOLUTE) => false,
        s if s.starts_with(RELATIVE) => true,
        _ => {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidImmediate,
                reason: format!("invalid immediate [ {immediate_str} ]"),
            })
        }
    };

    let prefix = match immediate_str.chars().nth(1) {
        Some(p) => p,
        None => {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidImmediate,
                reason: format!("literal [ {immediate_str} ] is empty."),
            })
        }
    };
    let raw = &immediate_str[2..];
    let parsed_value = match prefix {
        BINARY => match usize::from_str_radix(raw, 2) {
            Ok(r) => (Some(r), None),
            Err(err) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidImmediate,
                    reason: format!(
                        "[ {immediate_str} ] is an invalid binary literal :: [ {err} ]"
                    ),
                })
            }
        },
        HEX => match usize::from_str_radix(raw, 16) {
            Ok(r) => (Some(r), None),
            Err(err) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidImmediate,
                    reason: format!("[ {immediate_str} ] is an invalid hex literal :: [ {err} ]"),
                })
            }
        },
        DEC => match usize::from_str_radix(raw, 10) {
            Ok(r) => (Some(r), None),
            Err(err) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidImmediate,
                    reason: format!(
                        "[ {immediate_str} ] is an invalid decimal literal :: [ {err} ]"
                    ),
                })
            }
        },
        LABEL => (None, Some(raw.to_string())),

        p if p.is_digit(10) => match usize::from_str_radix(&immediate_str[1..], 10) {
            Ok(r) => (Some(r), None),
            Err(err) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidImmediate,
                    reason: format!(
                        "[ {immediate_str} ] is an invalid decimal literal :: [ {err} ]"
                    ),
                })
            }
        },
        _ => {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidImmediate,
                reason: format!(
                    " [ {prefix} ] in [ {immediate_str} ] is not a recognized symbol or digit"
                ),
            })
        }
    };
    // catch invalid states
    if parsed_value.0.is_some() {
        if !parsed_value.1.is_none() {
            return Err(AssemblyError{code  : AssemblyErrorCode::UnexpectedError,reason:format!("assembler caught invalid return state from parse_value function [ {parsed_value:?} ] ")});
        }
    }
    if parsed_value.1.is_some() {
        if !parsed_value.0.is_none() {
            return Err(AssemblyError{code  : AssemblyErrorCode::UnexpectedError,reason:format!("assembler caught invalid return state from parse_value function [ {parsed_value:?} ] ")});
        }
    }
    Ok((parsed_value.0, parsed_value.1, is_relative))
}
#[derive(Debug)]
struct IntermediateObject {
    intertype: InterType,
    object: Option<usize>,
    string: Option<String>,
    size: Option<usize>,
    is_resolved: bool,
    // fix_to_rambase: bool,
    is_relative: bool,
}

impl IntermediateObject {
    fn from_mnemonic(
        s: &str,
        expected_type: &InterType,
        opcodes: &OpcodeTable,
        registers: &RegisterTable,
    ) -> Result<Self, AssemblyError> {
        match expected_type {
            InterType::Reg => {
                let object = registers.get_reg(s)?;
                Ok(Self {
                    intertype: InterType::Reg,
                    object: Some(object),
                    string: None,
                    size: Some(constant::REGISTER_BYTES),
                    is_resolved: true,
                    is_relative: false,
                })
            }
            InterType::Addr => {
                let (object, string, is_relative) = parse_value(s)?;
                let is_resolved = if string.is_some() { false } else { true };
                Ok(Self {
                    intertype: InterType::Addr,
                    size: Some(constant::ADDRESS_BYTES),
                    object,
                    string,
                    is_resolved,
                    is_relative,
                })
            }
            InterType::Imm => {
                let (object, string, is_relative) = parse_value(s)?;

                let (is_resolved, size) = if string.is_some() {
                    (false, None)
                } else {
                    (true, Some(get_smallest_byte_size(object.unwrap())?))
                };
                Ok(Self {
                    intertype: InterType::Imm,
                    object,
                    string,
                    is_resolved,
                    is_relative,
                    size,
                })
            }
            InterType::Op => {
                let object = opcodes.get_opcode(s)?;
                Ok(Self {
                    intertype: InterType::Op,
                    object: Some(object.code),
                    string: None,
                    is_relative: false,
                    is_resolved: true,
                    size: Some(OPCODE_BYTES),
                })
            }
        }
    }

    fn resolve(&mut self, labels: &Labels) -> Result<(), AssemblyError> {
        let label = if let Some(s) = &self.string {
            s
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::ObjectAlreadyResolved,
                reason: format!("object {} already defined", self.object.unwrap()),
            });
        };
        self.object = Some(labels.get_label(label)?.dereference()?);
        Ok(())
    }
    fn get_unresolved(&self) -> Result<String, AssemblyError> {
        if let Some(str) = self.string.clone() {
            Ok(str)
        } else {
            let resolved = if let Some(obj) = self.object {
                obj
            } else {
                return Err(AssemblyError{code:AssemblyErrorCode::UnexpectedError,reason:format!("intermediate object of type {:?} is in an invalid trinary state. object and string are both None.",self.intertype)});
            };
            return Err(AssemblyError {
                code: AssemblyErrorCode::ObjectAlreadyResolved,

                reason: format!("{} already resolved", resolved),
            });
        }
    }

    fn to_bytes(&self) -> Result<Vec<u8>, AssemblyError> {
        let object = if let Some(obj) = self.object {
            obj
        } else {
            let unresolved = if let Some(s) = self.string.clone() {
                s
            } else {
                return Err(AssemblyError{code:AssemblyErrorCode::UnexpectedError,reason:format!("intermediate object of type {:?} is in an invalid trinary state. object and string are both None.",self.intertype)});
            };
            return Err(AssemblyError {
                code: AssemblyErrorCode::ObjectNotResolved,
                reason: format!("object [ {unresolved} ] was never resolved"),
            });
        };
        let bytes = object.to_le_bytes();
        Ok(bytes[0..self.size.unwrap()].to_vec())
    }
}
impl fmt::Display for IntermediateObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let is_relative = match self.is_relative {
            true => "Relative".to_string(),
            false => "Absolute".to_string(),
        };
        let (obj_rep, size, is_resolved) = match self.is_resolved {
            true => match self.object {
                Some(o) => (
                    o.to_string(),
                    {
                        if let Some(size) = self.size {
                            format!("{size} bytes")
                        } else {
                            unreachable!("resolved objects should always have a known size")
                        }
                    },
                    "Resolved".to_string(),
                ),
                None => unreachable!("if resolved object should always be some"),
            },
            false => match &self.string {
                Some(s) => (
                    s.clone(),
                    "size unknown".to_string(),
                    "Unresolved".to_string(),
                ),
                None => unreachable!("if not resolved string should always be some"),
            },
        };
        let signature = match self.intertype {
            InterType::Reg => format!("Register : [ {obj_rep} ] {size}"),
            InterType::Addr => {
                format!("{is_relative} {is_resolved} Address : [ {obj_rep} ] {size}")
            }
            InterType::Imm => {
                format!("{is_relative} {is_resolved} Immediate : [ {obj_rep} ] {size}")
            }
            InterType::Op => format!("Opcode : [ {obj_rep} ] {size}"),
        };
        write!(f, "[{signature}]")
    }
}

pub struct IntermediateInstruction {
    objects: Vec<IntermediateObject>,
}
impl fmt::Display for IntermediateInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut obj = String::new();
        for object in &self.objects {
            obj.push_str(&format!("{}:", object.to_string()));
        }
        write!(f, "[IR Object {obj} ]")
    }
}
impl IntermediateInstruction {
    fn parse_string(
        raw: &str,
        opcode_table: &OpcodeTable,
        register_table: &RegisterTable,
    ) -> Result<Self, AssemblyError> {
        let mut objects = vec![];
        let tokenized = tokenize_instruction(raw)?;
        let opcode_str = &tokenized[0];
        let operation = opcode_table.get_opcode(opcode_str)?;
        very_verbose_println!("recognized operation {operation:?}");
        if tokenized.len() - 1 != operation.fields {
            return Err(AssemblyError {
                code: AssemblyErrorCode::IncorrectNumberOfOperands,
                reason: format!(
                    "supplied [ {} ] operands when [ {} ] were expected",
                    tokenized.len() - 1,
                    operation.fields
                ),
            });
        }
        let op_obj = IntermediateObject::from_mnemonic(
            opcode_str,
            &InterType::Op,
            opcode_table,
            register_table,
        )?;
        objects.push(op_obj);
        for (i, raw_token) in tokenized.iter().skip(1).enumerate() {
            let expected_type = match operation.field_types.get(i){
                Some(t) => t,
                None => unreachable!("field count is already verified to match lengh, so element should always exist")
            };
            let object = IntermediateObject::from_mnemonic(
                raw_token,
                expected_type,
                opcode_table,
                register_table,
            )?;
            objects.push(object);
        }
        Ok(Self { objects })
    }
    fn to_bytes(&self) -> Result<Vec<u8>, AssemblyError> {
        let mut byte_vector: Vec<u8> = vec![];
        for object in &self.objects {
            let mut bytes = object.to_bytes()?;
            byte_vector.append(&mut bytes);
        }
        Ok(byte_vector)
    }

    fn resolve_imm(&mut self, labels: &Labels) -> Result<(), AssemblyError> {
        for object in &mut self.objects {
            match object.intertype {
                InterType::Imm => object.resolve(labels)?,
                _ => continue,
            }
        }
        Ok(())
    }
    fn resolve_program_addr(&mut self, labels: &Labels) -> Result<(), AssemblyError> {
        todo!()
    }
    fn resolve_relative_addr(&mut self, labels: &Labels) -> Result<(), AssemblyError> {
        todo!()
    }

    fn get_size(&self) -> Result<usize, AssemblyError> {
        let mut size = 0;
        for object in &self.objects {
            size += if let Some(i) = object.size {
                i
            } else {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::ObjectNotResolved,
                    reason: format!("cannot calculate size of unresolved object : {object}"),
                });
            }
        }
        Ok(size)
    }
}

enum TokenizerState {
    InString,
    None,
}
const STR: char = '"';
const ESCAPE: char = '\\';
const SPACE: char = ' ';
const COMMA: char = ',';
fn tokenize_instruction(s: &str) -> Result<Vec<String>, AssemblyError> {
    let mut buf: Vec<String> = vec![];

    // just gonna implement a manual split so i can preserve strings
    let mut token_buf: String = String::new();
    let mut state = TokenizerState::None;
    let mut escape = false;
    for char_token in s.chars() {
        if escape {
            // process escape stuff
            // there might be a better way to do this
            match char_token {
                ESCAPE => token_buf.push(ESCAPE),
                'n' => token_buf.push('\n'),
                't' => token_buf.push('\t'),
                _ => {
                    return Err(AssemblyError {
                        code: AssemblyErrorCode::SyntaxError,
                        reason: format!("\\{char_token} is not a valid escape sequence"),
                    })
                }
            };
            escape = false
        } else {
            match char_token {
                STR => {
                    state = match state {
                        TokenizerState::InString => TokenizerState::None,
                        TokenizerState::None => TokenizerState::InString,
                    }
                }
                ESCAPE => escape = true,

                _ => match char_token {
                    SPACE | COMMA => match state {
                        TokenizerState::InString => token_buf.push(char_token),
                        TokenizerState::None => {
                            very_verbose_println!("built token {token_buf}");
                            buf.push(token_buf.clone());
                            token_buf.clear();
                        }
                    },
                    _ => token_buf.push(char_token),
                },
            }
        }
    }
    very_verbose_println!("built token {token_buf}");
    buf.push(token_buf.clone());
    token_buf.clear();
    very_verbose_println!("tokenized {buf:?}");
    Ok(buf)
}

pub struct IntermediateProgram {
    instructions: Vec<IntermediateInstruction>,
    size: Option<usize>,
}
impl IntermediateProgram {
    pub fn parse_program(program_raw: &Vec<String>) -> Result<IntermediateProgram, AssemblyError> {
        let opcode_table = OpcodeTable::build_table()?;
        let register_table = RegisterTable::build_table();
        let mut program = vec![];
        for raw_instruction in program_raw {
            if raw_instruction.starts_with(constant::LABEL) {
                continue;
            }

            let instruction = IntermediateInstruction::parse_string(
                raw_instruction,
                &opcode_table,
                &register_table,
            )?;
            verbose_println!("built instruction {instruction}");
            program.push(instruction);
        }
        Ok(Self {
            instructions: program,
            size: None,
        })
    }

    pub fn collect_program_labels(
        &self,
        clean_program_src: &[String],
    ) -> Result<Vec<Label>, AssemblyError> {
        let mut labels: Vec<Label> = vec![];
        let mut program_head = MMIO_ADDRESS_SPACE;
        // let mut line = 0;
        let mut i = 0;
        for line in clean_program_src {
            if line.trim().starts_with(LABEL) {
                let mut label = Label::new(&line.trim()[1..]);
                label.resolve(program_head);
                very_verbose_println!("found program label {label}");
                labels.push(label)
            } else {
                let instruction = match self.instructions.get(i) {
                    Some(instruction) => instruction,
                    None => {
                        return Err(AssemblyError {
                            code: AssemblyErrorCode::UnexpectedError,
                            reason: format!(
                                "attempted to read instruction at index {i} associated with {line} but none was found"
                            ),
                        })
                    }
                };
                i += 1;

                // very_very_verbose_println!("advanced head by {instruction_size}");
                program_head += instruction.get_size()?;
                very_very_verbose_println!(
                    "head::{program_head} advanced past {line} :: {instruction}"
                );
            }
        }
        Ok(labels)
    }
    pub fn resolve_immediates(&mut self, labels: &Labels) -> Result<(), AssemblyError> {
        for instruction in &mut self.instructions {
            instruction.resolve_imm(labels)?;
        }
        Ok(())
    }

    /// called after intermediate resolution
    /// returns size in bytes
    pub fn estimate_program_size(&mut self) -> Result<(), AssemblyError> {
        let mut size = 0;
        for instruction in &self.instructions {
            size += instruction.get_size()?;
        }
        self.size = Some(size);
        Ok(())
    }
    pub fn resolve_program_addresses(&mut self, labels: Labels) -> Result<(), AssemblyError> {
        for instruction in &mut self.instructions {
            instruction.resolve_program_addr(&labels)?;
        }
        Ok(())
    }
    pub fn resolve_ram_addresses(&mut self, labels: Labels) -> Result<(), AssemblyError> {
        for instruction in &mut self.instructions {
            instruction.resolve_relative_addr(&labels)?;
        }
        Ok(())
    }
    pub fn to_bytes(&self) -> Result<Vec<u8>, AssemblyError> {
        let mut byte_vector: Vec<u8> = vec![];
        for object in &self.instructions {
            let mut bytes = object.to_bytes()?;
            byte_vector.append(&mut bytes);
        }
        Ok(byte_vector)
    }
}

pub struct DataParser;
impl DataParser {
    fn parse() {
        todo!()
    }
    // fn
}
