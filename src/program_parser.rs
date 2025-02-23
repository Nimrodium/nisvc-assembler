use std::collections::HashMap;

use crate::{
    constant::{self, Labels, ABSOLUTE, LABEL, OPCODE_BYTES, RELATIVE},
    data::{
        get_smallest_byte_size, AssemblyError, AssemblyErrorCode, InterType, OpcodeTable,
        RegisterTable,
    },
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

        p if p.is_digit(10) => match usize::from_str_radix(raw, 10) {
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

    fn resolve(
        &mut self,
        labels: &HashMap<String, constant::Register>,
    ) -> Result<(), AssemblyError> {
        let label = if let Some(s) = &self.string {
            s
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::ObjectAlreadyResolved,
                reason: format!("object {} already defined", self.object.unwrap()),
            });
        };
        self.object = match labels.get(label) {
            Some(r) => Some(*r as usize),
            None => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UndefinedLabel,
                    reason: format!("label: [ {label} ] was not defined when needed"),
                })
            }
        };
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

pub struct IntermediateInstruction {
    objects: Vec<IntermediateObject>,
}
impl IntermediateInstruction {
    fn parse_string(
        raw: &str,
        opcode_table: &OpcodeTable,
        register_table: &RegisterTable,
    ) -> Result<Self, AssemblyError> {
        let mut objects = vec![];
        let tokenized = tokenize_instruction(raw);
        let opcode_str = &tokenized[0];
        let operation = opcode_table.get_opcode(opcode_str)?;

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

        for (i, raw_token) in tokenized.iter().enumerate() {
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
    fn resolve_program_addr(&mut self, labels: Labels) -> Result<(), AssemblyError> {
        todo!()
    }
    fn resolve_relative_addr(&mut self, labels: Labels) -> Result<(), AssemblyError> {
        todo!()
    }
}

fn tokenize_instruction(s: &str) -> Vec<String> {
    todo!()
}

pub struct IntermediateProgram {
    instructions: Vec<IntermediateInstruction>,
    size: Option<usize>,
}
impl IntermediateProgram {
    pub fn parse_program(program_raw: Vec<&str>) -> Result<IntermediateProgram, AssemblyError> {
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
            program.push(instruction);
        }
        Ok(Self {
            instructions: program,
            size: None,
        })
    }
    pub fn resolve_immediates(&mut self) {}

    /// called after intermediate resolution
    pub fn estimate_program_size(&mut self) -> Result<(), AssemblyError> {
        todo!()
    }
    pub fn resolve_program_addresses(&mut self) -> Result<(), AssemblyError> {
        todo!()
    }
    pub fn resolve_ram_addresses(&mut self) -> Result<(), AssemblyError> {
        todo!()
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
