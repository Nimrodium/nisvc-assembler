use std::collections::HashMap;

use crate::{
    constant::{self, Labels},
    data::{AssemblyError, AssemblyErrorCode, InterType, OpcodeTable, Severity},
};

struct IntermediateObject {
    intertype: InterType,
    object: usize,
    string: Option<String>,
    size: usize,
    is_resolved: bool,
    fix_to_rambase: bool,
}

impl IntermediateObject {
    fn from_mnemonic(s: &str, expected_type: &InterType) -> Result<Self, AssemblyError> {
        todo!()
    }
    fn resolve(
        &mut self,
        labels: &HashMap<String, constant::Register>,
    ) -> Result<(), AssemblyError> {
        let label = if let Some(str) = &self.string {
            str
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::ObjectAlreadyResolved,
                reason: Some(format!("object {} already defined", self.object)),
                severity: Severity::Warning,
            });
        };
        self.object = match labels.get(label) {
            Some(r) => *r as usize,
            None => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UndefinedLabel,
                    reason: Some(format!("label: [ {label} ] was not defined when needed")),
                    severity: Severity::Fatal,
                })
            }
        };

        Ok(())
    }
    fn get_unresolved(&self) -> Result<String, AssemblyError> {
        if let Some(str) = self.string.clone() {
            Ok(str)
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::ObjectAlreadyResolved,
                severity: Severity::Fatal, // fatal here because it cannot proceed to return the string
                reason: Some(format!("{} already resolved", self.object)),
            });
        }
    }
    fn to_bytes(&self) -> Vec<u8> {
        let bytes = self.object.to_le_bytes();
        bytes[0..self.size].to_vec()
    }
}

pub struct IntermediateInstruction {
    objects: Vec<IntermediateObject>,
}
impl IntermediateInstruction {
    fn parse_string(raw: &str, opcode_table: &OpcodeTable) -> Result<Self, AssemblyError> {
        let mut objects = vec![];
        let tokenized = tokenize_instruction(raw);
        let opcode_str = &tokenized[0];
        let operation = opcode_table.fetch_entry(opcode_str)?;

        if tokenized.len() - 1 != operation.fields {
            return Err(AssemblyError {
                code: AssemblyErrorCode::IncorrectNumberOfOperands,
                reason: Some(format!(
                    "supplied [ {} ] operands when [ {} ] were expected",
                    tokenized.len() - 1,
                    operation.fields
                )),
                severity: Severity::Fatal,
            });
        }
        for (i, operand) in tokenized.iter().enumerate() {
            let expected_type = match operation.field_types.get(i){
                Some(t) => t,
                None => unreachable!("field count is already verified to match lengh, so element should always exist")
            };
            let object = IntermediateObject::from_mnemonic(operand, expected_type)?;
            objects.push(object);
        }
        Ok(Self { objects })
    }
    fn to_bytes(&self) -> Vec<u8> {
        todo!()
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

pub fn parse_program(
    program_raw: Vec<&str>,
) -> Result<Vec<IntermediateInstruction>, AssemblyError> {
    let opcode_table = OpcodeTable::build_table()?;
    let mut program = vec![];
    for raw_instruction in program_raw {
        if raw_instruction.starts_with(constant::LABEL) {
            continue;
        }

        let instruction = IntermediateInstruction::parse_string(raw_instruction, &opcode_table)?;
        program.push(instruction);
    }
    Ok(program)
}
