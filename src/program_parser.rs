use crate::{
    constant,
    data::{AssemblyError, AssemblyErrorCode, InterType, OpcodeTable, Severity},
};

struct IntermediateObject {
    intertype: InterType,
    object: usize,
    string: Option<String>,
    size: usize,
    is_resolved: bool,
}

impl IntermediateObject {
    fn from_mnemonic(s: &str, expected_type: &InterType) -> Result<Self, AssemblyError> {
        todo!()
    }
    fn to_bytes(&self) -> Vec<u8> {
        todo!()
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
