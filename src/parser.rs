use meval;
use std::fmt;

use crate::{
    constant::{
        self, ABSOLUTE, ASSEMBLY_PTR, BINARY, COMMA, DATA_MARKER, DEC, DUP_SEPERATOR, ESCAPE, HEX,
        LABEL, MAGIC_RESERVED_MEM, MMIO_ADDRESS_SPACE, OPCODE_BYTES, RELATIVE, SEPERATOR, SPACE,
        STR,
    },
    data::{
        get_smallest_byte_size, AssemblyError, AssemblyErrorCode, InterType, Label, LabelLocation,
        Labels, OpcodeTable, RegisterTable,
    },
    verbose_println, very_verbose_println, very_very_verbose_println,
};
/// returns (resolved,label,is_relative)
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
                InterType::Imm => {
                    if !object.is_resolved {
                        object.resolve(labels)?
                    }
                }
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
                let mut label = Label::new(&line.trim()[1..], LabelLocation::Ram);
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
    pub fn estimate_program_size(&mut self) -> Result<usize, AssemblyError> {
        let mut size = 0;
        for instruction in &self.instructions {
            size += instruction.get_size()?;
        }
        self.size = Some(size);
        Ok(size)
    }
    pub fn resolve_program_addresses(&mut self, labels: Labels) -> Result<(), AssemblyError> {
        for instruction in &mut self.instructions {
            instruction.resolve_program_addr(&labels)?;
        }
        Ok(())
    }
    pub fn resolve_ram_addresses(&mut self, labels: &Labels) -> Result<(), AssemblyError> {
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

pub struct Data {
    pub data_image: Vec<u8>,
    pub labels: Labels,
    // pub ram_base_address: usize,
    // length: usize,
}

enum Unit {
    B8,
    B16,
    B32,
    B64,
}
impl Unit {
    fn from_size_str(bit_count: &str) -> Result<Self, AssemblyError> {
        match bit_count {
            "8" => Ok(Self::B8),
            "16" => Ok(Self::B16),
            "32" => Ok(Self::B32),
            "64" => Ok(Self::B64),
            _ => Err(AssemblyError {
                code: AssemblyErrorCode::SyntaxError,
                reason: format!("{bit_count} is not a valid word size"),
            }),
        }
    }
    fn bits(&self) -> usize {
        match self {
            Unit::B8 => 8,
            Unit::B16 => 16,
            Unit::B32 => 32,
            Unit::B64 => 64,
        }
    }
    fn bytes(&self) -> usize {
        self.bits() / 8
    }
}

impl Data {
    /// parse data structure.
    pub fn new() -> Self {
        Self {
            data_image: vec![],
            labels: Labels::new(),
            // length: 0,
        }
    }
    pub fn shift_addresses(&mut self, ram_base_address: usize) -> Result<(), AssemblyError> {
        for label in &mut self.labels.table {
            label.1.shift(ram_base_address)?;
        }
        Ok(())
    }
    pub fn parse(&mut self, clean_src: &[String]) -> Result<(), AssemblyError> {
        verbose_println!("--- PARSE DATA START --");
        for line in clean_src {
            self.parse_line(line)?;
        }
        verbose_println!("data image : {:?}", self.data_image);
        verbose_println!("--- PARSE DATA END --");
        Ok(())
    }
    fn parse_line(&mut self, line: &str) -> Result<(), AssemblyError> {
        let (label_name, keyword_and_instruction) = if let Some(s) = line.split_once(SPACE) {
            s
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::SyntaxError,
                reason: format!("could not find label in {line}"),
            });
        };
        if label_name != "_" {
            let mut label = Label::new(label_name, LabelLocation::Ram);
            label.resolve(self.data_image.len());
            // label.is_relative_to_ram_base = true;
            self.labels.insert_label(&label);
        }
        let (keyword, instruction) = if let Some(s) = keyword_and_instruction.split_once(SPACE) {
            s
        } else {
            return Err(AssemblyError {
                code: AssemblyErrorCode::SyntaxError,
                reason: format!("could not find keyword in {keyword_and_instruction}"),
            });
        };

        match keyword {
            "def" => {
                let (unit, sequence) = Data::_get_unit_and_rest(instruction)?;
                self.define_sequence(unit, sequence)?
            }
            "res" => {
                let (unit, rest) = Data::_get_unit_and_rest(instruction)?;
                self.reserve_bytes(unit, rest)?
            }
            "equ" => {
                let immediate = self.equate(instruction)?;
                let label = self.labels.get_mut_label(label_name)?;
                label.resolve(immediate);
                very_verbose_println!("defined alias [ {label} ] from {instruction}")
            }
            "str" => self.define_string(instruction)?,
            "dup" => {
                let (unit, rest) = Data::_get_unit_and_rest(instruction)?;
                let (count_expr, value_expr) = match rest.split_once(DUP_SEPERATOR) {
                    Some(result) => result,
                    None => {
                        return Err(AssemblyError {
                            code: AssemblyErrorCode::SyntaxError,
                            reason: format!("syntax error in {rest} :: failed to split"),
                        })
                    }
                };
                self.duplicate(unit, count_expr, value_expr)?
            }
            _ => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::UnrecognizedDataKeyword,
                    reason: format!("{keyword} is not a valid keyword"),
                })
            }
        };
        Ok(())
    }
    fn duplicate(
        &mut self,
        unit: Unit,
        count_expr: &str,
        value_expr: &str,
    ) -> Result<(), AssemblyError> {
        let value = &self.equate(value_expr)?;
        let bytes_to_duplicate = &value.to_le_bytes()[0..unit.bytes()];
        let count = self.equate(count_expr)?;
        for _ in 0..=count {
            self.data_image.extend_from_slice(bytes_to_duplicate);
        }
        verbose_println!("duplicated {value} of unit {} {count} times ", unit.bits());
        Ok(())
    }
    fn define_string(&mut self, s: &str) -> Result<(), AssemblyError> {
        let mut byte_buf: Vec<u8> = Vec::new();

        let mut in_string = false;
        let mut str_buf = String::new();
        let mut byte_raw_buf = String::new();

        for c in s.to_string().chars() {
            match c {
                STR => {
                    if in_string {
                        in_string = false;
                        byte_buf.extend_from_slice(str_buf.as_bytes());
                        str_buf.clear();
                    } else {
                        in_string = true
                    }
                }
                SPACE | COMMA => {
                    if in_string {
                        // byte_buf.extend_from_slice(str_buf.as_bytes());
                        str_buf.push(c);
                    } else {
                        let trimmed_byte_raw_buf = byte_raw_buf.trim();
                        if !trimmed_byte_raw_buf.is_empty() {
                            let literal_byte =
                                self._define_string_resolve_byte(&trimmed_byte_raw_buf)?;
                            byte_buf.push(literal_byte);
                            byte_raw_buf.clear();
                        }
                    }
                }
                _ => {
                    if in_string {
                        str_buf.push(c);
                    } else {
                        byte_raw_buf.push(c);
                    }
                }
            }
        }
        // clean up
        if in_string {
            return Err(AssemblyError {
                code: AssemblyErrorCode::SyntaxError,
                reason: format!("string never closed in {s}"),
            });
        }
        if !byte_raw_buf.is_empty() {
            let literal_byte = self._define_string_resolve_byte(&byte_raw_buf)?;
            byte_buf.push(literal_byte)
        }
        verbose_println!(
            "defined string {s} -> {byte_buf:?} length {}",
            byte_buf.len()
        );
        self.data_image.extend_from_slice(&byte_buf);
        Ok(())
    }
    /// reserve bytes
    fn reserve_bytes(&mut self, unit: Unit, bytes_str: &str) -> Result<(), AssemblyError> {
        let bytes = match parse_value(bytes_str.trim())? {
            (Some(literal), None, false) => literal,
            _ => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: format!("unsupported value in data"),
                })
            }
        };
        let byte_sequence = vec![MAGIC_RESERVED_MEM; bytes * (unit.bytes())];
        self.data_image.extend_from_slice(&byte_sequence);
        verbose_println!(
            "reserved {bytes} bytes of unit {} :: {byte_sequence:x?}",
            unit.bits(),
        );
        Ok(())
    }
    /// parses and writes to image a sequence string
    fn define_sequence(&mut self, unit: Unit, sequence: &str) -> Result<(), AssemblyError> {
        let mut byte_buf: Vec<u8> = vec![];
        let split_sequence = sequence.split([SPACE, COMMA]);
        for token in split_sequence {
            let resolved_token_literal = match parse_value(token)? {
                (Some(u), None, false) => u,
                (None, Some(label), false) => self.labels.get_label(&label)?.dereference()?,
                _ => unreachable!(),
            };
            let literal_bytes = &resolved_token_literal.to_le_bytes()[0..unit.bytes()];
            byte_buf.extend_from_slice(literal_bytes);
        }
        self.data_image.extend_from_slice(&byte_buf);
        verbose_println!(
            "defined sequence of unit {} [ {sequence} ] ->  [ {byte_buf:?} ]",
            unit.bits()
        );

        Ok(())
    }
    fn _define_string_resolve_byte(&self, byte_raw_buf: &str) -> Result<u8, AssemblyError> {
        let literal_byte = match parse_value(byte_raw_buf)? {
            (Some(literal), None, false) => literal,
            _ => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidImmediate,
                    reason: format!("{byte_raw_buf} is an invalid state of parsed immediate"),
                })
            }
        };
        if literal_byte > u8::MAX as usize {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidImmediate,
                reason: format!(
                    "{literal_byte} is too big for inline byte definition in the string keyword"
                ),
            });
        }
        Ok(literal_byte as u8)
    }

    /// equate a data expression
    /// kinda sucks ass
    fn equate(&self, expr: &str) -> Result<usize, AssemblyError> {
        let mut resolved_expr = String::new();
        let mut token_buf = String::new();
        let operations = ['+', '-', '*', '/', '%', ' ', '(', ')'];
        for char_token in expr.chars() {
            if operations.contains(&char_token) {
                if !token_buf.trim().is_empty() {
                    let resolved_literal = if token_buf.trim() == ASSEMBLY_PTR.to_string().as_str()
                    {
                        self.data_image.len()
                    } else {
                        match parse_value(&token_buf.trim())?{

                            (Some(literal),None,false) => literal,
                            (None,Some(label),false) => self.labels.get_label(&label)?.dereference()?,
                            (_,_,true) => return Err(AssemblyError{code:AssemblyErrorCode::SyntaxError,reason:format!("{expr} contains a relative ({RELATIVE}) literal which is not supported in {DATA_MARKER}")}),
                            _ => return Err(AssemblyError{code:AssemblyErrorCode::SyntaxError,reason:format!("illegal state entered when attempting to parse {expr}")})

                        }
                    };
                    token_buf.clear();
                    resolved_expr.push_str(&resolved_literal.to_string());
                }
                resolved_expr.push(char_token);
            } else {
                token_buf.push(char_token);
            }
        }
        // cleanup
        if !token_buf.is_empty() {
            let resolved_literal = match parse_value(&token_buf)?{
                (Some(literal),None,false) => literal,
                (None,Some(label),false) => self.labels.get_label(&label)?.dereference()?,
                (_,_,true) => return Err(AssemblyError{code:AssemblyErrorCode::SyntaxError,reason:format!("{expr} contains a relative ({RELATIVE}) literal which is not supported in {DATA_MARKER}")}),
                _ => return Err(AssemblyError{code:AssemblyErrorCode::SyntaxError,reason:format!("illegal state entered when attempting to parse {expr}")})
            };
            resolved_expr.push_str(&resolved_literal.to_string());
        }
        let result = match meval::eval_str(&resolved_expr) {
            Ok(r) => r,
            Err(err) => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: format!("failed to parse expression {expr}: {err}"),
                })
            }
        } as usize;
        verbose_println!("resolved {expr} -> {resolved_expr} -> {result} ");
        Ok(result)
    }

    /// internal helper
    fn _equate_resolve_str_buf(&self, string: &mut String) -> Result<usize, AssemblyError> {
        let parsed_value = parse_value(&string)?;
        if parsed_value.2 {
            return Err(AssemblyError {
                code: AssemblyErrorCode::InvalidImmediate,
                reason: format!("{string} is a relative immediate, which is not supported in data"),
            });
        }
        let literal = match parsed_value {
            (None, Some(label), false) => self.labels.get_label(&label)?.dereference()?,
            (Some(literal), None, false) => literal,
            _ => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::InvalidImmediate,
                    reason: format!("{parsed_value:?} is an invalid state of parsed immediate"),
                })
            }
        };
        Ok(literal)
    }
    fn _get_unit_and_rest(instruction: &str) -> Result<(Unit, &str), AssemblyError> {
        let unit_sequence = match instruction.split_once(SPACE) {
            Some((unit, rest)) => (Unit::from_size_str(unit)?, rest),
            None => {
                return Err(AssemblyError {
                    code: AssemblyErrorCode::SyntaxError,
                    reason: format!("missing unit (8/16/32/64)"),
                })
            }
        };
        Ok(unit_sequence)
    }
}
