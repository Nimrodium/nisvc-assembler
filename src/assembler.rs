use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
    mem::transmute,
    vec,
};

use crate::{
    instruction::{Instruction, RegHandle},
    tokenizer::{tokenize_file, Lexeme, Source, Token},
    AssembleError,
};

macro_rules! invalid_token_err {
    ($msg:expr, $token:expr) => {{
        let lexeme = $token.get_lexeme();
        let msg = format!($msg, lexeme.s);
        Err(AssembleError::new(msg).attach_lexeme(lexeme))
    }};
}

#[derive(Debug)]
pub enum Label {
    Resolved(u64, bool),
    Unresolved(String, bool, Lexeme),
}
impl Label {
    fn resolve(
        &mut self,
        labels: &HashMap<String, (u64, bool)>,
        program_size: u64,
    ) -> Result<(), AssembleError> {
        let value = match self {
            Label::Resolved(value, offset) => {
                if *offset {
                    *value + program_size
                } else {
                    *value
                }
            }
            Label::Unresolved(label, expected_offset, lexeme) => match labels.get(label) {
                Some((value, offset)) => {
                    if *offset != *expected_offset {
                        return Err(AssembleError::new(format!(
                            "expected '{}' for label `{}`",
                            convert_to_symbolic_offset(*offset),
                            lexeme.s
                        ))
                        .attach_lexeme(lexeme));
                    }
                    if *offset {
                        *value + program_size
                    } else {
                        *value
                    }
                }

                None => {
                    return Err(AssembleError::new(format!("failed to resolve `{label}`"))
                        .attach_lexeme(lexeme))
                }
            },
        };
        *self = Label::Resolved(value, false);
        Ok(())
    }
}

enum ParserSectionState {
    Initial,
    Program,
    Data,
    Entry,
    Include,
}

enum WordSize {
    B1,
    B2,
    B4,
    B8,
    F8,
}
impl WordSize {
    fn from_token(token: Token) -> Result<Self, AssembleError> {
        match token {
            Token::KeyWord(lexeme) => match lexeme.s.to_lowercase().as_str() {
                "b1" => Ok(Self::B1),
                "b2" => Ok(Self::B2),
                "b4" => Ok(Self::B4),
                "b8" => Ok(Self::B8),
                "f8" => Ok(Self::F8),
                _ => {
                    let msg = format!("invalid byte size `{}`", lexeme.s);
                    Err(AssembleError::new(msg).attach_lexeme(&lexeme))
                }?,
            },
            _ => invalid_token_err!("invalid byte size `{}`", token)?,
        }
    }
}
pub struct Assembler {
    labels: HashMap<String, (u64, bool)>,
    program_intermediate: Vec<Instruction>,
    pub data: Vec<u8>,
    program_size: u64,
    pub entry_point: Option<u64>,
    includes: HashSet<String>,
    sources: Source,
}

enum Section {
    Uninit,
    Include,
    Entry,
    Data,
    Program,
    EOF,
}

impl Assembler {
    // pub fn assemble(
    //     tokens: Vec<Token>,
    //     stdlib_path: Option<String>,
    // ) -> Result<(u64, Vec<u8>, Vec<u8>, Vec<u8>), AssembleError> {
    //     let mut assembler = Self::new();
    //     assembler.parse(tokens)?;
    //     assembler.resolve()?;
    //     let program = assembler.emit();
    //     let debug_symbols = assembler.build_debug_symbol_table();
    //     let entry_point = if let Some(ep) = assembler.entry_point {
    //         ep
    //     } else {
    //         return Err(AssembleError::new(format!("no entry point defined")));
    //     };
    //     Ok((entry_point, assembler.data, program, debug_symbols))
    // }
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
            program_intermediate: Vec::new(),
            data: Vec::new(),
            program_size: 0,
            entry_point: None,
            includes: HashSet::new(),
            sources: Source::new(),
        }
    }

    pub fn parse_file(&mut self, file_path: &str) -> Result<(), AssembleError> {
        let (fd, file_content) = self.sources.open_file(file_path)?;
        let tokens = tokenize_file(fd, &file_content)?;
        self.parse(tokens)?;
        Ok(())
    }
    pub fn build_debug_symbol_table(&self) -> Vec<u8> {
        let mut image = Vec::<u8>::new();
        for label in &self.labels {
            image.extend(label.1 .0.to_le_bytes());
            image.extend((label.0.len() as u64).to_le_bytes());
            image.extend(label.0.clone().into_bytes())
        }
        image
    }
    pub fn emit(&self) -> Vec<u8> {
        let mut program_image = Vec::<u8>::new();
        for instruction in &self.program_intermediate {
            program_image.extend(instruction.compile());
        }
        program_image
    }
    pub fn resolve(&mut self) -> Result<(), AssembleError> {
        for instruction in &mut self.program_intermediate {
            match instruction {
                Instruction::Ldi { dest: _, src } => {
                    src.resolve(&self.labels, self.program_size)?
                }
                Instruction::Jmp { addr } => addr.resolve(&self.labels, self.program_size)?,
                Instruction::Jifz { addr, condition: _ } => {
                    addr.resolve(&self.labels, self.program_size)?
                }
                Instruction::Jifnz { addr, condition: _ } => {
                    addr.resolve(&self.labels, self.program_size)?
                }
                Instruction::Call { addr } => addr.resolve(&self.labels, self.program_size)?,
                Instruction::Pushi { immediate } => {
                    immediate.resolve(&self.labels, self.program_size)?
                }
                _ => continue,
            }
        }
        Ok(())
    }
    // fn include(&mut self, std_lib_mod: &str) -> Result<(), AssembleError> {
    //     if !self.includes.contains(std_lib_mod) {

    //         let path = self.get_include_path(std_lib_mod)

    //         self.includes.insert(std_lib_mod.to_string());
    //     }

    //     Ok(())
    // }

    fn get_include_path(&self, std_lib_mod: &str) -> Result<String, AssembleError> {
        let name: Vec<String> = std_lib_mod.split('.').map(|s| s.to_string()).collect();
        match name
            .get(0)
            .ok_or(AssembleError::new(format!("invalid module name")))?
        {
            _ => Err(AssembleError::new(format!(
                "unsupported module root {} in {std_lib_mod}",
                name[0]
            ))),
        }
    }

    /// generates IR from a token stream
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<(), AssembleError> {
        let mut stream = tokens.into_iter().peekable();
        // let mut labels: HashMap<String, (u64, bool)> = HashMap::new();
        // let mut data: Vec<u8> = Vec::new();
        let mut entry_point_label: Option<Label> = None;
        let mut program_intermediate: Option<Vec<Instruction>> = None;
        // let mut program_size = 0;
        // let mut includes: HashSet<String> = HashSet::new();
        loop {
            let token = stream.next().unwrap();
            match token {
                Token::EOF(_) => break,
                Token::INCLUDE(_) => {
                    for token in stream.by_ref() {
                        match token {
                            Token::KeyWord(lexeme) => {
                                // includes.insert(lexeme.s);
                            }
                            _ => break,
                        }
                    }
                }
                Token::PROGRAM(_) => {
                    let (program, program_labels, added_pc) =
                        Self::parse_program(&mut stream, self.program_size)?;
                    for (name, address) in program_labels {
                        self.labels.insert(name, (address, false));
                    }
                    self.program_size += added_pc;
                    program_intermediate = Some(program);
                }
                Token::DATA(_) => {
                    self.parse_data(&mut stream)?;
                }
                Token::ENTRYPOINT(_) => {
                    // let (label, address) = Self::parse_entry(&mut stream)?;
                    let token = stream.next().unwrap();
                    match token {
                        Token::KeyWord(lexeme) => {
                            entry_point_label =
                                Some(Label::Unresolved(lexeme.s.clone(), false, lexeme))
                        }
                        _ => invalid_token_err!("expected label `{}`", token)?,
                    }
                }
                Token::EOS(_) => continue,
                _ => {
                    println!("panic on `{token:?}`");
                    panic!()
                }
            }
        }
        println!("{program_intermediate:?}");

        let program_intermediate = if let Some(p) = program_intermediate {
            p
        } else {
            return Err(AssembleError::new("no program defined".to_string()));
        };

        let entry_point = if let Some(mut e) = entry_point_label {
            e.resolve(&self.labels, self.program_size)?;
            match e {
                Label::Resolved(v, _) => Some(v),
                Label::Unresolved(_, _, _) => unreachable!(),
            }
        } else {
            None
        };

        self.program_intermediate.extend(program_intermediate);
        self.entry_point = entry_point;
        // self.data.extend(data);
        Ok(())
    }

    // fn parse_entry(
    //     stream: &mut Peekable<vec::IntoIter<Token>>,
    // ) -> Result<(String, AssembleError> {

    // }
    fn parse_program(
        tokens: &mut Peekable<vec::IntoIter<Token>>,
        pc: u64,
    ) -> Result<(Vec<Instruction>, Vec<(String, u64)>, u64), AssembleError> {
        let mut program: Vec<Instruction> = Vec::new();
        let mut program_labels: Vec<(String, u64)> = Vec::new();
        let mut pc: u64 = pc;
        loop {
            let token = tokens.next().unwrap();
            match token {
                Token::KeyWord(lexeme) => {
                    if lexeme.s.starts_with('!') {
                        println!("found program label `{}` : {pc}", lexeme.s);
                        program_labels.push((lexeme.s.strip_prefix('!').unwrap().to_string(), pc));
                    } else {
                        let (instr, instr_size) = Instruction::new(&lexeme, tokens)?;
                        pc += instr_size;
                        program.push(instr);
                    }
                }
                Token::EOS(_) => break,
                // this is incredibly gross
                _ => {
                    let lexeme = token.get_lexeme();
                    return Err(AssembleError::new(format!(
                        "unexpected token in program section `{}`",
                        lexeme.s
                    ))
                    .attach_lexeme(&lexeme));
                }
            }
        }
        Ok((program, program_labels, pc))
    }

    fn parse_data(
        &mut self,
        stream: &mut Peekable<vec::IntoIter<Token>>,
        // labels: &mut HashMap<String, (u64, bool)>,
        // data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        loop {
            let token = stream.next().unwrap();
            match token {
                Token::KeyWord(lexeme) => {
                    let mut label: (String, (u64, bool)) =
                        (lexeme.s, (self.data.len() as u64, true));
                    let cmd_token = stream.next().unwrap();
                    match cmd_token {
                        Token::KeyWord(lexeme) => match lexeme.s.as_str() {
                            "def" => self.parse_def(stream)?,
                            "str" => self.parse_str(stream)?,
                            "equ" => Self::parse_equ(
                                stream,
                                &self.labels,
                                &mut label,
                                self.data.len() as u64,
                            )?,
                            "set" => self.parse_set(stream)?,
                            _ => {
                                let msg = format!("data command not recognized `{}`", lexeme.s);
                                Err(AssembleError::new(msg).attach_lexeme(&lexeme))
                            }?,
                        },
                        _ => invalid_token_err!("expected data command `{}`", cmd_token)?,
                    }
                    self.labels.insert(label.0, label.1);
                }
                Token::SemiColon(_) => continue,
                Token::EOS(_) => break,
                _ => invalid_token_err!("expected label declaration `{}`", token)?,
            }
        }
        Ok(())
    }

    // returns bytes consumed
    fn parse_def(
        &mut self,
        stream: &mut Peekable<vec::IntoIter<Token>>,
        // labels: &HashMap<String, (u64, bool)>,
        // data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        let word_size = WordSize::from_token(stream.next().unwrap())?;
        let full_vec = {
            let mut buf: Vec<u64> = Vec::new();
            loop {
                let token = stream.next().unwrap();
                match token {
                    Token::Constant(lexeme, expected_offset) => {
                        buf.push(resolve_label(&self.labels, &lexeme, expected_offset)?)
                    }
                    Token::OpenParen(_) => {
                        let expr = build_expression(self.data.len() as u64, stream, &self.labels)?;
                        buf.push(resolve_expression(expr))
                    }
                    Token::SemiColon(_) => break,
                    _ => invalid_token_err!("unexpected token `{}`", token)?,
                }
            }
            buf
        };

        // improvement would be adding type checking, throwing a warning if a byte is downcast
        // f8 is actually the exact same as b8 as parse constant will transmute the value at parse time
        match word_size {
            WordSize::B1 => full_vec.iter().for_each(|v| self.data.push(*v as u8)),
            WordSize::B2 => full_vec
                .iter()
                .for_each(|v| self.data.extend((*v as u16).to_le_bytes())),
            WordSize::B4 => full_vec
                .iter()
                .for_each(|v| self.data.extend((*v as u16).to_le_bytes())),
            WordSize::B8 | WordSize::F8 => full_vec
                .iter()
                .for_each(|v| self.data.extend((*v).to_le_bytes())),
        }
        Ok(())
    }

    // returns bytes consumed, alias of def b1 which accepts strings
    fn parse_str(
        &mut self,
        stream: &mut Peekable<vec::IntoIter<Token>>,
        // labels: &HashMap<String, (u64, bool)>,
        // data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        loop {
            let token = stream.next().unwrap();
            match token {
                Token::String(lexeme) => self.data.extend(lexeme.s.into_bytes()),
                Token::Constant(lexeme, expected_offset) => {
                    self.data
                        .push(resolve_label(&self.labels, &lexeme, expected_offset)? as u8)
                }
                Token::OpenParen(_) => {
                    let expr = build_expression(self.data.len() as u64, stream, &self.labels)?;
                    self.data.push(resolve_expression(expr) as u8)
                }
                Token::SemiColon(_) => break,
                _ => invalid_token_err!("unexpected token `{}`", token)?,
            }
        }
        Ok(())
    }

    // returns bytes consumed
    fn parse_set(
        &mut self,
        stream: &mut Peekable<vec::IntoIter<Token>>,
        // data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        todo!()
    }
    // returns bytes consumed
    fn parse_equ(
        stream: &mut Peekable<vec::IntoIter<Token>>,
        labels: &HashMap<String, (u64, bool)>,
        label: &mut (String, (u64, bool)),
        data_counter: u64,
    ) -> Result<(), AssembleError> {
        let token = stream.next().unwrap();
        label.1 .1 = false;
        label.1 .0 = match token {
            Token::Constant(lexeme, expected_offset) => {
                resolve_label(labels, &lexeme, expected_offset)?
            }

            Token::OpenParen(_) => {
                let expr = build_expression(data_counter, stream, labels)?;
                resolve_expression(expr)
            }
            _ => invalid_token_err!("unexpected token `{}`", token)?,
        };
        Ok(())
    }
}
fn convert_to_symbolic_offset(offset: bool) -> char {
    match offset {
        true => '@',
        false => '$',
    }
}
fn resolve_label(
    labels: &HashMap<String, (u64, bool)>,
    lexeme: &Lexeme,
    expected_offset: bool,
) -> Result<u64, AssembleError> {
    println!("labels: {labels:?}");
    let constant = parse_constant(lexeme, expected_offset)?;
    match constant {
        Label::Resolved(value, offset) => {
            if offset != expected_offset {
                panic!("pretty sure thsi is impossible but idk man")
            }
            Ok(value)
        }
        Label::Unresolved(label, _, _) => match labels.get(&label) {
            Some((value, offset)) => {
                if *offset != expected_offset {
                    return Err(AssembleError::new(format!(
                        "expected '{}' for label `{}`",
                        convert_to_symbolic_offset(*offset),
                        lexeme.s
                    ))
                    .attach_lexeme(lexeme));
                }
                Ok(*value)
            }

            None => {
                return Err(AssembleError::new(format!("failed to resolve `{label}`"))
                    .attach_lexeme(lexeme))
            }
        },
    }
}
pub fn consume_register(
    stream: &mut Peekable<vec::IntoIter<Token>>,
) -> Result<RegHandle, AssembleError> {
    let reg_str = stream.next().unwrap();

    let code = match reg_str {
        Token::KeyWord(lexeme) => resolve_register(&lexeme)?,
        //eof =>
        _ => invalid_token_err!("invalid register `{}`", reg_str)?,
    };
    Ok(code)
}

pub fn consume_constant(
    stream: &mut Peekable<vec::IntoIter<Token>>,
) -> Result<Label, AssembleError> {
    let constant_str = stream.next().unwrap();

    let constant = match constant_str {
        Token::Constant(lexeme, offset) => parse_constant(&lexeme, offset)?,
        //eof =>
        _ => invalid_token_err!("invalid token `{}`", constant_str)?,
    };
    Ok(constant)
}

fn resolve_register(reg: &Lexeme) -> Result<RegHandle, AssembleError> {
    let code = match reg.s.as_str() {
        "null" => 0x00,
        "pc" => 0x01,
        "sp" => 0x02,
        "fp" => 0x03,
        _ => {
            if let Some(rest) = reg.s.strip_prefix('r') {
                let (id, sub) = rest.split_at(
                    rest.find(|c: char| !c.is_ascii_digit())
                        .unwrap_or(rest.len()),
                );
                let base = id.parse::<u8>().unwrap() + 3;
                match sub {
                    "b1" => base + 0x10,
                    "b2" => base + 0x20,
                    "b3" => base + 0x30,
                    "b4" => base + 0x40,
                    "b5" => base + 0x50,
                    "b6" => base + 0x60,
                    "b7" => base + 0x70,
                    "b8" => base + 0x80,
                    "q1" => base + 0x90,
                    "q2" => base + 0xa0,
                    "q3" => base + 0xb0,
                    "q4" => base + 0xc0,
                    "l" => base + 0xd0,
                    "h" => base + 0xe0,
                    "f" => base + 0x00,
                    "" => base + 0x00,
                    _ => {
                        return Err(AssembleError::new(format!(
                            "unrecognized subregister `{}` ({id}+{sub})",
                            reg.s
                        ))
                        .attach_lexeme(reg))
                    }
                }
            } else {
                return Err(
                    AssembleError::new(format!("unrecognized register `{}`", reg.s))
                        .attach_lexeme(reg),
                );
            }
        }
    };
    Ok(code)
}

fn parse_constant(lexeme: &Lexeme, offset: bool) -> Result<Label, AssembleError> {
    match lexeme.s.chars().nth(0).unwrap() {
        '!' => Ok(Label::Unresolved(
            lexeme.s.clone().strip_prefix('!').unwrap().to_string(),
            offset,
            lexeme.clone(),
        )),
        'x' => {
            let stripped = lexeme.s.strip_prefix('x').unwrap();
            let value = u64::from_str_radix(&stripped, 16).map_err(|e| {
                AssembleError::new(format!("failed to parse hex digit `{}`: {e}", lexeme.s))
            })?;
            Ok(Label::Resolved(value, offset))
        }
        'b' => {
            todo!()
        }
        'f' => {
            let stripped = lexeme.s.strip_prefix('f').unwrap();
            let float = stripped.parse::<f64>().map_err(|e| {
                AssembleError::new(format!("not a float: {e}")).attach_lexeme(lexeme)
            })?;
            let value = unsafe { transmute::<f64, u64>(float) };
            Ok(Label::Resolved(value, offset))
        }
        _ => {
            if !lexeme.s.chars().nth(0).unwrap().is_ascii_digit() {
                return Err(
                    AssembleError::new(format!("invalid constant value `{}`", lexeme.s))
                        .attach_lexeme(lexeme),
                );
            }
            let n = {
                if lexeme.s.starts_with('d') {
                    lexeme.s.strip_prefix('d').unwrap()
                } else {
                    lexeme.s.as_str()
                }
            };
            let value = n.parse().map_err(|e| {
                AssembleError::new(format!("invalid constant value `{}`: {e}", lexeme.s))
                    .attach_lexeme(lexeme)
            })?;
            Ok(Label::Resolved(value, offset))
        }
    }
}
enum DataExprNode {
    Literal(u64),
    Add(Box<DataExprNode>, Box<DataExprNode>),
    Sub(Box<DataExprNode>, Box<DataExprNode>),
    Mult(Box<DataExprNode>, Box<DataExprNode>),
    Div(Box<DataExprNode>, Box<DataExprNode>),
    Mod(Box<DataExprNode>, Box<DataExprNode>),
}

fn build_expression(
    dc: u64,
    stream: &mut Peekable<vec::IntoIter<Token>>,
    labels: &HashMap<String, (u64, bool)>,
) -> Result<DataExprNode, AssembleError> {
    let left_token = stream.next().unwrap();
    // let left = build_expression(dc, stream, labels)?;
    // let right = build_expression(dc, stream, labels)?;

    let left = match left_token {
        Token::Constant(lexeme, offset) => {
            // let value = parse_constant(&lexeme, offset)?;
            let value = resolve_label(labels, &lexeme, offset)?;

            DataExprNode::Literal(value)
        }
        Token::Dot(lexeme) => DataExprNode::Literal(dc),
        Token::OpenParen(lexeme) => build_expression(dc, stream, labels)?,
        _ => invalid_token_err!("invalid token in expression `{}`", left_token)?,
    };

    let center_token = stream.next().unwrap();
    match center_token {
        Token::EOF(_) => invalid_token_err!("unexpected EOF `{}`", center_token)?,
        _ => (),
    };

    let right_token = stream.next().unwrap();

    let right = match right_token {
        Token::Constant(lexeme, offset) => {
            let value = parse_constant(&lexeme, offset)?;
            match value {
                Label::Resolved(value, offset) => DataExprNode::Literal(value),
                Label::Unresolved(string, offset, _) => {
                    let resolve = if let Some((v, offset)) = labels.get(&string) {
                        *v
                    } else {
                        {
                            let msg = format!(
                                "cannot resolve label `{}` labels must be declared before use",
                                lexeme.s
                            );
                            Err(AssembleError::new(msg).attach_lexeme(&lexeme))
                        }?
                    };

                    DataExprNode::Literal(resolve)
                }
            }
        }

        Token::Dot(lexeme) => DataExprNode::Literal(dc),
        Token::OpenParen(lexeme) => build_expression(dc, stream, labels)?,
        _ => invalid_token_err!("invalid token in expression `{}`", right_token)?,
    };
    println!("token after parsing expr `{:?}`", stream.next().unwrap());
    match center_token {
        Token::Plus(_) => Ok(DataExprNode::Add(Box::new(left), Box::new(right))),
        Token::Dash(_) => Ok(DataExprNode::Sub(Box::new(left), Box::new(right))),
        Token::Star(_) => Ok(DataExprNode::Mult(Box::new(left), Box::new(right))),
        Token::Slash(_) => Ok(DataExprNode::Div(Box::new(left), Box::new(right))),
        Token::Percent(_) => Ok(DataExprNode::Mod(Box::new(left), Box::new(right))),

        _ => invalid_token_err!("invalid operator in expression `{}`", center_token)?,
    }
}

fn resolve_expression(expr: DataExprNode) -> u64 {
    match expr {
        DataExprNode::Literal(literal) => literal,
        DataExprNode::Add(left, right) => {
            let collapsed_left = resolve_expression(*left);
            let collapsed_right = resolve_expression(*right);
            collapsed_left + collapsed_right
        }
        DataExprNode::Sub(left, right) => {
            let collapsed_left = resolve_expression(*left);
            let collapsed_right = resolve_expression(*right);
            collapsed_left.saturating_sub(collapsed_right)
        }
        DataExprNode::Mult(left, right) => {
            let collapsed_left = resolve_expression(*left);
            let collapsed_right = resolve_expression(*right);
            collapsed_left * collapsed_right
        }
        DataExprNode::Div(left, right) => {
            let collapsed_left = resolve_expression(*left);
            let collapsed_right = resolve_expression(*right);
            collapsed_left / collapsed_right
        }
        DataExprNode::Mod(left, right) => {
            let collapsed_left = resolve_expression(*left);
            let collapsed_right = resolve_expression(*right);
            collapsed_left % collapsed_right
        }
    }
}
