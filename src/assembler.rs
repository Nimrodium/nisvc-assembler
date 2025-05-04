use std::{collections::HashMap, iter::Peekable, mem::transmute, vec};

use crate::{
    tokenizer::{Lexeme, Source, Token},
    AssembleError,
};

macro_rules! invalid_token_err {
    ($msg:expr, $token:expr) => {{
        let lexeme = $token.get_lexeme();
        let msg = format!($msg, lexeme.s);
        Err(AssembleError::new(msg).attach_lexeme(lexeme))
    }};
}

type RegHandle = u8;
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
#[derive(Debug)]
pub enum Instruction {
    Nop,
    Cpy {
        dest: RegHandle,
        src: RegHandle,
    },
    Ldi {
        dest: RegHandle,
        src: Label,
    },
    Load {
        dest: RegHandle,
        n: RegHandle,
        addr: RegHandle,
    },
    Store {
        dest: RegHandle,
        n: RegHandle,
        src: RegHandle,
    },
    Add {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Sub {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Mult {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Div {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },

    Or {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Xor {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    And {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Not {
        dest: RegHandle,
        op: RegHandle,
    },
    Shl {
        dest: RegHandle,
        n: RegHandle,
        src: RegHandle,
    },
    Shr {
        dest: RegHandle,
        n: RegHandle,
        src: RegHandle,
    },
    Rotl {
        dest: RegHandle,
        n: RegHandle,
        src: RegHandle,
    },
    Rotr {
        dest: RegHandle,
        n: RegHandle,
        src: RegHandle,
    },
    Neg {
        dest: RegHandle,
        op: RegHandle,
    },

    Jmp {
        addr: Label,
    },
    Jifz {
        addr: Label,
        condition: RegHandle,
    },
    Jifnz {
        addr: Label,
        condition: RegHandle,
    },

    Inc {
        reg: RegHandle,
    },
    Dec {
        reg: RegHandle,
    },

    Push {
        src: RegHandle,
    },
    Pop {
        dest: RegHandle,
    },

    Call {
        addr: Label,
    },
    Ret,
    // fopen fd_store filep_ptr filep_len
    // fwrite fd str_ptr str_len
    // fread fd buf_ptr buf_len
    // fclose fd
    // Fopen {
    //     dest_fd: RegHandle,
    //     file_path_str_ptr: RegHandle,
    //     file_path_str_len: RegHandle,
    // },
    // Fread {
    //     fd: RegHandle,
    //     buf_ptr: RegHandle,
    //     buf_len: RegHandle,
    // },
    // Fwrite {
    //     fd: RegHandle,
    //     buf_ptr: RegHandle,
    //     buf_len: RegHandle,
    // },
    // Fseek {
    //     fd: RegHandle,
    //     seek: RegHandle,
    //     direction: RegHandle,
    // },
    // Fclose {
    //     fd: RegHandle,
    // },
    // //new

    // //heap management
    // Malloc {
    //     dest_ptr: RegHandle,
    //     size: RegHandle,
    // },
    // Realloc {
    //     dest_ptr: RegHandle,
    //     ptr: RegHandle,
    //     new_size: RegHandle,
    // },
    // Free {
    //     ptr: RegHandle,
    // },
    // Memcpy {
    //     dest: RegHandle,
    //     n: RegHandle,
    //     src: RegHandle,
    // },
    // Memset {
    //     dest: RegHandle,
    //     n: RegHandle,
    //     value: RegHandle,
    // },

    // floating point
    Itof {
        destf: RegHandle,
        srci: RegHandle,
    },
    Ftoi {
        desti: RegHandle,
        srcf: RegHandle,
    },

    Fadd {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Fsub {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Fmult {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Fdiv {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Fmod {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Mod {
        dest: RegHandle,
        op1: RegHandle,
        op2: RegHandle,
    },
    Int {
        code: Label,
    },
    Pushi {
        immediate: Label,
    },
    Breakpoint,
    HaltExe,
}

impl Instruction {
    fn new(
        lexeme: &Lexeme,
        stream: &mut Peekable<vec::IntoIter<Token>>,
    ) -> Result<(Self, u64), AssembleError> {
        match lexeme.s.as_str() {
            "nop" => Ok((Self::Nop, 1)),
            "cpy" => Ok((
                Self::Cpy {
                    dest: consume_register(stream)?,
                    src: consume_register(stream)?,
                },
                3,
            )),
            "ldi" => Ok((
                Self::Ldi {
                    dest: consume_register(stream)?,
                    src: consume_constant(stream)?,
                },
                10,
            )),
            "load" => Ok((
                Self::Load {
                    dest: consume_register(stream)?,
                    n: consume_register(stream)?,
                    addr: consume_register(stream)?,
                },
                4,
            )),
            "store" => Ok((
                Self::Store {
                    dest: consume_register(stream)?,
                    n: consume_register(stream)?,
                    src: consume_register(stream)?,
                },
                4,
            )),
            "add" => Ok((
                Self::Add {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "sub" => Ok((
                Self::Sub {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "mult" => Ok((
                Self::Mult {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "div" => Ok((
                Self::Div {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "or" => Ok((
                Self::Or {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "xor" => Ok((
                Self::Xor {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "and" => Ok((
                Self::And {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "not" => Ok((
                Self::Not {
                    dest: consume_register(stream)?,
                    op: consume_register(stream)?,
                },
                3,
            )),
            "shl" => Ok((
                Self::Shl {
                    dest: consume_register(stream)?,
                    n: consume_register(stream)?,
                    src: consume_register(stream)?,
                },
                4,
            )),
            "shr" => Ok((
                Self::Shr {
                    dest: consume_register(stream)?,
                    n: consume_register(stream)?,
                    src: consume_register(stream)?,
                },
                4,
            )),
            "rotl" => Ok((
                Self::Rotl {
                    dest: consume_register(stream)?,
                    n: consume_register(stream)?,
                    src: consume_register(stream)?,
                },
                4,
            )),
            "rotr" => Ok((
                Self::Rotr {
                    dest: consume_register(stream)?,
                    n: consume_register(stream)?,
                    src: consume_register(stream)?,
                },
                4,
            )),
            "neg" => Ok((
                Self::Neg {
                    dest: consume_register(stream)?,
                    op: consume_register(stream)?,
                },
                3,
            )),
            "jmp" => Ok((
                Self::Jmp {
                    addr: consume_constant(stream)?,
                },
                9,
            )),
            "jifz" => Ok((
                Self::Jifz {
                    condition: consume_register(stream)?,
                    addr: consume_constant(stream)?,
                },
                10,
            )),
            "jifnz" => Ok((
                Self::Jifnz {
                    condition: consume_register(stream)?,
                    addr: consume_constant(stream)?,
                },
                10,
            )),
            "inc" => Ok((
                Self::Inc {
                    reg: consume_register(stream)?,
                },
                2,
            )),
            "dec" => Ok((
                Self::Dec {
                    reg: consume_register(stream)?,
                },
                2,
            )),
            "push" => Ok((
                Self::Push {
                    src: consume_register(stream)?,
                },
                2,
            )),
            "pop" => Ok((
                Self::Pop {
                    dest: consume_register(stream)?,
                },
                2,
            )),
            "call" => Ok((
                Self::Call {
                    addr: consume_constant(stream)?,
                },
                9,
            )),
            "ret" => Ok((Self::Ret, 1)),
            // "fopen" => Ok((
            //     Self::Fopen {
            //         dest_fd: consume_register(stream)?,
            //         file_path_str_ptr: consume_register(stream)?,
            //         file_path_str_len: consume_register(stream)?,
            //     },
            //     4,
            // )),
            // "fread" => Ok((
            //     Self::Fread {
            //         fd: consume_register(stream)?,
            //         buf_ptr: consume_register(stream)?,
            //         buf_len: consume_register(stream)?,
            //     },
            //     4,
            // )),
            // "fwrite" => Ok((
            //     Self::Fwrite {
            //         fd: consume_register(stream)?,
            //         buf_ptr: consume_register(stream)?,
            //         buf_len: consume_register(stream)?,
            //     },
            //     4,
            // )),
            // "fseek" => Ok((
            //     Self::Fseek {
            //         fd: consume_register(stream)?,
            //         seek: consume_register(stream)?,
            //         direction: consume_register(stream)?,
            //     },
            //     4,
            // )),
            // "fclose" => Ok((
            //     Self::Fclose {
            //         fd: consume_register(stream)?,
            //     },
            //     2,
            // )),
            // "malloc" => Ok((
            //     Self::Malloc {
            //         dest_ptr: consume_register(stream)?,
            //         size: consume_register(stream)?,
            //     },
            //     3,
            // )),
            // "realloc" => Ok((
            //     Self::Realloc {
            //         dest_ptr: consume_register(stream)?,
            //         ptr: consume_register(stream)?,
            //         new_size: consume_register(stream)?,
            //     },
            //     4,
            // )),
            // "free" => Ok((
            //     Self::Free {
            //         ptr: consume_register(stream)?,
            //     },
            //     2,
            // )),
            // "memcpy" => Ok((
            //     Self::Memcpy {
            //         dest: consume_register(stream)?,
            //         n: consume_register(stream)?,
            //         src: consume_register(stream)?,
            //     },
            //     4,
            // )),
            // "memset" => Ok((
            //     Self::Memset {
            //         dest: consume_register(stream)?,
            //         n: consume_register(stream)?,
            //         value: consume_register(stream)?,
            //     },
            //     4,
            // )),
            "itof" => Ok((
                Self::Itof {
                    destf: consume_register(stream)?,
                    srci: consume_register(stream)?,
                },
                3,
            )),
            "ftoi" => Ok((
                Self::Ftoi {
                    desti: consume_register(stream)?,
                    srcf: consume_register(stream)?,
                },
                3,
            )),
            "fadd" => Ok((
                Self::Fadd {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "fsub" => Ok((
                Self::Fsub {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "fmult" => Ok((
                Self::Fmult {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "fdiv" => Ok((
                Self::Fdiv {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "fmod" => Ok((
                Self::Fmod {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "mod" => Ok((
                Self::Mod {
                    dest: consume_register(stream)?,
                    op1: consume_register(stream)?,
                    op2: consume_register(stream)?,
                },
                4,
            )),
            "int" => Ok((
                Self::Int {
                    code: consume_constant(stream)?,
                },
                9,
            )),

            "pushi" => Ok((
                Self::Pushi {
                    immediate: consume_constant(stream)?,
                },
                9,
            )),
            "haltexe" => Ok((Self::HaltExe, 1)),
            _ => Err(
                AssembleError::new(format!("unrecognized opcode `{}`", lexeme.s))
                    .attach_lexeme(lexeme),
            ),
        }
    }

    fn compile(&self) -> Vec<u8> {
        match self {
            Instruction::Nop => vec![0x00],
            Instruction::Cpy { dest, src } => vec![0x01, *dest, *src],
            Instruction::Ldi { dest, src } => vec![0x02, *dest]
                .into_iter()
                .chain(match src {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Load { dest, n, addr } => vec![0x03, *dest, *n, *addr],
            Instruction::Store { dest, n, src } => vec![0x04, *dest, *n, *src],
            Instruction::Add { dest, op1, op2 } => vec![0x05, *dest, *op1, *op2],
            Instruction::Sub { dest, op1, op2 } => vec![0x06, *dest, *op1, *op2],
            Instruction::Mult { dest, op1, op2 } => vec![0x07, *dest, *op1, *op2],
            Instruction::Div { dest, op1, op2 } => vec![0x08, *dest, *op1, *op2],
            Instruction::Or { dest, op1, op2 } => vec![0x09, *dest, *op1, *op2],
            Instruction::Xor { dest, op1, op2 } => vec![0x0a, *dest, *op1, *op2],
            Instruction::And { dest, op1, op2 } => vec![0x0b, *dest, *op1, *op2],
            Instruction::Not { dest, op } => vec![0x0c, *dest, *op],
            Instruction::Shl { dest, n, src } => vec![0x0d, *dest, *n, *src],
            Instruction::Shr { dest, n, src } => vec![0x0e, *dest, *n, *src],
            Instruction::Rotl { dest, n, src } => vec![0x0f, *dest, *n, *src],
            Instruction::Rotr { dest, n, src } => vec![0x10, *dest, *n, *src],
            Instruction::Neg { dest, op } => vec![0x11, *dest, *op],
            Instruction::Jmp { addr } => vec![0x12]
                .into_iter()
                .chain(match addr {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Jifz { addr, condition } => vec![0x13]
                .into_iter()
                .chain(match addr {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Jifnz { addr, condition } => vec![0x14]
                .into_iter()
                .chain(match addr {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Inc { reg } => vec![0x16, *reg],
            Instruction::Dec { reg } => vec![0x17, *reg],
            Instruction::Push { src } => vec![0x18, *src],
            Instruction::Pop { dest } => vec![0x19, *dest],
            Instruction::Call { addr } => vec![0x1a]
                .into_iter()
                .chain(match addr {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Ret => vec![],
            // Instruction::Fopen { dest_fd, file_path_str_ptr, file_path_str_len } => vec![0],
            // Instruction::Fread { fd, buf_ptr, buf_len } => vec![],
            // Instruction::Fwrite { fd, buf_ptr, buf_len } => vec![],
            // Instruction::Fseek { fd, seek, direction } => vec![],
            // Instruction::Fclose { fd } => vec![],
            // Instruction::Malloc { dest_ptr, size } => vec![],
            // Instruction::Realloc { dest_ptr, ptr, new_size } => vec![],
            // Instruction::Free { ptr } => vec![],
            // Instruction::Memcpy { dest, n, src } => vec![],
            // Instruction::Memset { dest, n, value } => vec![],
            Instruction::Itof { destf, srci } => vec![0x1b, *destf, *srci],
            Instruction::Ftoi { desti, srcf } => vec![0x1c, *desti, *srcf],
            Instruction::Fadd { dest, op1, op2 } => vec![0x1d, *dest, *op1, *op2],
            Instruction::Fsub { dest, op1, op2 } => vec![0x1f, *dest, *op1, *op2],
            Instruction::Fmult { dest, op1, op2 } => vec![0x20, *dest, *op1, *op2],
            Instruction::Fdiv { dest, op1, op2 } => vec![0x21, *dest, *op1, *op2],
            Instruction::Fmod { dest, op1, op2 } => vec![0x22, *dest, *op1, *op2],
            Instruction::Mod { dest, op1, op2 } => vec![0x23, *dest, *op1, *op2],
            Instruction::Int { code } => vec![0x24]
                .into_iter()
                .chain(match code {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Pushi { immediate } => vec![0x25]
                .into_iter()
                .chain(match immediate {
                    Label::Resolved(v, _) => v.to_le_bytes(),
                    _ => unreachable!(),
                })
                .collect(),
            Instruction::Breakpoint => panic!("deprecated"),
            Instruction::HaltExe => panic!("deprecated"),
        }
    }
}
enum ParserSectionState {
    Initial,
    Program,
    Data,
    Entry,
}

pub struct Assembler {
    labels: HashMap<String, (u64, bool)>,
    program_intermediate: Vec<Instruction>,
    data: Vec<u8>,
    program_size: u64,
    entry_point: u64,
}

enum Section {
    Uninit,
    Entry,
    Data,
    Program,
    EOF,
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

impl Assembler {
    pub fn assemble(tokens: Vec<Token>) -> Result<(u64, Vec<u8>, Vec<u8>, Vec<u8>), AssembleError> {
        let mut assembler = Self::parse(tokens)?;
        assembler.resolve()?;
        let program = assembler.emit();
        let debug_symbols = assembler.build_debug_symbol_table();
        Ok((
            assembler.entry_point,
            assembler.data,
            program,
            debug_symbols,
        ))
    }
    fn build_debug_symbol_table(&self) -> Vec<u8> {
        let mut image = Vec::<u8>::new();
        for label in &self.labels {
            image.extend(label.1 .0.to_le_bytes());
            image.extend((label.0.len() as u64).to_le_bytes());
            image.extend(label.0.clone().into_bytes())
        }
        image
    }
    fn emit(&self) -> Vec<u8> {
        let mut program_image = Vec::<u8>::new();
        for instruction in &self.program_intermediate {
            program_image.extend(instruction.compile());
        }
        program_image
    }
    fn resolve(&mut self) -> Result<(), AssembleError> {
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

    fn parse(tokens: Vec<Token>) -> Result<Self, AssembleError> {
        let mut stream = tokens.into_iter().peekable();
        let mut labels: HashMap<String, (u64, bool)> = HashMap::new();
        let mut data: Vec<u8> = Vec::new();
        let mut entry_point_label: Option<Label> = None;
        let mut program_intermediate: Option<Vec<Instruction>> = None;
        let mut program_size = 0;
        loop {
            let token = stream.next().unwrap();
            match token {
                Token::EOF(_) => break,
                Token::PROGRAM(_) => {
                    let (program, program_labels, added_pc) = Self::parse_program(&mut stream)?;
                    for (name, address) in program_labels {
                        labels.insert(name, (address, false));
                    }
                    program_size += added_pc;
                    program_intermediate = Some(program);
                }
                Token::DATA(_) => {
                    Self::parse_data_reimpl(&mut stream, &mut labels, &mut data)?;
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
        Ok(Self {
            program_intermediate: if let Some(p) = program_intermediate {
                p
            } else {
                return Err(AssembleError::new("no program defined".to_string()));
            },
            data,
            program_size,
            entry_point: if let Some(mut e) = entry_point_label {
                e.resolve(&labels, program_size)?;
                match e {
                    Label::Resolved(v, _) => v,
                    Label::Unresolved(_, _, _) => unreachable!(),
                }
            } else {
                return Err(AssembleError::new("no entry point defined".to_string()));
            },
            labels,
        })
    }

    // fn parse_entry(
    //     stream: &mut Peekable<vec::IntoIter<Token>>,
    // ) -> Result<(String, AssembleError> {

    // }
    fn parse_program(
        tokens: &mut Peekable<vec::IntoIter<Token>>,
    ) -> Result<(Vec<Instruction>, Vec<(String, u64)>, u64), AssembleError> {
        let mut program: Vec<Instruction> = Vec::new();
        let mut program_labels: Vec<(String, u64)> = Vec::new();
        let mut pc: u64 = 0;
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

    fn parse_data_reimpl(
        stream: &mut Peekable<vec::IntoIter<Token>>,
        labels: &mut HashMap<String, (u64, bool)>,
        data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        loop {
            let token = stream.next().unwrap();
            match token {
                Token::KeyWord(lexeme) => {
                    let mut label: (String, (u64, bool)) = (lexeme.s, (data.len() as u64, true));
                    let cmd_token = stream.next().unwrap();
                    match cmd_token {
                        Token::KeyWord(lexeme) => match lexeme.s.as_str() {
                            "def" => Self::parse_def(stream, labels, data)?,
                            "str" => Self::parse_str(stream, labels, data)?,
                            "equ" => {
                                Self::parse_equ(stream, labels, &mut label, data.len() as u64)?
                            }
                            "set" => Self::parse_set(stream, data)?,
                            _ => {
                                let msg = format!("data command not recognized `{}`", lexeme.s);
                                Err(AssembleError::new(msg).attach_lexeme(&lexeme))
                            }?,
                        },
                        _ => invalid_token_err!("expected data command `{}`", cmd_token)?,
                    }
                    labels.insert(label.0, label.1);
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
        stream: &mut Peekable<vec::IntoIter<Token>>,
        labels: &HashMap<String, (u64, bool)>,
        data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        let word_size = WordSize::from_token(stream.next().unwrap())?;
        let full_vec = {
            let mut buf: Vec<u64> = Vec::new();
            loop {
                let token = stream.next().unwrap();
                match token {
                    Token::Constant(lexeme, expected_offset) => {
                        buf.push(resolve_label(labels, &lexeme, expected_offset)?)
                    }
                    Token::OpenParen(_) => {
                        let expr = build_expression(data.len() as u64, stream, labels)?;
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
            WordSize::B1 => full_vec.iter().for_each(|v| data.push(*v as u8)),
            WordSize::B2 => full_vec
                .iter()
                .for_each(|v| data.extend((*v as u16).to_le_bytes())),
            WordSize::B4 => full_vec
                .iter()
                .for_each(|v| data.extend((*v as u16).to_le_bytes())),
            WordSize::B8 | WordSize::F8 => full_vec
                .iter()
                .for_each(|v| data.extend((*v).to_le_bytes())),
        }
        Ok(())
    }

    // returns bytes consumed, alias of def b1 which accepts strings
    fn parse_str(
        stream: &mut Peekable<vec::IntoIter<Token>>,
        labels: &HashMap<String, (u64, bool)>,
        data: &mut Vec<u8>,
    ) -> Result<(), AssembleError> {
        loop {
            let token = stream.next().unwrap();
            match token {
                Token::String(lexeme) => data.extend(lexeme.s.into_bytes()),
                Token::Constant(lexeme, expected_offset) => {
                    data.push(resolve_label(labels, &lexeme, expected_offset)? as u8)
                }
                Token::OpenParen(_) => {
                    let expr = build_expression(data.len() as u64, stream, labels)?;
                    data.push(resolve_expression(expr) as u8)
                }
                Token::SemiColon(_) => break,
                _ => invalid_token_err!("unexpected token `{}`", token)?,
            }
        }
        Ok(())
    }

    // returns bytes consumed
    fn parse_set(
        stream: &mut Peekable<vec::IntoIter<Token>>,
        data: &mut Vec<u8>,
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
fn consume_register(
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

fn consume_constant(stream: &mut Peekable<vec::IntoIter<Token>>) -> Result<Label, AssembleError> {
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
