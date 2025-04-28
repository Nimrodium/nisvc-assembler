use std::{collections::HashMap, intrinsics::unreachable, iter::Peekable, vec};

use crate::{
    tokenizer::{Lexeme, Source, Token},
    AssembleError,
};

type RegHandle = u8;
enum Label {
    Resolved(u64,bool),
    Unresolved(String,bool),
}

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
        addr: Label,
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
    Fopen {
        dest_fd: RegHandle,
        file_path_str_ptr: RegHandle,
        file_path_str_len: RegHandle,
    },
    Fread {
        fd: RegHandle,
        buf_ptr: RegHandle,
        buf_len: RegHandle,
    },
    Fwrite {
        fd: RegHandle,
        buf_ptr: RegHandle,
        buf_len: RegHandle,
    },
    Fseek {
        fd: RegHandle,
        seek: RegHandle,
        direction: RegHandle,
    },
    Fclose {
        fd: RegHandle,
    },
    //new

    //heap management
    Malloc {
        dest_ptr: RegHandle,
        size: RegHandle,
    },
    Realloc {
        dest_ptr: RegHandle,
        ptr: RegHandle,
        new_size: RegHandle,
    },
    Free {
        ptr: RegHandle,
    },
    Memcpy {
        dest: RegHandle,
        n: RegHandle,
        src: RegHandle,
    },
    Memset {
        dest: RegHandle,
        n: RegHandle,
        value: RegHandle,
    },

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
    Breakpoint,
    HaltExe,
}

impl Instruction {
    fn new(lexeme: &Lexeme, stream: &mut Peekable<vec::IntoIter<Token>>) -> Result<(Self, u64), AssembleError> {
        match lexeme.s.as_str() {
            "nop"  => Ok((Self::Nop, 1)),
            "cpy"  => Ok((Self::Cpy { dest: consume_register(stream)?, src: consume_register(stream)? }, 3)),
            "ldi"  => Ok((Self::Ldi { dest: consume_register(stream)?, src: consume_constant(stream)? }, 10)),
            "load" => Ok((Self::Load { dest: consume_register(stream)?, n: consume_register(stream)?, addr: consume_constant(stream)? }, 10)),
            "store"=> Ok((Self::Store { dest: consume_register(stream)?, n: consume_register(stream)?, src: consume_register(stream)? }, 4)),
            "add"  => Ok((Self::Add { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "sub"  => Ok((Self::Sub { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "mult" => Ok((Self::Mult { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "div"  => Ok((Self::Div { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "or"   => Ok((Self::Or { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "xor"  => Ok((Self::Xor { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "and"  => Ok((Self::And { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "not"  => Ok((Self::Not { dest: consume_register(stream)?, op: consume_register(stream)? }, 3)),
            "shl"  => Ok((Self::Shl { dest: consume_register(stream)?, n: consume_register(stream)?, src: consume_register(stream)? }, 4)),
            "shr"  => Ok((Self::Shr { dest: consume_register(stream)?, n: consume_register(stream)?, src: consume_register(stream)? }, 4)),
            "rotl" => Ok((Self::Rotl { dest: consume_register(stream)?, n: consume_register(stream)?, src: consume_register(stream)? }, 4)),
            "rotr" => Ok((Self::Rotr { dest: consume_register(stream)?, n: consume_register(stream)?, src: consume_register(stream)? }, 4)),
            "neg"  => Ok((Self::Neg { dest: consume_register(stream)?, op: consume_register(stream)? }, 3)),
            "jmp"  => Ok((Self::Jmp { addr: consume_constant(stream)? }, 9)),
            "jifz" => Ok((Self::Jifz { condition: consume_register(stream)?,addr: consume_constant(stream)? }, 10)),
            "jifnz"=> Ok((Self::Jifnz {  condition: consume_register(stream)?, addr: consume_constant(stream)? }, 10)),
            "inc"  => Ok((Self::Inc { reg: consume_register(stream)? }, 2)),
            "dec"  => Ok((Self::Dec { reg: consume_register(stream)? }, 2)),
            "push" => Ok((Self::Push { src: consume_register(stream)? }, 2)),
            "pop"  => Ok((Self::Pop { dest: consume_register(stream)? }, 2)),
            "call" => Ok((Self::Call { addr: consume_constant(stream)? }, 9)),
            "ret"  => Ok((Self::Ret, 1)),
            "fopen"=> Ok((Self::Fopen { dest_fd: consume_register(stream)?, file_path_str_ptr: consume_register(stream)?, file_path_str_len: consume_register(stream)? }, 4)),
            "fread"=> Ok((Self::Fread { fd: consume_register(stream)?, buf_ptr: consume_register(stream)?, buf_len: consume_register(stream)? }, 4)),
            "fwrite"=>Ok((Self::Fwrite { fd: consume_register(stream)?, buf_ptr: consume_register(stream)?, buf_len: consume_register(stream)? }, 4)),
            "fseek"=> Ok((Self::Fseek { fd: consume_register(stream)?, seek: consume_register(stream)?, direction: consume_register(stream)? }, 4)),
            "fclose"=>Ok((Self::Fclose { fd: consume_register(stream)? }, 2)),
            "malloc"=>Ok((Self::Malloc { dest_ptr: consume_register(stream)?, size: consume_register(stream)? }, 3)),
            "realloc"=>Ok((Self::Realloc { dest_ptr: consume_register(stream)?, ptr: consume_register(stream)?, new_size: consume_register(stream)? }, 4)),
            "free" => Ok((Self::Free { ptr: consume_register(stream)? }, 2)),
            "memcpy"=>Ok((Self::Memcpy { dest: consume_register(stream)?, n: consume_register(stream)?, src: consume_register(stream)? }, 4)),
            "memset"=>Ok((Self::Memset { dest: consume_register(stream)?, n: consume_register(stream)?, value: consume_register(stream)? }, 4)),
            "itof" => Ok((Self::Itof { destf: consume_register(stream)?, srci: consume_register(stream)? }, 3)),
            "ftoi" => Ok((Self::Ftoi { desti: consume_register(stream)?, srcf: consume_register(stream)? }, 3)),
            "fadd" => Ok((Self::Fadd { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "fsub" => Ok((Self::Fsub { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "fmult"=> Ok((Self::Fmult { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "fdiv" => Ok((Self::Fdiv { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "fmod" => Ok((Self::Fmod { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "mod"  => Ok((Self::Mod { dest: consume_register(stream)?, op1: consume_register(stream)?, op2: consume_register(stream)? }, 4)),
            "haltexe"=>Ok((Self::HaltExe, 1)),
            _ => Err(AssembleError::new(format!("unrecognized opcode `{}`", lexeme.s)).attach_lexeme(lexeme)),
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
    labels: HashMap<String, usize>,
    program_intermediate: Vec<Instruction>,
    data_intermediate: Vec<u8>,
}

enum Section {
    Uninit,
    Entry,
    Data,
    Program,
    EOF,
}

impl Assembler {
    pub fn assemble(tokens: Vec<Token>) -> Result<(u64, Vec<u8>, Vec<u8>, Vec<u8>), AssembleError> {
        todo!()
    }

    fn parse(tokens: Vec<Token>, source: &Source) -> Result<Self, AssembleError> {
        let stream = tokens.into_iter().peekable();
        let section = Section::Uninit;
        todo!()
    }

    fn parse_program(
        tokens: &mut Peekable<vec::IntoIter<Token>>,
    ) -> Result<(Vec<u8>,Vec<Label>), AssembleError> {
        let mut program : Vec<Instruction> = Vec::new();
        let mut program_labels
        loop {
            let token = match tokens.peek().unwrap() {
                Token::EOF(_) => break,
                Token::DATA(_) => break,
                Token::ENTRYPOINT(_) => break,
                Token::PROGRAM(lexeme) => {
                    return Err(AssembleError::new(
                        "unexpected program header inside program section".to_string(),
                    )
                    .attach_lexeme(lexeme))
                }
                _ => tokens.next().unwrap(),
            };
            match token {
                Token::KeyWord(lexeme) => {
                    if lexeme.s.starts_with('!'){

                    }
                },


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
        todo!()
    }

    fn build_data(tokens: &mut vec::IntoIter<Token>) {
        todo!()
    }

    fn resolve(&mut self) -> Result<(), AssembleError> {
        todo!()
    }

    pub fn emit() -> Result<(), AssembleError> {
        todo!()
    }
}

fn consume_register(
    stream: &mut Peekable<vec::IntoIter<Token>>,
) -> Result<RegHandle, AssembleError> {
    let reg_str = stream.next().unwrap();

    let code = match reg_str {
        Token::KeyWord(lexeme) => resolve_register(&lexeme)?,
        //eof =>
        _ => {
            let lexeme = reg_str.get_lexeme();
            return Err(
                AssembleError::new(format!("unexpected token `{}`", lexeme.s))
                    .attach_lexeme(lexeme),
            );
        }
    };
    Ok(code)
}

fn consume_constant(stream: &mut Peekable<vec::IntoIter<Token>>) -> Result<Label,AssembleError>{
    let constant_str = stream.next().unwrap();

    let constant = match constant_str {
        Token::Constant(lexeme,offset) => parse_constant(&lexeme,offset)?,
        //eof =>
        _ => {
            let lexeme = constant_str.get_lexeme();
            return Err(
                AssembleError::new(format!("unexpected token `{}`", lexeme.s))
                    .attach_lexeme(lexeme),
            );
        }
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
                let base = id.parse::<u8>().unwrap() + 4;
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
                    _ => {
                        return Err(AssembleError::new(format!(
                            "unrecognized subregister `{}`",
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

fn parse_constant(lexeme:&Lexeme,offset:bool) -> Result<Label,AssembleError>{

    match lexeme.s.chars().nth(0).unwrap(){
        '!' => {Ok(Label::Unresolved(lexeme.s.clone(),offset))}
        'x' => {todo!()}
        'b' => {todo!()}
        _ => {

            if !lexeme.s.chars().nth(0).unwrap().is_ascii_digit(){
                return Err(AssembleError::new(format!("invalid constant value `{}`",lexeme.s)).attach_lexeme(lexeme))
            }
            let value : u64 = if let Some(n) = lexeme.s.strip_prefix('d'){
                n.parse().map_err(|e|AssembleError::new(format!("invalid constant value `{}`: {e}",lexeme.s)).attach_lexeme(lexeme))?
            }else{lexeme.s.parse().map_err(|e|AssembleError::new(format!("invalid constant value `{}`: {e}",lexeme.s)).attach_lexeme(lexeme))?};
            Ok(Label::Resolved(value,offset))
        }
    }

}
