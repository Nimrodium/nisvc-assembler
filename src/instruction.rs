use std::{iter::Peekable, vec};

use crate::{
    assembler::{consume_constant, consume_register, Label},
    tokenizer::{Lexeme, Token},
    AssembleError,
};

pub type RegHandle = u8;
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
    pub fn new(
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

    pub fn compile(&self) -> Vec<u8> {
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
