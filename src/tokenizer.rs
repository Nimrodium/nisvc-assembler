use std::{
    collections::HashMap,
    fs::File,
    io::{stdout, Read, Write},
    iter::repeat_n,
};

use colorize::AnsiColor;

use crate::AssembleError;

pub struct Source {
    files: HashMap<usize, String>,
    paths: HashMap<usize, String>,
    next_fd: usize,
}
impl Source {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            paths: HashMap::new(),
            next_fd: 0,
        }
    }
    pub fn open_file(&mut self, path: &str) -> Result<(usize, String), AssembleError> {
        let mut f = File::open(path)
            .map_err(|e| AssembleError::new(format!("could not open file `{path}`: {e}")))?;
        let mut contents: String = String::new();
        f.read_to_string(&mut contents).map_err(|e| {
            AssembleError::new(format!("could not read file `{path}` into memory: {e}"))
        })?;
        let fd = self.next_fd;
        self.next_fd += 1;
        self.paths.insert(fd, path.to_string());
        self.files.insert(fd, contents.clone());
        Ok((fd, contents))
    }
    pub fn traceback(&self, lexeme: &Lexeme) -> String {
        let file = self.files.get(&lexeme.fd).unwrap().lines();
        let path = self.paths.get(&lexeme.fd).unwrap();
        let n = lexeme.line - 1; // no idea

        let line: String = file
            .clone()
            .nth(n)
            .expect(&format!(
                "failed to get line `{}`, lexeme {}, in file {path}\n {:#?}",
                n,
                lexeme.s,
                file.enumerate().collect::<Vec<(usize, &str)>>(),
            ))
            .to_string();

        let highlight = {
            let mut buf: String = repeat_n(' ', lexeme.column.saturating_sub(2)).collect();
            buf.push_str("^".red().as_str());
            let squigle: String = repeat_n('~', lexeme.s.len().saturating_sub(1)).collect();
            buf.push_str(&(squigle.yellow()));
            buf
        };

        let msg = format!(
            "at {path}:{}:{}:\n{line}\n{highlight}",
            lexeme.line,
            lexeme.column - 1,
        );
        msg
    }
    pub fn exists(&self, file_path: &str) -> bool {
        for (_, val) in &self.paths {
            if val == file_path {
                return true;
            }
        }
        false
    }
}
#[derive(Debug)]
pub enum Token {
    INCLUDE(Lexeme),
    PROGRAM(Lexeme),
    DATA(Lexeme),
    ENTRYPOINT(Lexeme),
    KeyWord(Lexeme),
    Constant(Lexeme, bool),
    String(Lexeme),
    Plus(Lexeme),
    Dash(Lexeme),
    Star(Lexeme),
    Slash(Lexeme),
    Percent(Lexeme),
    OpenParen(Lexeme),
    ClosedParen(Lexeme),
    OpenBracket(Lexeme),
    ClosedBracket(Lexeme),
    SemiColon(Lexeme),
    Comma(Lexeme),
    Dot(Lexeme),
    EOS(Lexeme),
    EOL(Lexeme),
    EOF(Lexeme),
}

impl Token {
    pub fn get_lexeme(&self) -> &Lexeme {
        match self {
            Token::INCLUDE(lexeme) => lexeme,
            Token::PROGRAM(lexeme) => lexeme,
            Token::DATA(lexeme) => lexeme,
            Token::ENTRYPOINT(lexeme) => lexeme,
            Token::KeyWord(lexeme) => lexeme,
            Token::Constant(lexeme, _) => lexeme,
            Token::String(lexeme) => lexeme,
            Token::Plus(lexeme) => lexeme,
            Token::Dash(lexeme) => lexeme,
            Token::Star(lexeme) => lexeme,
            Token::Slash(lexeme) => lexeme,
            Token::Percent(lexeme) => lexeme,
            Token::OpenParen(lexeme) => lexeme,
            Token::ClosedParen(lexeme) => lexeme,
            Token::OpenBracket(lexeme) => lexeme,
            Token::ClosedBracket(lexeme) => lexeme,
            Token::SemiColon(lexeme) => lexeme,
            Token::EOF(lexeme) => lexeme,
            Token::Comma(lexeme) => lexeme,
            Token::Dot(lexeme) => lexeme,
            Token::EOS(lexeme) => lexeme,
            Token::EOL(lexeme) => lexeme,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme {
    pub s: String,
    line: usize,
    column: usize,
    fd: usize,
}

impl Lexeme {
    fn new(s: &str, line: usize, column: usize, fd: usize) -> Self {
        Self {
            s: s.to_string(),
            line,
            column,
            fd,
        }
    }
}

enum TokenizerState {
    Initial,
    BuildingKeyWord,
    BuildingConstant(bool),
    BuildingString,
    BuildingSection,
    StrEsc,
    Comment,
}
pub fn newline(line: &mut usize, column: &mut usize, caller: &str) {
    *line += 1;
    *column = 0;
    print!("{caller}\t: NEW LINE {line}");

    // print!("{line}: ");
    // stdout().flush();
}
pub fn tokenize_file(fd: usize, file_content: &str) -> Result<Vec<Token>, AssembleError> {
    // println!("---");

    let mut tokens: Vec<Token> = Vec::new();
    let mut state = TokenizerState::Initial;
    let mut column = 0;
    let mut line = 0;
    newline(&mut line, &mut column, "init");
    let mut token_column_start = 0;
    let mut lexeme_buffer = String::new();
    let mut char_iter = file_content.chars();
    let mut old_char = 0 as char;
    let mut advance = true;
    // print!("{line}: ");
    // stdout().flush();
    //
    let mut debug_print_char_flag = false;
    loop {
        let chr = if advance {
            match char_iter.next() {
                Some(c) => c,
                None => break,
            }
        } else {
            advance = true;
            old_char
        };
        old_char = chr;
        if chr == '\n' {
            println!(
                "chr is newline, length of iter() {}",
                char_iter.clone().collect::<Vec<char>>().len()
            );
        }
        // print!("New Char");
        if debug_print_char_flag {
            print!("[{}]", chr.escape_debug());
            stdout().flush();
        }
        // }
        // for chr in file.chars() {
        column += 1;

        match state {
            TokenizerState::Initial => {
                token_column_start = column;
                match chr {
                    '#' => state = TokenizerState::Comment,
                    ' ' | '\t' => continue,
                    '\n' => {
                        debug_print_char_flag = false;
                        newline(&mut line, &mut column, "initial-empty-line");
                        continue;
                    }
                    ':' => state = TokenizerState::BuildingSection,
                    '(' => tokens.push(Token::OpenParen(Lexeme::new(
                        "(",
                        line,
                        token_column_start,
                        fd,
                    ))),
                    ')' => tokens.push(Token::ClosedParen(Lexeme::new(
                        ")",
                        line,
                        token_column_start,
                        fd,
                    ))),
                    '"' => state = TokenizerState::BuildingString,
                    '[' => tokens.push(Token::OpenBracket(Lexeme::new(
                        "[",
                        line,
                        token_column_start,
                        fd,
                    ))),
                    ']' => tokens.push(Token::ClosedBracket(Lexeme::new(
                        "]",
                        line,
                        token_column_start,
                        fd,
                    ))),
                    '*' => tokens.push(Token::Star(Lexeme::new("*", line, token_column_start, fd))),
                    '+' => tokens.push(Token::Plus(Lexeme::new("+", line, token_column_start, fd))),
                    '-' => tokens.push(Token::Dash(Lexeme::new("-", line, token_column_start, fd))),
                    '/' => {
                        tokens.push(Token::Slash(Lexeme::new("/", line, token_column_start, fd)))
                    }

                    ';' => tokens.push(Token::SemiColon(Lexeme::new(
                        ";",
                        line,
                        token_column_start,
                        fd,
                    ))),
                    ',' => {
                        tokens.push(Token::Comma(Lexeme::new(",", line, token_column_start, fd)))
                    }
                    '.' => tokens.push(Token::Dot(Lexeme::new(".", line, token_column_start, fd))),
                    '%' => tokens.push(Token::Dot(Lexeme::new("%", line, token_column_start, fd))),
                    '0'..='9' => {
                        return Err(AssembleError::new(format!("invalid word"))
                            .attach_lexeme(&Lexeme::new(&lexeme_buffer, line, column, fd)))
                    }
                    '@' | '$' => {
                        let offset = match chr {
                            '@' => true,
                            '$' => false,
                            _ => unreachable!(),
                        };
                        state = TokenizerState::BuildingConstant(offset);
                    }
                    _ => {
                        lexeme_buffer.push(chr);
                        state = TokenizerState::BuildingKeyWord;
                    }
                }
            }
            TokenizerState::Comment => match chr {
                '\n' => {
                    newline(&mut line, &mut column, "comment-end");
                    state = TokenizerState::Initial;
                    lexeme_buffer.clear();
                }
                _ => continue,
            },
            TokenizerState::BuildingSection => match chr {
                '\n' | ' ' => {
                    state = TokenizerState::Initial;
                    tokens.push(Token::EOS(Lexeme::new("End Of Section", line, column, fd)));
                    let lexeme = Lexeme::new(&lexeme_buffer, line, column, fd);
                    match lexeme_buffer.as_str() {
                        "data" => tokens.push(Token::DATA(lexeme)),
                        "entry" => tokens.push(Token::ENTRYPOINT(lexeme)),
                        "program" => tokens.push(Token::PROGRAM(lexeme)),
                        "include" => tokens.push(Token::INCLUDE(lexeme)),
                        _ => {
                            println!("{tokens:?}");
                            return Err(AssembleError::new(format!(
                                "invalid section header `{lexeme_buffer}`"
                            ))
                            .attach_lexeme(&lexeme));
                        }
                    }
                    if chr == '\n' {
                        newline(&mut line, &mut column, "section-build-unreachable?");
                    }
                    lexeme_buffer.clear();
                }
                _ => lexeme_buffer.push(chr),
            },
            // keywords are opcode mnemonics and registers
            TokenizerState::BuildingKeyWord => match chr {
                'a'..='z' | 'A'..='Z' | '_' | '!' | '0'..='9' => lexeme_buffer.push(chr),
                '#' => state = TokenizerState::Comment,
                _ => {
                    tokens.push(Token::KeyWord(Lexeme::new(
                        &lexeme_buffer,
                        line,
                        token_column_start,
                        fd,
                    )));
                    lexeme_buffer.clear();
                    if chr == '\n' {
                        newline(&mut line, &mut column, "keyword-end-in-\\n");
                    }
                    state = TokenizerState::Initial;
                }
            },
            // any value
            TokenizerState::BuildingConstant(offset) => match chr {
                'a'..='z' | 'A'..='Z' | '_' | '!' | '0'..='9' => lexeme_buffer.push(chr),
                '#' => state = TokenizerState::Comment,
                _ => {
                    println!("built new constant {lexeme_buffer} at {line}:{token_column_start}");
                    tokens.push(Token::Constant(
                        Lexeme::new(&lexeme_buffer, line, token_column_start, fd),
                        offset,
                    ));
                    if chr != '"' && chr != '\n' {
                        advance = false;
                    }
                    lexeme_buffer.clear();
                    if chr == '\n' {
                        // println!("New line skipped because appar");
                        newline(&mut line, &mut column, "constant-end-in-\\n");
                    };
                    debug_print_char_flag = true;
                    state = TokenizerState::Initial;
                }
            },
            TokenizerState::BuildingString => match chr {
                '"' => {
                    tokens.push(Token::String(Lexeme::new(
                        &lexeme_buffer,
                        line,
                        token_column_start,
                        fd,
                    )));
                    lexeme_buffer.clear();
                    state = TokenizerState::Initial
                }
                '\\' => state = TokenizerState::StrEsc,
                _ => lexeme_buffer.push(chr),
            },

            TokenizerState::StrEsc => {
                let esc_char = match chr {
                    '\\' => '\\',
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\'' => '\'',
                    '"' => '"',
                    '0' => '\0',
                    'b' => '\x08',
                    _ => {
                        return Err(AssembleError::new(format!(
                            "invalid escape sequence >>\"\\{chr}\"<< "
                        )))
                    }
                };
                lexeme_buffer.push(esc_char);
                state = TokenizerState::BuildingString;
            }
        }
    }
    tokens.push(Token::EOS(Lexeme::new("End Of Section", line, column, fd)));
    tokens.push(Token::EOF(Lexeme::new(
        "EOF SENTINEL",
        line.saturating_sub(1),
        token_column_start,
        fd,
    )));
    // println!("{tokens:#?}");
    Ok(tokens)
}
