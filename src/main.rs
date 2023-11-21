use std::{fs::File, io::Read};

const SOURCE_FILE_PATH: &'static str = "main.hh";

fn load_source_code_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf)
}

#[derive(Debug, Clone)]
enum TokenKind<'a> {
    Ident(&'a str),
    String(&'a str),
    Number(&'a str),
    Equals,
    Plus,
    Minus,
    Times,
    Semicolon,
    OpeningBraket,
    ClosingBraket,
    Comma,
}

#[derive(Debug, Clone)]
struct Token<'a> {
    loc: usize,
    kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(loc: usize, kind: TokenKind<'a>) -> Self {
        Self {
            loc,
            kind
        }
    }
}

#[derive(Debug, Clone)]
struct Tokenizer<'a> {
    source_code: &'a str,
    current_loc: usize,
}

#[derive(Debug, Clone)]
enum TokenizerError {
    
}

impl<'a> Tokenizer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            source_code,
            current_loc: 0
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, TokenizerError> {
        let current_slice = &self.source_code[self.current_loc..];
        let mut char_iterator = current_slice.chars().peekable();
        let mut current_len = 0;
        let mut in_string_context = false;

        loop {
            if let Some(c) = char_iterator.next() {
                // parse strings
                if in_string_context {
                    // end string context and finalize string
                    if c == '\"' {
                        let string_slice = &self.source_code[self.current_loc..=self.current_loc+current_len];
                        let string_token = Token::new(self.current_loc, TokenKind::String(string_slice));
                        self.current_loc += current_len + 1;
                        return Ok(Some(string_token));
                    } else {
                        current_len += 1;
                        continue;
                    }
                }

                if c == '\"' && !in_string_context {
                    in_string_context = true;
                    current_len += 1;
                    continue;
                }

                // skip whitespace
                if self.is_white_space(c) {
                    self.current_loc += 1;
                    continue;
                }
                
                // one symbol tokens
                if self.is_special_token(c) {
                    self.current_loc += 1;
                    let kind = self.parse_one_symbol_token(c);
                    let token = Token::new(self.current_loc - 1, kind);
                    return Ok(Some(token));
                } else {
                    let finish_token = if let Some(next) = char_iterator.peek() {
                        // look if the next token is whitespace or a special token
                        self.is_special_token(*next) || self.is_white_space(*next)
                    } else {
                        // finish the token because end of input
                        true
                    };

                    if finish_token {
                        let current_slice = &self.source_code[self.current_loc..=self.current_loc+current_len];
                        
                        // check for number or string
                        let result: Token<'_> = if self.is_number(current_slice) {
                            Token::new(self.current_loc, TokenKind::Number(current_slice))
                        } else if self.is_string(current_slice) {
                            Token::new(self.current_loc, TokenKind::String(current_slice))
                        } else {
                            // if not number and not string => ident
                            Token::new(self.current_loc, TokenKind::Ident(current_slice))
                        };

                        self.current_loc += current_len + 1;

                        return Ok(Some(result));
                    } else {
                        // continue the search at the next symbol
                        current_len += 1;
                    }
                }
            } else {
                // end of input
                return Ok(None);
            }
        }
    }

    fn is_special_token(&self, c: char) -> bool {
        c == ',' || c == '+' || c == '-' || c == '*' || c == '=' || c == '[' || c == ']' || c == ';'
    }

    fn is_white_space(&self, c: char) -> bool {
        c == ' ' || c == '\t' || c == '\n'
    }

    fn is_number(&self, token: &str) -> bool {
        token.parse::<u32>().is_ok()
    }

    fn is_string(&self, token: &str) -> bool {
        token.starts_with("\"") && token.ends_with("\"") && !token[1..token.len()-1].contains("\"") && token.len() >= 2
    }

    fn parse_one_symbol_token(&self, c: char) -> TokenKind {
        match c {
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Times,
            '=' => TokenKind::Equals,
            '[' => TokenKind::OpeningBraket,
            ']' => TokenKind::ClosingBraket,
            _ => unreachable!(),
        }
    }

}

fn tokenize(source_code: &str) {
    let mut tokenizer = Tokenizer::new(source_code);
    while let Ok(Some(token)) = tokenizer.next_token() {
        println!("{:?}", token);
    }
}

fn main() {
    let source_code = load_source_code_file(SOURCE_FILE_PATH).unwrap();
    println!("{source_code}");
    tokenize(&source_code);
}
