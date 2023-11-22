use std::{fmt::Debug, str::Chars};


#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident,
    String,
    Number,

    // single symbol tokens
    Equals,
    Plus,
    Minus,
    Times,
    Semicolon,
    OpeningBraket,
    ClosingBraket,
    Comma,
    LessThan,
    GreaterThan,
    Bang,

    // composite tokens
    LessOrEqual,
    GreaterOrEqual,
    NotEqual,
    EqualsEquals,

    // reserved keywords
    Func,
    Start,
    End,
    Or,
    And,
    Xor,
    If,
    While,
    Do,
    Then,
}

#[derive(Debug, Clone)]
pub struct Token {
    loc: usize,
    kind: TokenKind,
    symbols: String,
}

impl Token {
    pub fn new(loc: usize, kind: TokenKind, symbols: String) -> Self {
        Self {
            loc,
            kind,
            symbols
        }
    }
}

struct TokenizerRule {
    rule: Box<dyn Fn(Chars) -> (usize, Option<TokenKind>)>,
    name: &'static str,
}

impl Debug for TokenizerRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenizerRule").field("name", &self.name).finish()
    }
}

#[derive(Debug)]
pub struct Tokenizer {
    source_code: String,
    current_loc: usize,
    rules: Vec<TokenizerRule>,
}

fn whitespace(char_iter: Chars) -> (usize, Option<TokenKind>) {
    for (idx, char) in char_iter.enumerate() {
        if !char.is_whitespace() { return (idx, None); }
    }

    (0, None)
}

fn number(char_iter: Chars) -> (usize, Option<TokenKind>) {
    for (idx, char) in char_iter.enumerate() {
        if !char.is_numeric() {
            return (idx, Some(TokenKind::Number));
        }
    }
    
    (0, None)
}

fn string(char_iter: Chars) -> (usize, Option<TokenKind>) {
    for (idx, char) in char_iter.enumerate() {
        if idx == 0 && char != '"' { return (0, None); }
        if idx != 0 && char == '"' { return (idx + 1, Some(TokenKind::String)); }
    }

    (0, None)
}

fn is_operator_token(c: char) -> bool {
    c == ',' || c == '+' || c == '-' || c == '*' || c == '=' || c == '[' || c == ']' || c == ';' || c == '<' || c == '>' || c == '!'
}

fn parse_one_symbol_token(c: char) -> TokenKind {
    match c {
        ',' => TokenKind::Comma,
        ';' => TokenKind::Semicolon,
        '+' => TokenKind::Plus,
        '-' => TokenKind::Minus,
        '*' => TokenKind::Times,
        '=' => TokenKind::Equals,
        '[' => TokenKind::OpeningBraket,
        ']' => TokenKind::ClosingBraket,
        '<' => TokenKind::LessThan,
        '>' => TokenKind::GreaterThan,
        '!' => TokenKind::Bang,
        _ => unreachable!(),
    }
}

fn parse_composite(first_char: char, second_char: char) -> Option<TokenKind> {
    match (first_char, second_char) {
        ('!', '=') => Some(TokenKind::NotEqual),
        ('<', '=') => Some(TokenKind::LessOrEqual),
        ('>', '=') => Some(TokenKind::GreaterOrEqual),
        ('=', '=') => Some(TokenKind::EqualsEquals),
        _ => None,
    }
}

fn operator(mut char_iter: Chars) -> (usize, Option<TokenKind>) {
    let first = char_iter.next();
    let second = char_iter.next();

    let first = match first {
        None => return (0, None),
        Some(c) => {
            if !is_operator_token(c) { return (0, None); }
            c
        }
    };

    match second {
        None => return (1, Some(parse_one_symbol_token(first))),
        Some(second) => {
            if is_operator_token(second) { return (2, parse_composite(first, second)); } 
            else { return (1, Some(parse_one_symbol_token(first))); }
        }
    }
}

pub fn parse_ident(slice: &str) -> TokenKind {
    match slice {
        "func" => TokenKind::Func,
        "start" => TokenKind::Start,
        "end" => TokenKind::End,
        "or" => TokenKind::Or,
        "and" => TokenKind::And,
        "xor" => TokenKind::Xor,
        "if" => TokenKind::If,
        "while" => TokenKind::While,
        "do" => TokenKind::Do,
        "then" => TokenKind::Then,
        _ => TokenKind::Ident,
    }
}

fn ident(char_iter: Chars) -> (usize, Option<TokenKind>) {
    let mut buf = String::new();

    for (idx, char) in char_iter.enumerate() {
        if idx == 0 && !char.is_alphabetic() { return (0, None); }
        else if !char.is_alphanumeric() { 
            break;
        }
        buf.push(char);
    }

    (buf.len(), Some(parse_ident(&buf)))
}

impl Tokenizer {
    pub fn new(source_code: String) -> Self {
        Self {
            source_code,
            current_loc: 0,
            rules: vec![
                TokenizerRule { name: "Whitespace", rule: Box::new(whitespace) },
                TokenizerRule { name: "Operator",   rule: Box::new(operator) },
                TokenizerRule { name: "Number",     rule: Box::new(number) },
                TokenizerRule { name: "String",     rule: Box::new(string) },
                TokenizerRule { name: "Identifier", rule: Box::new(ident) },
            ],
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let current_slice = &self.source_code[self.current_loc..];
        let char_iterator = current_slice.chars();

        for rule in self.rules.iter() {
            let (len, kind) = (rule.rule)(char_iterator.clone());
            
            // println!("{:?} {:?}", len, kind);
            // println!("{}", current_slice);

            // whitespace
            if len != 0 && kind.is_none() {
                self.current_loc += len;
                return self.next_token();
            }

            if len != 0 {
                if let Some(kind) = kind {
                    let token = Token::new(
                        self.current_loc, 
                        kind, 
                        self.source_code[self.current_loc..self.current_loc+len].to_owned());                
                    self.current_loc += len;
                    return Some(token);
                }
            }
        }
        
        None
    }
}