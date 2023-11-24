use std::{fmt::{Debug, Display}, str::Chars};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    String,
    Number,

    // single symbol tokens
    Equals,
    Plus,
    Minus,
    Times,
    Div,
    Semicolon,
    OpeningBracket,
    ClosingBracket,
    OpeningParan,    
    ClosingParan,
    Comma,
    LessThan,
    GreaterThan,
    Bang,
    Dot,

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
    Else,
    While,
    Do,
    Then,
    True,
    False,
    Let,
    Var,
    Call,
    Record,
    New,
    Return,

    // built-in functions
    Print,
    Println,
    Dbg,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;

        write!(f, "{}", match *self {
            Ident => "Ident",
            Number => "Number",
            String => "String",
            Equals => "=",
            Plus => "+",
            Minus => "-",
            Times => "*",
            Div => "/",
            Semicolon => ";",
            OpeningBracket => "[",
            ClosingBracket => "]",
            OpeningParan => "(",
            ClosingParan => ")",
            Comma => ",",
            LessThan => "<",
            GreaterThan => ">",
            Bang => "!",
            Dot => ".",
            LessOrEqual => "<=",
            GreaterOrEqual => ">=",
            NotEqual => "!=",
            EqualsEquals => "==",
            Func => "func",
            Start => "start",
            End => "end",
            Or => "or",
            And => "and",
            Xor => "xor",
            If => "if",
            Else => "else",
            While => "while",
            Do => "do",
            Then => "then",
            True => "true",
            False => "false",
            Let => "let",
            Var => "var",
            Print => "Built-In Print",
            Println => "Built-In Pritnln",
            Call => "call",
            Record => "record",
            New => "new",
            Dbg => "Built-In Dbg",
            Return => "return",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    row: usize,
    col: usize,
    // TODO: filename
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub loc: Location,
    pub index: usize,
    pub kind: TokenKind,
    pub symbols: String,
}

impl Token {
    pub fn new(loc: Location, index: usize, kind: TokenKind, symbols: String) -> Self {
        Self {
            loc,
            index,
            kind,
            symbols
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Ident | TokenKind::String | TokenKind::Number => write!(f, "{}({})", self.kind, self.symbols),
            _ => write!(f, "{}", self.kind)
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
    current_pointer: usize,
    current_loc: Location,
    rules: Vec<TokenizerRule>,
}

fn whitespace(char_iter: Chars) -> (usize, Option<TokenKind>) {
    for (idx, char) in char_iter.enumerate() {
        if !char.is_whitespace() { return (idx, None); }
    }

    (0, None)
}

fn comment(char_iter: Chars) -> (usize, Option<TokenKind>) {
    for (idx, char) in char_iter.enumerate() {
        if idx == 0 && char != '#' { break; }
        else if idx != 0 && char == '\n' { return (idx, None) }; 
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
    c == ',' || c == '+' || c == '-' || c == '*' || c == '/' || c == '=' 
 || c == '[' || c == ']' || c == ';' || c == '<' || c == '>' || c == '!'
 || c == '(' || c == ')' || c == '.'
}

fn parse_one_symbol_token(c: char) -> TokenKind {
    match c {
        ',' => TokenKind::Comma,
        ';' => TokenKind::Semicolon,
        '+' => TokenKind::Plus,
        '-' => TokenKind::Minus,
        '*' => TokenKind::Times,
        '/' => TokenKind::Div,
        '=' => TokenKind::Equals,
        '[' => TokenKind::OpeningBracket,
        ']' => TokenKind::ClosingBracket,
        '(' => TokenKind::OpeningParan,
        ')' => TokenKind::ClosingParan,
        '<' => TokenKind::LessThan,
        '>' => TokenKind::GreaterThan,
        '!' => TokenKind::Bang,
        '.' => TokenKind::Dot,
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
            if let Some(composite) = parse_composite(first, second) { 
                return (2, Some(composite)); 
            } else {
                return (1, Some(parse_one_symbol_token(first))); 
            }
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
        "else" => TokenKind::Else,
        "while" => TokenKind::While,
        "do" => TokenKind::Do,
        "then" => TokenKind::Then,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "let" => TokenKind::Let,
        "var" => TokenKind::Var,
        "print" => TokenKind::Print,
        "println" => TokenKind::Println,
        "call" => TokenKind::Call,
        "record" => TokenKind::Record,
        "new" => TokenKind::New,
        "dbg" => TokenKind::Dbg,
        "return" => TokenKind::Return,
        _ => TokenKind::Ident,
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' 
}

fn ident(char_iter: Chars) -> (usize, Option<TokenKind>) {
    let mut buf = String::new();

    for (idx, char) in char_iter.enumerate() {
        if idx == 0 && !char.is_alphabetic() { return (0, None); }
        else if !is_ident_char(char) { 
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
            current_pointer: 0,
            current_loc: Location { row: 1, col: 1 },
            rules: vec![
                TokenizerRule { name: "Whitespace", rule: Box::new(whitespace) },
                TokenizerRule { name: "Comment",    rule: Box::new(comment) },
                TokenizerRule { name: "Operator",   rule: Box::new(operator) },
                TokenizerRule { name: "Number",     rule: Box::new(number) },
                TokenizerRule { name: "String",     rule: Box::new(string) },
                TokenizerRule { name: "Identifier", rule: Box::new(ident) },
            ],
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let current_slice = &self.source_code[self.current_pointer..];
        let char_iterator = current_slice.chars();

        for rule in self.rules.iter() {
            let (len, kind) = (rule.rule)(char_iterator.clone());
            
            // skip whitespace and comments
            if len != 0 && kind.is_none() {
                let mut whitespace_slice = &current_slice[0..len];

                let mut reset_col = false;
                while let Some(idx) = whitespace_slice.find("\n") {
                    self.current_loc.row += 1;
                    whitespace_slice = &whitespace_slice.split_at(idx+1).1;
                    reset_col = true;
                }
                
                if reset_col {
                    self.current_loc.col = whitespace_slice.len() + 1;
                } else {
                    self.current_loc.col += whitespace_slice.len();
                }

                self.current_pointer += len;
                return self.next_token();
            }

            if len != 0 {
                if let Some(kind) = kind {
                    let (start, end) = if kind == TokenKind::String {
                        (self.current_pointer+1, self.current_pointer+len-1) 
                    } else {
                        (self.current_pointer, self.current_pointer+len)
                    };

                    let token = Token::new(
                        self.current_loc.clone(), 
                        self.current_pointer,
                        kind, 
                        self.source_code[start..end].to_owned());                
                    self.current_pointer += len;
                    self.current_loc.col += len;
                    return Some(token);
                }
            }
        }
        
        None
    }

    pub fn get_context(&self, idx: usize) -> String {
        let slice = &self.source_code[idx..];
        String::from(if slice.len() > 50 {
            &slice[0..50]
        } else {
            &slice[..]
        })
    }
}