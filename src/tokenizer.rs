
#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(String),
    String(String),
    Number(String),

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
}

impl Token {
    pub fn new(loc: usize, kind: TokenKind) -> Self {
        Self {
            loc,
            kind
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tokenizer {
    source_code: String,
    current_loc: usize,
}

#[derive(Debug, Clone)]
pub enum TokenizerError {}

impl Tokenizer {
    pub fn new(source_code: String) -> Self {
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
                        let string_token = Token::new(self.current_loc, TokenKind::String(string_slice.to_owned()));
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
                if Self::is_white_space(c) {
                    self.current_loc += 1;
                    continue;
                }
                
                // one symbol tokens
                if Self::is_special_token(c) {
                    // check for composites like <=, >=, !=
                    if Self::can_be_composite(c) {
                        if let Some(next) = char_iterator.peek() {
                            if let Some(token_kind) = Self::check_composite(c, *next) {
                                let token = Token::new(self.current_loc, token_kind);
                                self.current_loc += 2;
                                return Ok(Some(token));
                            }
                        }
                    }

                    let kind = Self::parse_one_symbol_token(c);
                    let token = Token::new(self.current_loc, kind);
                    self.current_loc += 1;
                    return Ok(Some(token));
                } else {
                    let finish_token = if let Some(next) = char_iterator.peek() {
                        // look if the next token is whitespace or a special token
                        Self::is_special_token(*next) || Self::is_white_space(*next)
                    } else {
                        // finish the token because end of input
                        true
                    };

                    if finish_token {
                        let current_slice = &self.source_code[self.current_loc..=self.current_loc+current_len];
                        
                        // check for number or string
                        let result: Token = if Self::is_number(current_slice) {
                            Token::new(self.current_loc, TokenKind::Number(current_slice.to_owned()))
                        } else {
                            // if not number and not string => check for reserved and if not reserved => ident
                            Token::new(self.current_loc, Self::parse_ident(current_slice))
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

    fn can_be_composite(c: char) -> bool {
        c == '!' || c == '<' || c == '>' || c == '='
    }

    fn check_composite(first_char: char, second_char: char) -> Option<TokenKind> {
        match (first_char, second_char) {
            ('!', '=') => Some(TokenKind::NotEqual),
            ('<', '=') => Some(TokenKind::LessOrEqual),
            ('>', '=') => Some(TokenKind::GreaterOrEqual),
            ('=', '=') => Some(TokenKind::EqualsEquals),
            _ => None,
        }
    }

    fn is_special_token(c: char) -> bool {
        c == ',' || c == '+' || c == '-' || c == '*' || c == '=' || c == '[' || c == ']' || c == ';' || c == '<' || c == '>' || c == '|'
    }

    fn is_white_space(c: char) -> bool {
        c == ' ' || c == '\t' || c == '\n'
    }

    fn is_number(token: &str) -> bool {
        token.parse::<u32>().is_ok()
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
            _ => TokenKind::Ident(slice.to_owned()),
        }
    }

}

pub fn tokenize<'a>(source_code: &'a str) -> Result<Vec<Token>, TokenizerError> {
    let mut tokenizer = Tokenizer::new(source_code.to_owned());
    let mut tokens: Vec<Token> = Vec::new();
    loop {
        if let Some(token) = tokenizer.next_token()? {
            tokens.push(token);
        } else {
            break;
        }
    }

    return Ok(tokens);
}