use crate::tokenizer::{Tokenizer, TokenKind, Token};

struct Parser {
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(source_code: &str) -> Self {
        Self { tokenizer: Tokenizer::new(source_code.to_owned()) }
    }

    pub fn consume(&mut self, token_kind: TokenKind) -> Option<Token> {
        None
    }
}