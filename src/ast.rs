use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Block>);

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Reassign(Token, Expr),
    ConstAssign(Token, Literal),
    VarDecl(Token, Option<Literal>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    // Unary(Unary),
    Literal(Literal),
    Ident(Token),
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Number(u32),
    True,
    False,
    Unit,
}