use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Block>);

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Reassign(Token, Expr),
    ConstAssign(Vec<(Token, Literal)>),
    VarDecl(Vec<(Token, Option<Literal>)>),
    If(Expr, Block, Option<Block>),
    While(Expr, Block),
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

impl Literal {
    pub fn is_truthy(&self) -> bool {
        match self {
            Literal::String(str) => !str.is_empty(),
            Literal::Number(num) => *num != 0,
            Literal::True => true,
            Literal::False => false,
            Literal::Unit => false,
        }
    }
}