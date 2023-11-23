use std::fmt::Display;

use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Block>);

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Reassign(Token, Expr),
    ListReassign(Token, Expr, Expr),
    ConstAssign(Vec<(Token, Literal)>),
    VarDecl(Vec<(Token, Option<Literal>)>),
    ListDecl(Token, Expr),
    If(Expr, Block, Option<Block>),
    While(Expr, Block),
    BuiltIn(Token, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    // Unary(Unary),
    Literal(Literal),
    Ident(Token),
    IdentIndexed(Token, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Number(i64),
    List(Vec<Box<Literal>>),
    True,
    False,
    Unit,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string())
    }
}

impl Literal {
    pub fn is_truthy(&self) -> bool {
        match self {
            Literal::String(str) => !str.is_empty(),
            Literal::Number(num) => *num != 0,
            Literal::List(values) => values.len() != 0, 
            Literal::True => true,
            Literal::False => false,
            Literal::Unit => false,
        }
    }

    pub fn string(&self) -> String {
        match self {
            Literal::String(str) => str.clone(),
            Literal::Number(num) => num.to_string(),
            Literal::True => "true".to_string(),
            Literal::False => "false".to_string(),
            Literal::Unit => "()".to_string(),
            _ => format!("{:?}", self),
        }
    }
}