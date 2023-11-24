use std::fmt::Display;

use crate::{tokenizer::Token, interpreter::MutRc};

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Block>);

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Reassign(Token, Vec<Token>, Expr),
    ListReassign(Token, Expr, Expr),
    ConstAssign(Vec<(Token, Literal)>),
    VarDecl(Vec<(Token, Option<Literal>)>),
    If(Expr, Block, Option<Block>),
    While(Expr, Block),
    FuncDecl(Token, Vec<Token>, Block),
    RecordDecl(Token, Vec<Token>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    // Unary(Unary),
    Literal(MutRc<Literal>),
    Ident(Token),
    IdentIndexed(Token, Box<Expr>),
    FuncCall(Token, Vec<Expr>),
    BuiltInCall(Token, Vec<Expr>),
    ListInstantiation(Token, Box<Expr>, Vec<Expr>),
    RecordInstantiation(Token, Vec<Expr>),
    RecordFieldDeref(Token, Vec<Token>),
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Number(i64),
    True,
    False,
    List(Vec<MutRc<Literal>>),
    RecordInstance(Token, Vec<MutRc<Literal>>),
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
            Literal::RecordInstance(_, _) => true,
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
            Literal::List(values) => {
                let mut buf = String::new();
                buf.push_str("[ ");
                for lit in values {
                    buf.push_str(&format!("{} ", lit.borrow()));
                }
                buf.push_str("]");
                buf
            }
            Literal::RecordInstance(record_name, field_values) => {
                let mut buf = String::new();
                buf.push_str(&record_name.symbols);
                buf.push_str("{ ");
                for lit in field_values {
                    buf.push_str(&format!("{} ", lit.borrow()));
                }
                buf.push_str("}");
                buf
            },
            _ => format!("{:?}", self),
        }
    }

    pub fn get_type(&self) -> &'static str {
        match self {
            Literal::String(_) => "String",
            Literal::Number(_) => "Number",
            Literal::True => "Bool",
            Literal::False => "Bool",
            Literal::List(_) => "List",
            Literal::RecordInstance(_, _) => "RecordInstance",
            Literal::Unit => "Unit",
        }
    }
}