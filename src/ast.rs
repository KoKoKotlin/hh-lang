use std::fmt::Display;

use crate::{tokenizer::Token, interpreter::MutRc};

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Block>);

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub struct Reassign {
    pub name_tok: Token,
    pub op_tok: Token,
    pub record_fields: Vec<Token>,
    pub assign_expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Reassign(Reassign),
    ListReassign(Token, Expr, Expr),
    ConstAssign(Vec<(Token, Literal)>),
    VarDecl(Vec<(Token, Option<Expr>)>),
    If(Expr, Block, Option<Block>),
    While(Expr, Block),
    FuncDecl(Token, Vec<Token>, Block),
    RecordDecl(Token, Vec<Token>),
    Return(Expr),
    Break(Token),
    Continue(Token),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Literal(MutRc<Literal>),
    Ident(Token),
    IdentIndexed(Token, Box<Expr>),
    FuncCall(Token, Vec<Expr>),
    BuiltInCall(Token, Vec<Expr>),
    ListInstantiation(Token, Box<Expr>, Vec<Expr>),
    RecordInstantiation(Token, Vec<Expr>),
    RecordFieldDeref(Token, Vec<Token>),
    LambdaInstantiation(Vec<Token>, Box<Statement>),
    LambdaCall(Token, Box<Expr>, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    List(Vec<MutRc<Literal>>),
    RecordInstance(Token, Vec<MutRc<Literal>>),
    LambdaInstance(Vec<Token>, Box<Statement>),
    Unit,
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::String(str1), Literal::String(str2)) => str1 == str2,
            (Literal::Int(num1), Literal::Int(num2)) => num1 == num2,
            (Literal::Float(num1), Literal::Float(num2)) => num1 == num2,
            (Literal::Bool(val1), Literal::Bool(val2)) => val1 == val2,
            (Literal::Char(c1), Literal::Char(c2)) => c1 == c2,
            (Literal::List(l1), Literal::List(l2)) => l1 == l2,
            (Literal::RecordInstance(_, _), Literal::RecordInstance(_, _)) => false,
            (Literal::LambdaInstance(_, _), Literal::LambdaInstance(_, _)) => false,
            (Literal::Unit, Literal::Unit) => true,
            _ => false,
        }
    }
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
            Literal::Int(num) => *num != 0,
            Literal::Float(num) => *num != 0.0,
            Literal::List(values) => values.len() != 0, 
            Literal::Bool(val) => *val,
            Literal::Char(c) => *c != '\0',
            Literal::RecordInstance(_, _) => true,
            Literal::LambdaInstance(_, _) => true,
            Literal::Unit => false,
        }
    }

    pub fn string(&self) -> String {
        match self {
            Literal::String(str) => str.clone(),
            Literal::Int(num) => num.to_string(),
            Literal::Float(num) => num.to_string(),
            Literal::Bool(val) => val.to_string(),
            Literal::Char(c) => c.to_string(),
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
            Literal::Int(_) => "Int",
            Literal::Float(_) => "Float",
            Literal::Bool(_) => "Bool",
            Literal::Char(_) => "Char",
            Literal::List(_) => "List",
            Literal::RecordInstance(_, _) => "RecordInstance",
            Literal::LambdaInstance(_, _) => "LambdaInstance",
            Literal::Unit => "Unit",
        }
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Literal::Bool(value)
    }
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Literal::Int(value)
    }
}


impl From<f64> for Literal {
    fn from(value: f64) -> Self {
        Literal::Float(value)
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Literal::String(value)
    }
}

impl From<char> for Literal {
    fn from(value: char) -> Self {
        Literal::Char(value)
    }
}