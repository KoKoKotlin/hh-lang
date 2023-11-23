use std::env::var;

use crate::{ast::{Program, Literal, Block, Statement, Expr}, tokenizer::{Token, TokenKind}};

#[derive(Debug)]
enum Name {
    Const { name: String, value: Literal },
    Var { name: String, value: Option<Literal> },
    List { name: String, value: Vec<Literal> },
}

impl Name {
    fn is_var(&self) -> bool {
        match self {
            Self::Const { name: _, value: _ } => false,
            Self::Var { name: _, value: _ } => true,
            Self::List { name: _, value: _ } => false,
        }
    }

    fn is_const(&self) -> bool {
        match self {
            Self::Const { name: _, value: _ } => true,
            Self::Var { name: _, value: _ } => false,
            Self::List { name: _, value: _ } => false,
        }
    }

    fn is_list(&self) -> bool {
        match self {
            Self::Const { name: _, value: _ } => false,
            Self::Var { name: _, value: _ } => false,
            Self::List { name: _, value: _ } => true,
        }
    }

    fn reassign_var(&mut self, value: &Literal) -> InterpreterResult {
        match self {
            Self::Const { name: _, value: _ } => Err(InterpreterError::ConstantReassign),
            Self::Var { name: _, value: ref mut v } => {
                *v = Some(value.clone());
                Ok(())
            },
            Self::List { name: _, value: _ } => Err(InterpreterError::ListReassign),
        }
    }

    fn make_const(name: &str, value: Literal) -> Self {
        Self::Const { name: name.to_string(), value }
    }

    fn make_var(name: &str, value: Option<Literal>) -> Self {
        Self::Var { name: name.to_string(), value }
    }

    fn make_list(name: &str, capacity: usize) -> Self {
        let mut value = Vec::new();
        for _ in 0..capacity {
            value.push(Literal::Number(0));
        }

        Self::List { name: name.to_string(), value }
    }
}

#[derive(Debug)]
pub struct InterpreterContext {
    names: Vec<Name>,
}

#[derive(Debug)]
pub enum InterpreterError {
    VariableDoesNotExists,
    ConstantReassign,
    ListReassign,
    VariableNotInitialized,
    UndefinedBinOperation(Literal, Token, Literal),
    DivisionByZero,
    TypeError(String),
    ValueError(String),
    IndexOutOfBounds,
}

pub type InterpreterResult = Result<(), InterpreterError>;

impl InterpreterContext {
    pub fn new() -> Self {
        Self { names: vec![] }
    }

    fn get_name(&mut self, name: &str) -> Option<&mut Name> {
        for var in self.names.iter_mut() {
            let found = match var {
                Name::Const { name: n, value: _ } => n == name,
                Name::Var { name: n, value: _ } => n == name,
                Name::List { name: n, value: _ } => n == name,
            };
            if found { return Some(var); }
        }

        None
    }

    fn name_exists(&self, name: &str) -> bool {
        self.names.iter().any(|var| match var {
            Name::Const { name: n, value: _ } => n == name,
            Name::Var { name: n, value: _ } => n == name,
            Name::List { name: n, value: _ } => n == name,
        })
    }

    fn get_var_value(&self, name: &str, idx: Option<&Expr>) -> Result<Literal, InterpreterError> {
        for var in self.names.iter() {
            let maybe_name = match var {
                Name::Const { name: n, value: _ } => if n == name { Some(var) } else { None },
                Name::Var { name: n, value: _ } => if n == name { Some(var) } else { None },
                Name::List { name: n, value: _ } => if n == name { Some(var) } else { None },
            };
            
            if let Some(name) = maybe_name {
                match name {
                    Name::Const { name: _, value: v } => return Ok(v.clone()),
                    Name::Var { name: _, value: v } => {
                        if let Some(v) = v { return Ok(v.clone()) } else { return Err(InterpreterError::VariableNotInitialized) }  
                    },
                    Name::List { name: _, value: v } => {
                        if let Some(idx) = idx {
                            let idx = self.eval(idx)?;
                            if let Literal::Number(idx) = idx {
                                return Ok(v[idx as usize].clone());
                            } else {
                                return Err(InterpreterError::TypeError(format!("Indices must be Number, got {:?}", idx)));
                            }
                        }
                    },
                }
            }
        }

        Err(InterpreterError::VariableDoesNotExists)
    }

    fn eval(&self, expr: &Expr) -> Result<Literal, InterpreterError> {
        match expr {
            Expr::Binary(left, op, right) => {
                let left = self.eval(&left)?;
                let right = self.eval(&right)?;
                apply_op(&left, op, &right)
            },
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Ident(tok) => {
                self.get_var_value(&tok.symbols, None)
            },
            Expr::IdentIndexed(tok, idx_expr) => self.get_var_value(&tok.symbols, Some(idx_expr)),
        }
    }
}

fn apply_op(left: &Literal, op: &Token, right: &Literal) -> Result<Literal, InterpreterError> {
    use TokenKind::*;

    return match op.kind {
        Plus => {
            match (left, right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left + right)),
                (Literal::String(left), Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Minus => {
            match (left, right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left - right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Times => {
            match (left, right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left * right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Div => {
            match (left, right) {
                (Literal::Number(left), Literal::Number(right)) if *right != 0 => Ok(Literal::Number(left / right)),
                (Literal::Number(_), Literal::Number(right)) if *right == 0 => Err(InterpreterError::DivisionByZero),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        _ => unimplemented!(),
    }
}

fn exec_reassign(context: &mut InterpreterContext, tok: &Token, expr: &Expr) -> InterpreterResult {
    if !context.name_exists(&tok.symbols) {
        return Err(InterpreterError::VariableDoesNotExists);
    }

    let result = context.eval(expr);
    let name = context.get_name(&tok.symbols).unwrap();

    if name.is_const() {
        return Err(InterpreterError::ConstantReassign);
    } else if name.is_list() {
        return Err(InterpreterError::ListReassign);
    }

    name.reassign_var(&result?)?;

    Ok(())
}

fn exec_list_reassign(context: &mut InterpreterContext, tok: &Token, idx_expr: &Expr, val_expr: &Expr) -> InterpreterResult {
    if !context.name_exists(&tok.symbols) {
        return Err(InterpreterError::VariableDoesNotExists);
    }

    let result = context.eval(val_expr)?;
    let idx = context.eval(idx_expr)?;

    if let Literal::Number(idx) = idx {
        let name = context.get_name(&tok.symbols).unwrap();
        if name.is_const() || name.is_var() {
            return Err(InterpreterError::ValueError("Can't index into a non-list".to_string()));
        }

        if let Name::List { name: _, value: v } = name {
            println!("{idx}");
            if idx < 0 || v.len() <= idx as usize {
                return Err(InterpreterError::IndexOutOfBounds);
            }
            v[idx as usize] = result;
        }
    } else {
        return Err(InterpreterError::TypeError(format!("Indices must be Number, got {:?}", idx)));
    }

    Ok(())
}

fn exec_const_assign(context: &mut InterpreterContext, assigns: &Vec<(Token, Literal)>) -> InterpreterResult {
    for (tok, lit) in assigns {
        let var = Name::make_const(&tok.symbols, lit.clone());
        context.names.push(var);
    }

    Ok(())
}

fn exec_var_decl(context: &mut InterpreterContext, decls: &Vec<(Token, Option<Literal>)>) -> InterpreterResult {
    for (tok, lit) in decls {
        let var = Name::make_var(&tok.symbols, lit.clone());
        context.names.push(var);
    }

    Ok(())
}

fn exec_list_decl(context: &mut InterpreterContext, tok: &Token, expr: &Expr) -> InterpreterResult {
    let capacity = match context.eval(expr)? {
        Literal::String(_) => return Err(InterpreterError::TypeError("String cannot be used as a list".to_string())),
        Literal::Number(num) => {
            if num < 0 {
                return Err(InterpreterError::ValueError("Number cannot be negative".to_string()));
            }

            num as usize
        },
        Literal::True => return Err(InterpreterError::TypeError("Bool cannot be used as a list".to_string())),
        Literal::False => return Err(InterpreterError::TypeError("Bool cannot be used as a list".to_string())),
        Literal::Unit => return Err(InterpreterError::TypeError("Unit cannot be used as a list".to_string())),
    };

    let list = Name::make_list(&tok.symbols, capacity);
    context.names.push(list);
    Ok(())
}

fn exec_if(context: &mut InterpreterContext, cond: &Expr, if_block: &Block, else_block: Option<&Block>) -> InterpreterResult {
    let cond = context.eval(cond)?;

    if cond.is_truthy() {
        exec_block(context, if_block)?;
    } else if let Some(else_block) = else_block {
        exec_block(context, else_block)?;
    }

    Ok(())
}

fn exec_while(context: &mut InterpreterContext, cond_expr: &Expr, block: &Block) -> InterpreterResult {

    let mut cond = context.eval(cond_expr)?;
    while cond.is_truthy() {
        exec_block(context, block)?;
        cond = context.eval(cond_expr)?;
    }

    Ok(())
}

fn exec_built_in(context: &mut InterpreterContext, tok: &Token, args: &Vec<Expr>) -> InterpreterResult {
    use TokenKind::*;

    match tok.kind {
        Print => {
            for arg in args {
                let arg = context.eval(arg)?;
                print!("{} ", arg);
            }
            println!();
        },
        _ => unimplemented!("BuiltIn {:?} not implemented yet", tok.kind),
    }

    Ok(())
}

fn exec_block(context: &mut InterpreterContext, block: &Block) -> InterpreterResult {
    for statment in block.0.iter() {
        match statment {
            Statement::Reassign(tok, expr) => exec_reassign(context, tok, expr)?,
            Statement::ListReassign(tok, idx_expr, val_expr) => exec_list_reassign(context, tok, idx_expr, val_expr)?,
            Statement::ConstAssign(assigns) => exec_const_assign(context, assigns)?,
            Statement::VarDecl(decls) => exec_var_decl(context, decls)?,
            Statement::ListDecl(tok, expr) => exec_list_decl(context, tok, expr)?,
            Statement::If(cond, if_block, else_block) => exec_if(context, cond, if_block, else_block.as_ref())?,
            Statement::While(cond, block) => exec_while(context, cond, block)?,
            Statement::BuiltIn(tok, args) => exec_built_in(context, tok, args)?,
        }
    }

    Ok(())
}

pub fn interprete_ast(root_node: Program) -> (InterpreterResult, InterpreterContext) {
    let mut context = InterpreterContext::new();

    for block in root_node.0 {
        match exec_block(&mut context, &block) {
            Ok(_) => {},
            Err(err) => return (Err(err), context),
        }
    }

    (Ok(()), context)
}