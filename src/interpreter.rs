use crate::{ast::{Program, Literal, Block, Statement, Expr}, tokenizer::{Token, TokenKind}};

#[derive(Debug)]
struct Variable {
    name: String,
    value: Option<Literal>,
    constant: bool,
}

impl Variable {
    fn make_const(name: &str, value: Literal) -> Self {
        Self { name: name.to_string(), value: Some(value), constant: true }
    }

    fn make_var(name: &str, value: Option<Literal>) -> Self {
        Self { name: name.to_string(), value, constant: false }
    }
}

#[derive(Debug)]
pub struct InterpreterContext {
    variables: Vec<Variable>,
}

#[derive(Debug)]
pub enum InterpreterError {
    VariableDoesNotExists,
    ConstantReassign,
    VariableNotInitialized,
    UndefinedBinOperation(Literal, Token, Literal),
    DivisionByZero,
}

pub type InterpreterResult = Result<(), InterpreterError>;

impl InterpreterContext {
    pub fn new() -> Self {
        Self { variables: vec![] }
    }

    fn get_var(&mut self, name: &str) -> Option<&mut Variable> {
        for var in self.variables.iter_mut() {
            if var.name == name { return Some(var); }
        }

        None
    }

    fn var_exists(&self, name: &str) -> bool {
        self.variables.iter().any(|var| var.name == name)
    }

    fn get_var_value(&self, name: &str) -> Result<Literal, InterpreterError> {
        for var in self.variables.iter() {
            if var.name == name { 
                let lit = var.value.clone();
                if let Some(lit) = lit {
                    return Ok(lit);
                } else {
                    return Err(InterpreterError::VariableNotInitialized);
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
                self.get_var_value(&tok.symbols)
            },
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
    if !context.var_exists(&tok.symbols) {
        return Err(InterpreterError::VariableDoesNotExists);
    }

    let result = context.eval(expr);
    let var = context.get_var(&tok.symbols).unwrap();

    if var.constant {
        return Err(InterpreterError::ConstantReassign);
    }

    var.value = Some(result?);

    Ok(())
}

fn exec_const_assign(context: &mut InterpreterContext, assigns: &Vec<(Token, Literal)>) -> InterpreterResult {
    for (tok, lit) in assigns {
        let var = Variable::make_const(&tok.symbols, lit.clone());
        context.variables.push(var);
    }

    Ok(())
}

fn exec_var_decl(context: &mut InterpreterContext, decls: &Vec<(Token, Option<Literal>)>) -> InterpreterResult {
    for (tok, lit) in decls {
        let var = Variable::make_var(&tok.symbols, lit.clone());
        context.variables.push(var);
    }

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
            Statement::ConstAssign(assigns) => exec_const_assign(context, assigns)?,
            Statement::VarDecl(decls) => exec_var_decl(context, decls)?,
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