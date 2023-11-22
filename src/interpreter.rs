use crate::{ast::{Program, Literal, Block, Statement, Expr}, tokenizer::Token};

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

struct InterpreterContext {
    variables: Vec<Variable>,
}

impl InterpreterContext {
    fn eval(&self, expr: &Expr) -> Literal {
        todo!()
    } 
}

#[derive(Debug)]
pub enum InterpreterError {
    VariableDoesNotExists,
    ConstantReassign,
}

pub type InterpreterResult = Result<(), InterpreterError>;

impl InterpreterContext {
    pub fn new() -> Self {
        Self { variables: vec![] }
    }

    pub fn get_var(&mut self, name: &str) -> Option<&mut Variable> {
        for var in self.variables.iter_mut() {
            if var.name == name { return Some(var); }
        }

        None
    }

    pub fn var_exists(&self, name: &str) -> bool {
        self.variables.iter().any(|var| var.name == name)
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

    var.value = Some(result);

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
    let cond = context.eval(cond);

    if cond.is_truthy() {
        exec_block(context, if_block)?;
    } else if let Some(else_block) = else_block {
        exec_block(context, else_block)?;
    }

    Ok(())
}

fn exec_while(context: &mut InterpreterContext, cond: &Expr, block: &Block) -> InterpreterResult {
    let cond = context.eval(cond);

    while cond.is_truthy() {
        exec_block(context, block)?;
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
        }
    }

    Ok(())
}

pub fn interprete_ast(root_node: Program) -> InterpreterResult {
    let mut context = InterpreterContext::new();

    for block in root_node.0 {
        exec_block(&mut context, &block)?;
    }

    Ok(())
}