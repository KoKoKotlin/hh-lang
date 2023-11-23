use crate::{ast::{Program, Literal, Block, Statement, Expr}, tokenizer::{Token, TokenKind}};

#[derive(Debug)]
struct Scope {
    names: Vec<Name>,
}

impl Scope {
    fn new(init: Option<Vec<(&Token, &Literal)>>) -> Self {

        if let Some(init) = init {
            let mut names = vec![];

            for (token, lit) in init.into_iter() {
                names.push(Name::make_var(&token.symbols, Some(lit.clone())));
            }            
            Self { names }
        } else {
            Self { names: vec![] }
        }
    }

    fn get_name(&mut self, tok: &Token) -> Option<&mut Name> {
        for var in self.names.iter_mut() {
            if var.name == tok.symbols { return Some(var); }
        }

        None
    }

    fn name_exists(&self, tok: &Token) -> bool {
        self.names.iter().any(|n| n.name == tok.symbols)
    }

    fn get_var_value(&self, tok: &Token) -> Result<Literal, InterpreterError> {
        for name in self.names.iter() {
            if name.name == tok.symbols {
                if let Some(value) = &name.value {
                    return Ok(value.clone());
                } else {
                    return Err(InterpreterError::VariableNotInitialized(tok.clone()));
                }
            }
        }

        Err(InterpreterError::VariableDoesNotExists(tok.clone()))
    }

    fn push(&mut self, name: Name) {
        self.names.push(name);
    }
}

#[derive(Debug)]
struct Name {
    name: String,
    value: Option<Literal>,
    constant: bool,
}

impl Name {
    fn is_var(&self) -> bool {
        !self.constant
    }

    fn is_const(&self) -> bool {
        self.constant
    }

    fn make_const(name: &str, value: Literal) -> Self {
        Self { name: name.to_string(), value: Some(value), constant: true }
    }

    fn make_var(name: &str, value: Option<Literal>) -> Self {
        Self { name: name.to_string(), value, constant: false }
    }
}

#[derive(Debug, Clone)]
struct Func {
    pub name: String,
    pub args: Vec<Token>,
    pub code: Block,
}

impl Func {
    fn new(name: &str, args: &Vec<Token>, code: &Block) -> Self {
        Self { name: name.to_owned(), args: args.clone(), code: code.clone() }
    }
}

#[derive(Debug)]
pub struct InterpreterContext {
    func_decls: Vec<Func>,
    global_scope: Scope,
    scope_stack: Vec<Scope>,
}

#[derive(Debug)]
pub enum InterpreterError {
    VariableDoesNotExists(Token),
    ConstantReassign(Token),
    ListReassign,
    VariableNotInitialized(Token),
    UndefinedBinOperation(Literal, Token, Literal),
    DivisionByZero,
    TypeError(String),
    ValueError(String),
    IndexOutOfBounds,
    FunctionNotDeclared,
}

pub type InterpreterResult = Result<(), InterpreterError>;

impl InterpreterContext {
    pub fn new() -> Self {
        Self { global_scope: Scope::new(None), func_decls: vec![], scope_stack: vec![] }
    }

    fn create_var(&mut self, tok: &Token, lit: Option<Literal>) {
        let scope = if let Some(local_scope) = self.scope_stack.last_mut() {
            local_scope
        } else {
            &mut self.global_scope
        };

        scope.push(Name::make_var(&tok.symbols, lit));
    }

    fn create_const(&mut self, tok: &Token, lit: Literal) {
        let scope = if let Some(local_scope) = self.scope_stack.last_mut() {
            local_scope
        } else {
            &mut self.global_scope
        };

        scope.push(Name::make_const(&tok.symbols, lit));
    }

    fn get_name(&mut self, tok: &Token) -> Option<&mut Name> {
        if let Some(scope) = self.scope_stack.last_mut() {
            if let Some(name) = scope.get_name(tok) {
                return Some(name);
            }
        } else if let Some(name) = self.global_scope.get_name(tok) {
            return Some(name);
        } 

        None
    }

    fn name_exists(&self, tok: &Token) -> bool {
        let exists_globally = self.global_scope.name_exists(tok);
        self.scope_stack.last().map_or(exists_globally, |scope| scope.name_exists(tok))
    }

    fn get_var_value(&self, tok: &Token) -> Result<Literal, InterpreterError> {
        if !self.name_exists(tok) { return Err(InterpreterError::VariableDoesNotExists(tok.clone())) };

        if let Some(scope) = self.scope_stack.last() {
            scope.get_var_value(tok)
        } else {
            self.global_scope.get_var_value(tok)
        }
    }

    fn index_var(&self, tok: &Token, idx_expr: &Expr) -> Result<Literal, InterpreterError> {
        let name_lit = self.get_var_value(tok)?;
        self.index(&name_lit, idx_expr)
    }

    fn index(&self, lit: &Literal, idx_expr: &Expr) -> Result<Literal, InterpreterError> {
        if let Literal::Number(idx) = self.eval(idx_expr)? {
            if idx < 0 {
                return Err(InterpreterError::IndexOutOfBounds);
            }

            match lit {
                Literal::String(str) => return Err(InterpreterError::TypeError("String cannot be indexed".to_string())),
                Literal::Number(_) => return Err(InterpreterError::TypeError("Number cannot be indexed".to_string())),
                Literal::True => return Err(InterpreterError::TypeError("Bool cannot be indexed".to_string())),
                Literal::False => return Err(InterpreterError::TypeError("Bool cannot be indexed".to_string())),
                Literal::Unit => return Err(InterpreterError::TypeError("Unit cannot be indexed".to_string())),
                Literal::List(values) => {
                    if idx as usize >= values.len() {
                        return Err(InterpreterError::IndexOutOfBounds);
                    }

                    return Ok(*values[idx as usize].clone());
                },
            }
        } else {
            return Err(InterpreterError::TypeError("Indices must be Number".to_string()));
        }
    }

    fn assign_indexed(&mut self, tok: &Token, idx_expr: &Expr, val_expr: &Expr) -> InterpreterResult {
        let idx_lit = self.eval(idx_expr)?;
        let result = self.eval(val_expr)?;

        if let Some(name) = self.get_name(tok) {
            if let Literal::Number(idx) = idx_lit {
                if idx < 0 {
                    return Err(InterpreterError::IndexOutOfBounds);
                }

                if name.value.is_none() {
                    return Err(InterpreterError::VariableNotInitialized(tok.clone()));
                }

                match name.value.as_mut().unwrap() {
                    Literal::String(_) => return Err(InterpreterError::TypeError("String cannot be indexed".to_string())),
                    Literal::Number(_) => return Err(InterpreterError::TypeError("Number cannot be indexed".to_string())),
                    Literal::True => return Err(InterpreterError::TypeError("Bool cannot be indexed".to_string())),
                    Literal::False => return Err(InterpreterError::TypeError("Bool cannot be indexed".to_string())),
                    Literal::Unit => return Err(InterpreterError::TypeError("Unit cannot be indexed".to_string())),
                    Literal::List(ref mut values) => {
                        if idx as usize >= values.len() {
                            return Err(InterpreterError::IndexOutOfBounds);
                        }

                        values[idx as usize] = Box::new(result);
                        return Ok(());
                    },
                }
            } else {
                return Err(InterpreterError::TypeError("Indices must be Number".to_string()));
            }
        } else {
            return Err(InterpreterError::VariableDoesNotExists(tok.clone()));
        }
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
                self.get_var_value(tok)
            },
            Expr::IdentIndexed(tok, idx_expr) => self.index_var(tok, idx_expr),
        }
    }

    fn get_func(&self, func_name: &Token) -> Option<Func> {
        for func in self.func_decls.iter() {
            if func.name == func_name.symbols { return Some(func.clone()); }
        }
        None
    }

    fn exec_func(&mut self, func_name: &Token, args_exprs: &Vec<Expr>) -> InterpreterResult {
        if let Some(func) = self.get_func(func_name) {
            let args_lits: Result<Vec<Literal>, InterpreterError> = args_exprs.iter()
                .map(|expr| self.eval(expr))
                .collect();
            let args_lits = args_lits?;

            let func_scope = Scope::new(Some(func.args.iter().zip(args_lits.iter()).collect()));
            self.scope_stack.push(func_scope);
            exec_block(self, &func.code)?;
            let _ = self.scope_stack.pop();
        } else {
            return Err(InterpreterError::FunctionNotDeclared);
        }

        Ok(())
    }
}

fn apply_op(left: &Literal, op: &Token, right: &Literal) -> Result<Literal, InterpreterError> {
    use TokenKind::*;

    return match op.kind {
        Plus => {
            match (left, right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left + right)),
                
                (Literal::String(left), Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
                (Literal::String(left), _) => Ok(Literal::String(format!("{}{}", left, right))),
                (_, Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
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
    if !context.name_exists(tok) {
        return Err(InterpreterError::VariableDoesNotExists(tok.clone()));
    }

    let result = context.eval(expr);
    let name = context.get_name(tok).unwrap();

    if name.is_const() {
        return Err(InterpreterError::ConstantReassign(tok.clone()));
    }

    name.value = Some(result?);

    Ok(())
}

fn exec_list_reassign(context: &mut InterpreterContext, tok: &Token, idx_expr: &Expr, val_expr: &Expr) -> InterpreterResult {
    if !context.name_exists(tok) {
        return Err(InterpreterError::VariableDoesNotExists(tok.clone()));
    }

    context.assign_indexed(tok, idx_expr, val_expr)?;

    Ok(())
}

fn exec_const_assign(context: &mut InterpreterContext, assigns: &Vec<(Token, Literal)>) -> InterpreterResult {
    for (tok, lit) in assigns {
        context.create_const(tok, lit.clone());
    }

    Ok(())
}

fn exec_var_decl(context: &mut InterpreterContext, decls: &Vec<(Token, Option<Literal>)>) -> InterpreterResult {
    for (tok, lit) in decls {
        context.create_var(tok, lit.clone());
    }

    Ok(())
}

fn exec_list_decl(context: &mut InterpreterContext, tok: &Token, expr: &Expr, init_val: &Vec<Expr>) -> InterpreterResult {
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
        Literal::List(_) => return Err(InterpreterError::TypeError("List cannot be used as a list".to_string())),
    };

    let mut value = Vec::with_capacity(capacity);
    if capacity < init_val.len() {
        return Err(InterpreterError::IndexOutOfBounds);
    }

    for val in init_val {
        let lit = context.eval(val)?;
        value.push(Box::new(lit));
    }

    for _ in 0..(capacity - init_val.len()) {
        value.push(Box::new(Literal::Number(0)));
    }
    
    context.create_var(tok, Some(Literal::List(value)));
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
                print!("{}", arg);
            }
        },
        Println => {
            for arg in args {
                let arg = context.eval(arg)?;
                print!("{}", arg);
            }
            println!();
        },
        _ => unimplemented!("BuiltIn {:?} not implemented yet", tok.kind),
    }

    Ok(())
}

fn exec_func_decl(context: &mut InterpreterContext, func_name: &Token, args: &Vec<Token>, code: &Block) -> InterpreterResult {
    context.func_decls.push(Func::new(&func_name.symbols, args, code));
    Ok(())
}

fn exec_func_call(context: &mut InterpreterContext, func_name: &Token, args_exprs: &Vec<Expr>) -> InterpreterResult {
    context.exec_func(func_name, args_exprs)
}

fn exec_block(context: &mut InterpreterContext, block: &Block) -> InterpreterResult {
    for statment in block.0.iter() {
        match statment {
            Statement::Reassign(tok, expr) => exec_reassign(context, tok, expr)?,
            Statement::ListReassign(tok, idx_expr, val_expr) => exec_list_reassign(context, tok, idx_expr, val_expr)?,
            Statement::ConstAssign(assigns) => exec_const_assign(context, assigns)?,
            Statement::VarDecl(decls) => exec_var_decl(context, decls)?,
            Statement::ListDecl(tok, expr, init_val) => exec_list_decl(context, tok, expr, init_val)?,
            Statement::If(cond, if_block, else_block) => exec_if(context, cond, if_block, else_block.as_ref())?,
            Statement::While(cond, block) => exec_while(context, cond, block)?,
            Statement::BuiltIn(tok, args) => exec_built_in(context, tok, args)?,
            Statement::FuncDecl(func_name, args, code) => exec_func_decl(context, func_name, args, code)?,
            Statement::FuncCall(func_name, args_exprs) => exec_func_call(context, func_name, args_exprs)?,
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