use std::{rc::Rc, cell::RefCell, fs, path::{Path, PathBuf}};
use crate::{ast::{Program, Literal, Block, Statement, Expr, Reassign, For}, tokenizer::{Token, TokenKind, to_operator}, parser::Parser};

pub type MutRc<T> = Rc<RefCell<T>>;

pub fn mut_rc<T>(t: T) -> MutRc<T> {
    Rc::new(RefCell::new(t))
}

#[derive(Debug, Clone)]
struct Scope {
    _func_name: Option<Token>,
    names: Vec<Name>,
    is_returning: bool,
    is_breaking: bool,
    is_continuing: bool,
}

impl Scope {
    fn new(func_name: Option<&Token>, init: Option<Vec<(&Token, MutRc<Literal>)>>) -> Self {
        if let Some(init) = init {
            let mut names = vec![];

            for (token, lit) in init.into_iter() {
                names.push(Name::make_var(&token.symbols, Some(lit.clone())));
            }            
            Self { _func_name: func_name.map(|f| f.clone()), names, is_returning: false, is_breaking: false, is_continuing: false }
        } else {
            Self { _func_name: func_name.map(|f| f.clone()), names: vec![], is_returning: false, is_breaking: false, is_continuing: false }
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

    fn get_var_value(&self, tok: &Token) -> Result<MutRc<Literal>, InterpreterError> {
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

    fn delete_name(&mut self, tok: &Token) {
        let mut found_idx: Option<usize> = None;
        for (idx, name) in self.names.iter().enumerate() {
            if name.name == tok.symbols {
                found_idx = Some(idx);
                break;
            }
        }

        found_idx.map(|idx| self.names.remove(idx));
    }
}

#[derive(Debug, Clone)]
struct Name {
    name: String,
    value: Option<MutRc<Literal>>,
    constant: bool,
}

impl Name {
    fn is_var(&self) -> bool {
        !self.constant
    }

    fn is_const(&self) -> bool {
        self.constant
    }

    fn make_const(name: &str, value: MutRc<Literal>) -> Self {
        Self { name: name.to_string(), value: Some(value), constant: true }
    }

    fn make_var(name: &str, value: Option<MutRc<Literal>>) -> Self {
        Self { name: name.to_string(), value, constant: false }
    }
}

#[derive(Debug, Clone)]
struct Func {
    pub name: Token,
    pub args: Vec<Token>,
    pub code: Block,
}

impl Func {
    fn new(name: &Token, args: &Vec<Token>, code: &Block) -> Self {
        Self { name: name.clone(), args: args.clone(), code: code.clone() }
    }

    fn new_lambda(name: &Token, args: &Vec<Token>, body: &Statement) -> Self {
        let code = Block(vec![body.clone()]);
        Self { name: name.clone(), args: args.clone(), code }
    }
}

#[derive(Debug, Clone)]
struct Record {
    pub name: Token,
    pub fields: Vec<Token>,
}

impl Record {
    pub fn new(name: &Token, fields: &Vec<Token>) -> Self {
        Self { name: name.clone(), fields: fields.clone() }
    }

    fn find_field(&self, field_tok: &Token) -> Option<usize> {
        self.fields.iter()
            .enumerate()
            .find(|(_, f_tok)| f_tok.symbols == field_tok.symbols)
            .map(|opt| opt.0)
    }
}

#[derive(Debug, Clone)]
pub struct InterpreterContext {
    record_decls: Vec<Record>,
    func_decls: Vec<Func>,
    global_scope: Scope,
    scope_stack: Vec<Scope>,
    include_path: Vec<String>,
}

#[derive(Debug)]
pub enum InterpreterError {
    VariableDoesNotExists(Token),
    ConstantReassign(Token),
    VariableNotInitialized(Token),
    UndefinedUnaryOperation(Token, Literal),
    UndefinedBinOperation(Literal, Token, Literal),
    DivisionByZero(Token),
    TypeError(Token, String),
    ValueError(Token, String),
    IndexOutOfBounds(Token),
    FunctionNotDeclared(Token),
    CallArgumentCount(Token),
    RecordNotDeclared(Token),
    RecordFieldCount(Token),
    RecordFieldDoesNotExist(Token, Token),
    BuiltInError(Token, String),
    ImportError(String),
}

impl InterpreterError {
    pub fn info(&self) -> String {
        match self {
            InterpreterError::VariableDoesNotExists(name_tok) => 
                format!("Variable {} at {} does not exists!", name_tok.symbols, name_tok.loc),
            InterpreterError::ConstantReassign(name_tok) => 
                format!("Can't reassing constant {} at {}!", name_tok.symbols, name_tok.loc),
            InterpreterError::VariableNotInitialized(name_tok) => 
                format!("Variable {} at {} is not initialized!", name_tok.symbols, name_tok.loc),
            InterpreterError::UndefinedUnaryOperation(op_tok, lit) => 
                format!("Can't apply unary operator {} at {} to literal of type {}!", op_tok.symbols, op_tok.loc, lit.get_type()),
            InterpreterError::UndefinedBinOperation(lit1, op_tok,lit2) => 
                format!("Can't perform binary operation {} at {} on values of type {} and {}!", op_tok.symbols, op_tok.loc, lit1.get_type(), lit2.get_type()),
            InterpreterError::DivisionByZero(op_token) => 
                format!("Division by zero at {}!", op_token.loc),
            InterpreterError::TypeError(name_tok, reason) => 
                format!("TypeError at {}! Reason: {}", name_tok.loc, reason),
            InterpreterError::ValueError(name_tok, reason) =>
                format!("ValueError at {}! Reason: {}", name_tok.loc, reason),
            InterpreterError::IndexOutOfBounds(name_tok) =>
                format!("Index out of bounds ({}) at {}!", name_tok.symbols, name_tok.loc),
            InterpreterError::FunctionNotDeclared(name_tok) =>
                format!("Function {} not declared at {}!", name_tok.symbols, name_tok.loc),
            InterpreterError::CallArgumentCount(name_tok) => 
                format!("Wrong number of arguments to call of {} at {}!", name_tok.symbols, name_tok.loc),
            InterpreterError::RecordNotDeclared(name_tok) => 
                format!("Record {} not declared at {}!", name_tok.symbols, name_tok.loc),
            InterpreterError::RecordFieldCount(name_tok) => 
                format!("Wrong number of initial expressions for new record {} at {}!", name_tok.symbols, name_tok.loc),
            InterpreterError::RecordFieldDoesNotExist(record_tok, field_tok) => 
                format!("Field {} does not exist in record {} at {}!", field_tok.symbols, record_tok.symbols, field_tok.loc),
            InterpreterError::BuiltInError(name_tok, err_str) => 
                format!("Error during execution of built in function {} at {}: Reason: {}", name_tok.symbols, name_tok.loc, err_str),
            InterpreterError::ImportError(reason) => 
                format!("Error during import! Reason: {}", reason),
        }
    }
}

pub type InterpreterResult = Result<(), InterpreterError>;

const STANDARD_INCLUDE_PATH: &'static str = "/usr/local/lib/hh-stdlib";

impl InterpreterContext {
    pub fn new(additional_include_paths: Vec<String>) -> Self {
        let mut include_path = vec![STANDARD_INCLUDE_PATH.to_owned()];
        include_path.extend(additional_include_paths);
        Self { global_scope: Scope::new(None, None), record_decls: vec![], func_decls: vec![], scope_stack: vec![], include_path }
    }

    fn create_var(&mut self, tok: &Token, lit: Option<MutRc<Literal>>) {
        self.get_curr_scope_mut().delete_name(tok);
        self.get_curr_scope_mut().push(Name::make_var(&tok.symbols, lit));
    }

    fn create_const(&mut self, tok: &Token, lit: MutRc<Literal>) {
        self.get_curr_scope_mut().delete_name(tok);
        self.get_curr_scope_mut().push(Name::make_const(&tok.symbols, lit));
    }

    fn get_name(&mut self, tok: &Token) -> Option<&mut Name> {
        let scope = self.scope_stack.last_mut();

        match scope {
            Some(s) => {
                if s.name_exists(tok) {
                    s.get_name(tok)
                } else {
                    self.global_scope.get_name(tok)
                }
            },
            None => return self.global_scope.get_name(tok),
        }
    }

    fn get_curr_scope_mut(&mut self) -> &mut Scope {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope
        } else {
            &mut self.global_scope
        }
    }

    fn name_exists(&self, tok: &Token) -> bool {
        let scope = self.scope_stack.last();

        match scope {
            Some(s) => s.name_exists(tok) || self.global_scope.name_exists(tok),
            None => return self.global_scope.name_exists(tok),
        }
    }

    fn get_var_value(&self, tok: &Token) -> Result<MutRc<Literal>, InterpreterError> {
        if !self.name_exists(tok) { return Err(InterpreterError::VariableDoesNotExists(tok.clone())) };

        let scope = self.scope_stack.last();

        match scope {
            Some(s) => {
                if s.name_exists(tok) {
                    s.get_var_value(tok)
                } else {
                    self.global_scope.get_var_value(tok)
                }
            },
            None => return self.global_scope.get_var_value(tok),
        }
    }

    fn index_var(&mut self, tok: &Token, idx_expr: &Expr) -> Result<MutRc<Literal>, InterpreterError> {
        let name_lit = self.get_var_value(tok)?;
        let name_lit = name_lit.borrow();
        self.index(tok, &name_lit, idx_expr)
    }

    fn index(&mut self, tok: &Token, lit: &Literal, idx_expr: &Expr) -> Result<MutRc<Literal>, InterpreterError> {
        if let Literal::Int(idx) = *self.eval(idx_expr)?.borrow() {
            if idx < 0 {
                return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
            }

            match lit {
                Literal::String(s) => {
                    if idx as usize >= s.len() {
                        return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                    }

                    return Ok(mut_rc(s.chars().nth(idx as usize).unwrap().into()));
                },
                Literal::List(values) => {
                    if idx as usize >= values.len() {
                        return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                    }

                    return Ok(values[idx as usize].clone());
                },
                lit => return Err(InterpreterError::TypeError(tok.clone(), format!("{} cannot be indexed!", lit.get_type()))),
            }
        } else {
            return Err(InterpreterError::TypeError(tok.clone(), "Indices must be Number".to_string()));
        }
    }

    fn assign_indexed(&mut self, tok: &Token, idx_expr: &Expr, val_expr: &Expr) -> InterpreterResult {
        let idx_lit = self.eval(idx_expr)?.borrow().clone();
        let result = self.eval(val_expr)?;

        if let Some(name) = self.get_name(tok) {
            if let Literal::Int(idx) = idx_lit {
                if idx < 0 {
                    return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                }

                if name.value.is_none() {
                    return Err(InterpreterError::VariableNotInitialized(tok.clone()));
                }
                
                let value = name.value.as_ref().unwrap().clone();
                let mut value_ = value.borrow_mut();
        
                match &mut *(value_) {
                    Literal::List(values) => {
                        if idx as usize >= values.len() {
                            return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                        }

                        values[idx as usize] = mut_rc(result.borrow().clone());
                        return Ok(());
                    },
                    Literal::String(s) => {
                        if idx as usize >= s.len() {
                            return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                        }
                       
                        match result.borrow().clone() {
                            Literal::Char(c) => { 
                                s.remove(idx as usize); 
                                s.insert(idx as usize, c);
                            },
                            _ => return Err(InterpreterError::TypeError(tok.clone(), "Indices must be Number".to_string())),
                        }
                        return Ok(());
                    },
                    lit => return Err(InterpreterError::TypeError(tok.clone(), format!("{} cannot be indexed!", lit.get_type()))),
                };
            } else {
                return Err(InterpreterError::TypeError(tok.clone(), "Indices must be Number".to_string()));
            }
        } else {
            return Err(InterpreterError::VariableDoesNotExists(tok.clone()));
        }
    }

    fn eval(&mut self, expr: &Expr) -> Result<MutRc<Literal>, InterpreterError> {
        match expr {
            Expr::Binary(left, op, right) => {
                let left = self.eval(&left)?;
                
                // short curcuiting for and and or
                match op.kind {
                    TokenKind::And if !left.borrow().is_truthy() => return Ok(mut_rc(false.into())),
                    TokenKind::Or if left.borrow().is_truthy() => return Ok(mut_rc(true.into())),
                    _ => {},
                }
                let right = self.eval(&right)?;
                apply_op(op, left, &op.kind, right).map(|lit| mut_rc(lit))
            },
            Expr::Unary(op, operand) => {
                let operand = self.eval(&operand)?;
                apply_unary(op, operand).map(|lit| mut_rc(lit))
            }
            Expr::Literal(lit) => Ok(mut_rc(lit.borrow().clone())),
            Expr::Ident(tok) => {
                self.get_var_value(tok)
            },
            Expr::IdentIndexed(tok, idx_expr) => self.index_var(tok, idx_expr),
            Expr::BuiltInCall(tok, args) => exec_built_in(self, tok, args),
            Expr::FuncCall(func_name, args_exprs) => self.exec_func(func_name, args_exprs),
            Expr::ListInstantiation(tok, capacity, init_exprs) => exec_list_instantiation(self, tok, capacity, init_exprs),
            Expr::RecordInstantiation(record_name, field_exprs) => self.instance_record(record_name, field_exprs),
            Expr::RecordFieldDeref(record_tok, field_toks) => exec_record_field_deref(self, record_tok, field_toks),
            Expr::LambdaInstantiation(arg_toks, body_expr) => self.instance_lambda(arg_toks, body_expr),
            Expr::LambdaCall(invoke_tok, lambda_expr, args_exprs) => {
                let lambda = self.eval(&lambda_expr)?;
                
                let (arg_toks, body) = match lambda.borrow().clone() {
                    Literal::LambdaInstance(args, body) => (args, body),
                    _ => return Err(InterpreterError::TypeError(invoke_tok.clone(), "Can't call non-lambda!".to_string())),
                };

                self.exec_lambda(invoke_tok, &arg_toks, &body, args_exprs)
            }
        }
    }

    fn get_func(&self, func_name: &Token) -> Option<Func> {
        for func in self.func_decls.iter() {
            if func.name.symbols == func_name.symbols { return Some(func.clone()); }
        }
        
        None
    }

    fn exec_func(&mut self, func_name: &Token, args_exprs: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
        if let Some(func) = self.get_func(func_name) {
            if func.args.len() != args_exprs.len() {
                return Err(InterpreterError::CallArgumentCount(func_name.clone()));
            }

            let args_lits: Result<Vec<MutRc<Literal>>, InterpreterError> = args_exprs.iter()
                .map(|expr| self.eval(expr))
                .collect();
            let args_lits = args_lits?;

            let func_scope = Scope::new(Some(func_name), Some(func.args.iter().zip(args_lits.into_iter()).collect()));
            self.scope_stack.push(func_scope);
            let return_val = exec_block(self, &func.code)?;
            let _val = self.scope_stack.pop();

            return Ok(return_val);
        } else {
            return Err(InterpreterError::FunctionNotDeclared(func_name.clone()));
        }
    }

    fn exec_lambda(&mut self, invoke_tok: &Token, arg_toks: &Vec<Token>, body: &Statement, args_exprs: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
        if arg_toks.len() != args_exprs.len() {
            return Err(InterpreterError::CallArgumentCount(invoke_tok.clone()));
        }

        let args_lits: Result<Vec<MutRc<Literal>>, InterpreterError> = args_exprs.iter()
            .map(|expr| self.eval(expr))
            .collect();
        let args_lits = args_lits?;

        let func_scope = Scope::new(Some(invoke_tok), Some(arg_toks.iter().zip(args_lits.into_iter()).collect()));
        self.scope_stack.push(func_scope);
        let return_val = exec_block(self, &Block(vec![body.clone()]))?;
        let _ = self.scope_stack.pop();
        return Ok(return_val);
    }

    fn get_record(&self, record_name: &Token) -> Option<Record> {
        for record in self.record_decls.iter() {
            if record.name.symbols == record_name.symbols { return Some(record.clone()); }
        }
        None
    }

    fn instance_record(&mut self, record_name: &Token, field_exprs: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
        let record = self.get_record(record_name);

        if let Some(record) = record {
            if field_exprs.len() != record.fields.len() {
                return Err(InterpreterError::RecordFieldCount(record_name.clone()));
            }

            let field_lits: Result<Vec<MutRc<Literal>>, InterpreterError> = field_exprs.iter().map(|expr| self.eval(expr)).collect();
            return Ok(mut_rc(Literal::RecordInstance(record_name.clone(), field_lits?)));
        } else {
            return Err(InterpreterError::RecordNotDeclared(record_name.clone()));
        }
    }

    fn instance_lambda(&mut self, arg_toks: &Vec<Token>, body_expr: &Statement) -> Result<MutRc<Literal>, InterpreterError> {
        Ok(mut_rc(Literal::LambdaInstance(arg_toks.clone(), Box::new(body_expr.clone()))))
    }

    fn record_field_defef(&mut self, record_tok: &Token, record_lit: MutRc<Literal>, field_toks: &Vec<Token>, current_tok_idx: usize) -> Result<MutRc<Literal>, InterpreterError> {
        if current_tok_idx >= field_toks.len() { return Ok(record_lit); }

        let (record_name, field_lits) = match record_lit.borrow().clone() {
            Literal::RecordInstance(record_name, field_lits) => (record_name, field_lits),
            lit => return Err(
                InterpreterError::TypeError(record_tok.clone(), 
                format!("Literal of type {} cannot be derefed!", lit.get_type()))),
        };

        let record = self.get_record(&record_name)
            .ok_or(InterpreterError::RecordNotDeclared(record_tok.clone()))?;

        let field_tok = &field_toks[current_tok_idx];
        if let Some(idx) = record.find_field(field_tok) {
            let lit = &field_lits[idx];
            return self.record_field_defef(record_tok, lit.clone(), field_toks, current_tok_idx + 1);
        } else {
            return Err(InterpreterError::RecordFieldDoesNotExist(record.name.clone(), field_tok.clone()));
        }
    }

    fn is_returning(&self) -> bool {
        if let Some(scope) = self.scope_stack.last() {
            scope.is_returning
        } else {
            false
        }
    }

    fn is_breaking(&self) -> bool {
        if let Some(scope) = self.scope_stack.last() {
            scope.is_breaking
        } else {
            false
        }
    }

    fn is_continuing(&self) -> bool {
        if let Some(scope) = self.scope_stack.last() {
            scope.is_continuing
        } else {
            false
        }
    }

    fn start_returning(&mut self) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.is_returning = true;
        }
    }

    fn set_breaking(&mut self, v: bool) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.is_breaking = v;
        }
    }

    fn set_continuing(&mut self, v: bool) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.is_continuing = v;
        }
    }
    
    fn search_file_in_path(&self, file_name: &str) -> Result<String, InterpreterError> {
        let path = Path::new(file_name);
        if path.exists() {
            if path.is_file() {
                return Ok(file_name.to_owned())
            } else {
                return Err(InterpreterError::ImportError(format!("{} is not a file.", file_name)));
            }
        }

        for path in self.include_path.iter() {
            let mut path_buf = PathBuf::new();
            path_buf.push(path);
            
            if !path_buf.exists() {
                println!("Warning: {} added to include path is not an existing path.", path_buf.display());
                continue;
            }

            path_buf.push(file_name);
            if path_buf.exists() {
                return Ok(path_buf.to_str().unwrap().to_owned())
            }
        }

        Err(InterpreterError::ImportError(format!("{} does not exist.", file_name)))
    }

    fn import(&mut self, source_code: &str, filename: String) -> InterpreterResult {
        let mut parser = Parser::new(&source_code, filename);
        let ast_root = parser.parse();

        if let Ok(ast_root) = ast_root {
            for block in ast_root.0 {
                match exec_block(self, &block) {
                    Ok(_) => {},
                    Err(err) => return Err(err),
                }
            }

            Ok(())
        } else {
            return Err(InterpreterError::ImportError(format!("Parser error")));
        }
        
    }

    fn print_stack_trace(&self) {
        for scope in self.scope_stack.iter().rev() {
            println!("{}", &scope._func_name.as_ref().unwrap());
        }
    }
}

fn apply_unary(op: &Token, operand: MutRc<Literal>) -> Result<Literal, InterpreterError> {
    use TokenKind::*;

    // TODO for better performance don't clone
    let operand = operand.borrow().clone();

    return match op.kind {
        Minus => match &operand {
            Literal::Int(n) => Ok(Literal::Int(-n)),
            Literal::Float(n) => Ok(Literal::Float(-n)),
            _ => Err(InterpreterError::UndefinedUnaryOperation(op.clone(), operand.clone())),
        },
        Bang => match &operand {
            Literal::Bool(val) => Ok(Literal::Bool(!val)),
            _ => Err(InterpreterError::UndefinedUnaryOperation(op.clone(), operand.clone())),
        },
        _ => unimplemented!(),
    }
}

fn apply_op(op: &Token, left: MutRc<Literal>, op_kind: &TokenKind, right: MutRc<Literal>) -> Result<Literal, InterpreterError> {
    use TokenKind::*;

    // TODO for better performance don't clone
    let left = left.borrow().clone();
    let right = right.borrow().clone();

    return match op_kind {
        Plus => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left + right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok((*left as f64 + right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((left + *right as f64).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left + right).into()),
                (Literal::String(left), Literal::String(right)) => Ok(format!("{}{}", left, right).into()),
                (Literal::String(left), _) => Ok(format!("{}{}", left, right).into()),
                (_, Literal::String(right)) => Ok(format!("{}{}", left, right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Minus => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left - right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok((*left as f64 - right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((left - *right as f64).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left - right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Times => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left * right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok((*left as f64 * right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((left * *right as f64).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left * right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Div => {
            match (&left, &right) {
                (_, Literal::Int(right)) if *right == 0 => Err(InterpreterError::DivisionByZero(op.clone())),
                (_, Literal::Float(right)) if *right == 0.0 => Err(InterpreterError::DivisionByZero(op.clone())),
                
                (Literal::Int(left), Literal::Int(right)) => Ok((left / right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok((*left as f64 / right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((left / *right as f64).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left / right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Percent => {
            match (&left, &right) {
                (_, Literal::Int(right)) if *right == 0 => Err(InterpreterError::DivisionByZero(op.clone())),
                (_, Literal::Float(right)) if *right == 0.0 => Err(InterpreterError::DivisionByZero(op.clone())),
                (Literal::Int(left), Literal::Int(right)) => Ok((left % right).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left % right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        }
        And => {
            match (&left, &right) {
                (Literal::Bool(val1), Literal::Bool(val2)) => Ok((*val1 && *val2).into()),
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Int(left & right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Or => {
            match (&left, &right) {
                (Literal::Bool(val1), Literal::Bool(val2)) => Ok((*val1 || *val2).into()),
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Int(left | right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Xor => {
            match (&left, &right) {
                (Literal::Bool(val1), Literal::Bool(val2)) => Ok((*val1 ^ *val2).into()),
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Int(left ^ right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        LessThan => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left < right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok(((*left as f64) < *right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((*left < (*right as f64)).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left < right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        GreaterThan => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left > right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok(((*left as f64) > *right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((*left > (*right as f64)).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left > right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        LessOrEqual => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left <= right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok(((*left as f64) <= *right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((*left <= (*right as f64)).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left <= right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        GreaterOrEqual => {
            match (&left, &right) {
                (Literal::Int(left), Literal::Int(right)) => Ok((left >= right).into()),
                (Literal::Int(left), Literal::Float(right)) => Ok(((*left as f64) >= *right).into()),
                (Literal::Float(left), Literal::Int(right)) => Ok((*left >= (*right as f64)).into()),
                (Literal::Float(left), Literal::Float(right)) => Ok((left >= right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        EqualsEquals => {
            match (&left, &right) {
                (lit1, lit2) => Ok((*lit1 == *lit2).into())
            }
        },
        NotEqual => {
            match (&left, &right) {
                (lit1, lit2) => Ok((*lit1 != *lit2).into())
            }
        }
        _ => unimplemented!(),
    }
}

fn exec_reassign(context: &mut InterpreterContext, reassign: &Reassign) -> InterpreterResult {
    if !context.name_exists(&reassign.name_tok) {
        return Err(InterpreterError::VariableDoesNotExists(reassign.name_tok.clone()));
    }

    let result = context.eval(&reassign.assign_expr);
    let name = context.get_name(&reassign.name_tok).unwrap_or_else(|| {
        eprintln!("Couldn't find name despite it being existent. This is an interpreter bug! Name: {}: {}", reassign.name_tok, reassign.name_tok.loc);
        panic!()
    });

    if name.is_const() {
        return Err(InterpreterError::ConstantReassign(reassign.name_tok.clone()));
    }

    if reassign.record_fields.len() == 0 {
        match reassign.op_tok.kind {
            TokenKind::Equals => name.value = Some(result?),
            _ => {
                let op_kind   = to_operator(&reassign.op_tok.kind)
                    .expect("The parser makes sure that the kind is only a valid assign operator.");
                let left = name.value.as_ref();
                let right = result?;
                
                if left.is_none() {
                    return Err(InterpreterError::VariableNotInitialized(reassign.name_tok.clone()));
                }

                let result = mut_rc(apply_op(&reassign.op_tok, left.unwrap().clone(), &op_kind, right)?);
                name.value = Some(result);
            }
        }
    } else {
        if let Some(record_lit) = name.value.clone() {
            let record_deref = context.record_field_defef(&reassign.name_tok, record_lit, &reassign.record_fields, 0)?;
            match reassign.op_tok.kind {
                TokenKind::Equals => { let _ = record_deref.replace(result?.borrow().clone()); },
                _ => {
                    let op_kind   = to_operator(&reassign.op_tok.kind)
                        .expect("The parser makes sure that the kind is only a valid assign operator.");
                    let left = record_deref.borrow().clone();
                    let right = result?;
                    
                    let result = mut_rc(apply_op(&reassign.op_tok, mut_rc(left), &op_kind, right)?);
                    record_deref.replace(result.borrow().clone());
                }
            }
        } else {
            return Err(InterpreterError::VariableNotInitialized(reassign.name_tok.clone()));
        }
    }

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
        context.create_const(tok, mut_rc(lit.clone()));
    }

    Ok(())
}

fn exec_var_decl(context: &mut InterpreterContext, decls: &Vec<(Token, Option<Expr>)>) -> InterpreterResult {
    for (tok, expr) in decls {
        let lit = match expr {
            Some(expr) => Some(context.eval(expr)?),
            None => None,
        };
        
        let lits = lit.clone().map(|lit| lit);
        context.create_var(tok, lits);
    }

    Ok(())
}

fn exec_list_instantiation(context: &mut InterpreterContext, tok: &Token, capacity_expr: &Expr, init_exprs: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
    let capacity = match *context.eval(capacity_expr)?.borrow() {
        Literal::Int(num) => {
            if num < 0 {
                return Err(InterpreterError::ValueError(tok.clone(), "Number cannot be negative".to_string()));
            }

            num as usize
        },
        ref lit => return Err(InterpreterError::TypeError(tok.clone(), format!("{} cannot be used as a list", lit.get_type()))),
    };

    let mut value = Vec::with_capacity(capacity);
    if capacity < init_exprs.len() {
        return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
    }
    
    let init_lits: Result<Vec<MutRc<Literal>>, InterpreterError> = init_exprs.iter().map(|expr| context.eval(expr)).collect();
    let init_lits = init_lits?;

    value.extend(init_lits);
    for _ in 0..(capacity - init_exprs.len()) {
        value.push(mut_rc(Literal::Int(0)));
    }
    
    Ok(mut_rc(Literal::List(value)))
}

fn exec_if(context: &mut InterpreterContext, cond: &Expr, if_block: &Block, else_block: Option<&Block>) -> Result<MutRc<Literal>, InterpreterError> {
    let cond = context.eval(cond)?;
    let cond = cond.borrow();

    if cond.is_truthy() {
        exec_block(context, if_block)
    } else if let Some(else_block) = else_block {
        exec_block(context, else_block)
    } else {
        Ok(mut_rc(Literal::Unit))
    }
}

fn exec_while(context: &mut InterpreterContext, cond_expr: &Expr, block: &Block) -> Result<MutRc<Literal>, InterpreterError> {

    let mut cond = context.eval(cond_expr)?;
    while (*cond.borrow()).is_truthy() {
        let maybe_return_val = exec_block(context, block)?;

        if context.is_returning() {
            return Ok(maybe_return_val);
        }

        if context.is_breaking() {
            context.set_breaking(false);
            break;
        }

        if context.is_continuing() {
            context.set_continuing(false);
        }

        cond = context.eval(cond_expr)?;
    }

    Ok(mut_rc(Literal::Unit))
}

fn exec_for(context: &mut InterpreterContext, for_statement: &Box<For>) -> Result<MutRc<Literal>, InterpreterError> {
    let init_lit = context.eval(&for_statement.init_expr)?;
    context.create_var(&for_statement.ident_tok, Some(init_lit));

    let mut cond = context.eval(&for_statement.cond)?;
    while cond.borrow().is_truthy() {
        let maybe_return_val = exec_block(context, &for_statement.body.clone())?;
        
        if context.is_returning() {
            return Ok(maybe_return_val);
        }

        if context.is_breaking() {
            context.set_breaking(false);
            break;
        }

        if context.is_continuing() {
            context.set_continuing(false);
        }

        exec_block(context, &Block(vec![for_statement.loop_statement.clone()]))?;
        cond = context.eval(&for_statement.cond)?;
    }
    
    Ok(mut_rc(Literal::Unit))
}

fn get_args(context: &mut InterpreterContext, tok: &Token, expected_count: usize, args: &Vec<Expr>) -> Result<Vec<MutRc<Literal>>, InterpreterError> {
    if args.len() != expected_count {
        return Err(InterpreterError::CallArgumentCount(tok.clone()));
    }

    let exprs: Result<Vec<MutRc<Literal>>, InterpreterError> = args.iter().map(|arg| context.eval(arg)).collect();
    exprs
}

fn exec_built_in(context: &mut InterpreterContext, tok: &Token, args: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
    use TokenKind::*;

    match tok.kind {
        Print => {
            for arg in args {
                let arg = context.eval(arg)?;
                print!("{}", *arg.borrow());
            }
        },
        Println => {
            for arg in args {
                let arg = context.eval(arg)?;
                print!("{}", *arg.borrow());
            }
            println!();
        },
        Dbg => {
            for arg in args {
                let arg = context.eval(arg)?;
                print!("{:?}", *arg.borrow());
            }
            println!();
        },
        ReadFile => {
            let args = get_args(context, tok, 1, args)?;
            let arg = args[0].borrow().clone();

            let file_path = match arg {
                Literal::String(str) => str,
                _ => return Err(InterpreterError::ValueError(tok.clone(), format!("File path must be String not {}!", arg.get_type()))),
            };

            let text = fs::read_to_string(file_path)
                .map_err(|err| InterpreterError::BuiltInError(tok.clone(), format!("{}", err)))?;
            
            return Ok(mut_rc(Literal::String(text)));
        },
        WriteFile => {
            let args = get_args(context, tok, 2, args)?;
            let file_name = args[0].borrow().clone();
            let content = args[1].borrow().clone();
            
            let file_path = match &file_name {
                Literal::String(str) => str,
                _ => return Err(InterpreterError::ValueError(tok.clone(), format!("File path must be String not {}!", file_name.get_type()))),
            };
            fs::write(file_path, content.to_string())
                .map_err(|err| InterpreterError::BuiltInError(tok.clone(), format!("{}", err)))?;
        },
        AppendFile => todo!(),
        Len => {
            let args = get_args(context, tok, 1, args)?;
            let container = args[0].borrow().clone();
            return Ok(mut_rc((match container {
                Literal::String(s) => s.len(),
                Literal::List(l) => l.len(),
                lit => return Err(InterpreterError::ValueError(tok.clone(), format!("Can't get length of {}!", lit.get_type()))),
            } as i64).into()));
        },
        Import => {
            let args = get_args(context, tok, 1, args)?;
            let file_path = args[0].borrow().clone();
            match file_path {
                Literal::String(file_path) => {
                    let complete_path = context.search_file_in_path(&file_path)?;
                    let file_content = fs::read_to_string(&complete_path)
                        .map_err(|err| InterpreterError::BuiltInError(tok.clone(), format!("{}", err)))?;

                    context.import(&file_content, complete_path.clone())?;
                },
                _ => return Err(InterpreterError::ValueError(tok.clone(), format!("File path must be String not {}!", file_path.get_type()))),
            }
        },
        ToString => {
            let args = get_args(context, tok, 1, args)?;
            let arg = args[0].borrow().clone();
            return Ok(mut_rc(Literal::String(arg.to_string())));
        },
        ParseInt => {
            let args = get_args(context, tok, 1, args)?;
            let arg = args[0].borrow().clone();
            return match arg {
                Literal::String(str) => {
                    match str.parse::<i64>() {
                        Ok(int) => Ok(mut_rc(Literal::Int(int))),
                        Err(err) => Err(InterpreterError::ValueError(tok.clone(), format!("{} is not a valid integer! Reason: {}", str, err))),
                    }
                },
                Literal::Char(c) => {
                    Ok(mut_rc(Literal::Int(c as i64)))
                },
                _ => Err(InterpreterError::ValueError(tok.clone(), format!("Expected String got {}!", arg.get_type())))
            };
        },
        Floor => {
            let args = get_args(context, tok, 1, args)?;
            let arg = args[0].borrow().clone();
            return match arg {
                Literal::Float(f) => {
                    Ok(mut_rc((f.floor() as i64).into()))
                },
                _ => Err(InterpreterError::ValueError(tok.clone(), format!("Expected String got {}!", arg.get_type())))
            };
        },
        Ceil => {
            let args = get_args(context, tok, 1, args)?;
            let arg = args[0].borrow().clone();
            return match arg {
                Literal::Float(f) => {
                    Ok(mut_rc((f.ceil() as i64).into()))
                },
                _ => Err(InterpreterError::ValueError(tok.clone(), format!("Expected String got {}!", arg.get_type())))
            };
        },
        _ => unimplemented!("BuiltIn {:?} not implemented yet", tok.kind),
    }

    Ok(mut_rc(Literal::Unit))
}

fn exec_func_decl(context: &mut InterpreterContext, func_name: &Token, args: &Vec<Token>, code: &Block) -> InterpreterResult {
    context.func_decls.push(Func::new(func_name, args, code));
    Ok(())
}

fn exec_record_decl(context: &mut InterpreterContext, record_name: &Token, fields: &Vec<Token>) -> InterpreterResult {
    context.record_decls.push(Record::new(record_name, fields));
    Ok(())
}

fn exec_expr(context: &mut InterpreterContext, expr: &Expr) -> InterpreterResult {
    let _ = context.eval(expr)?;
    Ok(())
}

fn exec_record_field_deref(context: &mut InterpreterContext, record_tok: &Token, field_toks: &Vec<Token>) -> Result<MutRc<Literal>, InterpreterError> {
    let value = context.get_var_value(record_tok)?;
    context.record_field_defef(record_tok,value, field_toks, 0)
}

fn exec_block(context: &mut InterpreterContext, block: &Block) -> Result<MutRc<Literal>, InterpreterError> {
    for statment in block.0.iter() {
        match statment {
            Statement::Reassign(reassign) => exec_reassign(context, reassign)?,
            Statement::ListReassign(tok, idx_expr, val_expr) => exec_list_reassign(context, tok, idx_expr, val_expr)?,
            Statement::ConstAssign(assigns) => exec_const_assign(context, assigns)?,
            Statement::VarDecl(decls) => exec_var_decl(context, decls)?,
            Statement::If(cond, if_block, else_block) => {
                let maybe_return_val = exec_if(context, cond, if_block, else_block.as_ref())?;
                if context.is_returning() || context.is_breaking() || context.is_continuing() {
                    return Ok(maybe_return_val);
                }
            },
            Statement::While(cond, block) => {
                let maybe_return_val = exec_while(context, cond, block)?;
                if context.is_returning() {
                    return Ok(maybe_return_val);
                }
            },
            Statement::For(for_statement) => {
                let maybe_return_val = exec_for(context, for_statement)?;
                if context.is_returning() {
                    return Ok(maybe_return_val);
                }
            },
            Statement::FuncDecl(func_name, args, code) => exec_func_decl(context, func_name, args, code)?,
            Statement::RecordDecl(record_name, fields) => exec_record_decl(context, record_name, fields)?,
            Statement::Expr(expr) => exec_expr(context, expr)?,
            Statement::Return(expr) => {
                context.start_returning();
                return Ok(context.eval(expr)?);
            },
            Statement::Break(_tok) => {
                context.set_breaking(true);
                return Ok(mut_rc(Literal::Unit));
            },
            Statement::Continue(_tok) => {
                context.set_continuing(true);
                return Ok(mut_rc(Literal::Unit));
            },
        }
    }

    Ok(mut_rc(Literal::Unit))
}

pub fn interprete_ast(root_node: Program, include_path: Vec<String>) -> (InterpreterResult, InterpreterContext) {
    let mut context = InterpreterContext::new(include_path);

    for block in root_node.0 {
        match exec_block(&mut context, &block) {
            Ok(_) => {},
            Err(err) => return (Err(err), context),
        }
    }

    (Ok(()), context)
}
