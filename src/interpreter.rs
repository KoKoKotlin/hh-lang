use std::{rc::Rc, cell::RefCell, borrow::BorrowMut};
use crate::{ast::{Program, Literal, Block, Statement, Expr}, tokenizer::{Token, TokenKind}};

pub type MutRc<T> = Rc<RefCell<T>>;

pub fn mut_rc<T>(t: T) -> MutRc<T> {
    Rc::new(RefCell::new(t))
}

#[derive(Debug)]
struct Scope {
    names: Vec<Name>,
}

impl Scope {
    fn new(init: Option<Vec<(&Token, MutRc<Literal>)>>) -> Self {
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
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct InterpreterContext {
    record_decls: Vec<Record>,
    func_decls: Vec<Func>,
    global_scope: Scope,
    scope_stack: Vec<Scope>,
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
}

pub type InterpreterResult = Result<(), InterpreterError>;

impl InterpreterContext {
    pub fn new() -> Self {
        Self { global_scope: Scope::new(None), record_decls: vec![], func_decls: vec![], scope_stack: vec![] }
    }

    fn create_var(&mut self, tok: &Token, lit: Option<MutRc<Literal>>) {
        let scope = if let Some(local_scope) = self.scope_stack.last_mut() {
            local_scope
        } else {
            &mut self.global_scope
        };

        scope.push(Name::make_var(&tok.symbols, lit));
    }

    fn create_const(&mut self, tok: &Token, lit: MutRc<Literal>) {
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

    fn get_var_value(&self, tok: &Token) -> Result<MutRc<Literal>, InterpreterError> {
        if !self.name_exists(tok) { return Err(InterpreterError::VariableDoesNotExists(tok.clone())) };

        if let Some(scope) = self.scope_stack.last() {
            scope.get_var_value(tok)
        } else {
            self.global_scope.get_var_value(tok)
        }
    }

    fn index_var(&mut self, tok: &Token, idx_expr: &Expr) -> Result<MutRc<Literal>, InterpreterError> {
        let name_lit = self.get_var_value(tok)?;
        let name_lit = name_lit.borrow();
        self.index(tok, &name_lit, idx_expr)
    }

    fn index(&mut self, tok: &Token, lit: &Literal, idx_expr: &Expr) -> Result<MutRc<Literal>, InterpreterError> {
        if let Literal::Number(idx) = *self.eval(idx_expr)?.borrow() {
            if idx < 0 {
                return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
            }

            match lit {
                Literal::String(_) => return Err(InterpreterError::TypeError(tok.clone(), "String cannot be indexed".to_string())),
                Literal::Number(_) => return Err(InterpreterError::TypeError(tok.clone(), "Number cannot be indexed".to_string())),
                Literal::True => return Err(InterpreterError::TypeError(tok.clone(), "Bool cannot be indexed".to_string())),
                Literal::False => return Err(InterpreterError::TypeError(tok.clone(), "Bool cannot be indexed".to_string())),
                Literal::Unit => return Err(InterpreterError::TypeError(tok.clone(), "Unit cannot be indexed".to_string())),
                Literal::RecordInstance(_, _) => return Err(InterpreterError::TypeError(tok.clone(), "Record instance cannot be indexed".to_string())),
                Literal::List(values) => {
                    if idx as usize >= values.len() {
                        return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                    }

                    return Ok(values[idx as usize].clone());
                },
            }
        } else {
            return Err(InterpreterError::TypeError(tok.clone(), "Indices must be Number".to_string()));
        }
    }

    fn assign_indexed(&mut self, tok: &Token, idx_expr: &Expr, val_expr: &Expr) -> InterpreterResult {
        let idx_lit = self.eval(idx_expr)?.borrow().clone();
        let result = self.eval(val_expr)?;

        if let Some(name) = self.get_name(tok) {
            if let Literal::Number(idx) = idx_lit {
                if idx < 0 {
                    return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                }

                if name.value.is_none() {
                    return Err(InterpreterError::VariableNotInitialized(tok.clone()));
                }
                
                let value = name.value.as_ref().unwrap().clone();
                match &*value.borrow() {
                    Literal::String(_) => return Err(InterpreterError::TypeError(tok.clone(), "String cannot be indexed".to_string())),
                    Literal::Number(_) => return Err(InterpreterError::TypeError(tok.clone(), "Number cannot be indexed".to_string())),
                    Literal::True => return Err(InterpreterError::TypeError(tok.clone(), "Bool cannot be indexed".to_string())),
                    Literal::False => return Err(InterpreterError::TypeError(tok.clone(), "Bool cannot be indexed".to_string())),
                    Literal::Unit => return Err(InterpreterError::TypeError(tok.clone(), "Unit cannot be indexed".to_string())),
                    Literal::RecordInstance(_, _) => return Err(InterpreterError::TypeError(tok.clone(), "Record instance cannot be indexed".to_string())),
                    Literal::List(values) => {
                        if idx as usize >= values.len() {
                            return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
                        }

                        values[idx as usize].replace(result.borrow().clone());
                        return Ok(());
                    },
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
                let right = self.eval(&right)?;
                apply_op(left, op, right).map(|lit| mut_rc(lit))
            },
            Expr::Unary(op, operand) => {
                let operand = self.eval(&operand)?;
                apply_unary(op, operand).map(|lit| mut_rc(lit))
            }
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Ident(tok) => {
                self.get_var_value(tok)
            },
            Expr::IdentIndexed(tok, idx_expr) => self.index_var(tok, idx_expr),
            Expr::BuiltInCall(tok, args) => exec_built_in(self, tok, args),
            Expr::FuncCall(func_name, args_exprs) => exec_func_call(self, func_name, args_exprs),
            Expr::ListInstantiation(tok, capacity, init_exprs) => exec_list_instantiation(self, tok, capacity, init_exprs),
            Expr::RecordInstantiation(record_name, field_exprs) => exec_record_instantiation(self, record_name, field_exprs),
            Expr::RecordFieldDeref(record_tok, field_toks) => exec_record_field_deref(self, record_tok, field_toks),
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

            let func_scope = Scope::new(Some(func.args.iter().zip(args_lits.into_iter()).collect()));
            self.scope_stack.push(func_scope);
            let return_val = exec_block(self, &func.code)?;
            let _ = self.scope_stack.pop();
            return Ok(return_val);
        } else {
            return Err(InterpreterError::FunctionNotDeclared(func_name.clone()));
        }
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
}

fn apply_unary(op: &Token, operand: MutRc<Literal>) -> Result<Literal, InterpreterError> {
    use TokenKind::*;

    // TODO for better performance don't clone
    let operand = operand.borrow().clone();

    return match op.kind {
        Minus => match &operand {
            Literal::Number(n) => Ok(Literal::Number(-n)),
            _ => Err(InterpreterError::UndefinedUnaryOperation(op.clone(), operand.clone())),
        },
        Bang => match &operand {
            Literal::True => Ok(Literal::False),
            Literal::False => Ok(Literal::True),
            _ => Err(InterpreterError::UndefinedUnaryOperation(op.clone(), operand.clone())),
        },
        _ => unimplemented!(),
    }
}

fn apply_op(left: MutRc<Literal>, op: &Token, right: MutRc<Literal>) -> Result<Literal, InterpreterError> {
    use TokenKind::*;

    // TODO for better performance don't clone
    let left = left.borrow().clone();
    let right = right.borrow().clone();

    return match op.kind {
        Plus => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left + right)),
                
                (Literal::String(left), Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
                (Literal::String(left), _) => Ok(Literal::String(format!("{}{}", left, right))),
                (_, Literal::String(right)) => Ok(Literal::String(format!("{}{}", left, right))),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Minus => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left - right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Times => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left * right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Div => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) if *right != 0 => Ok(Literal::Number(left / right)),
                (Literal::Number(_), Literal::Number(right)) if *right == 0 => Err(InterpreterError::DivisionByZero(op.clone())),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        And => {
            match (&left, &right) {
                (Literal::True, Literal::True) => Ok(Literal::True),
                (Literal::False, Literal::True) => Ok(Literal::False),
                (Literal::True, Literal::False) => Ok(Literal::False),
                (Literal::False, Literal::False) => Ok(Literal::False),
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left & right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Or => {
            match (&left, &right) {
                (Literal::True, Literal::True) => Ok(Literal::True),
                (Literal::False, Literal::True) => Ok(Literal::True),
                (Literal::True, Literal::False) => Ok(Literal::True),
                (Literal::False, Literal::False) => Ok(Literal::False),
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left | right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        Xor => {
            match (&left, &right) {
                (Literal::True, Literal::True) => Ok(Literal::False),
                (Literal::False, Literal::True) => Ok(Literal::True),
                (Literal::True, Literal::False) => Ok(Literal::True),
                (Literal::False, Literal::False) => Ok(Literal::False),
                (Literal::Number(left), Literal::Number(right)) => Ok(Literal::Number(left ^ right)),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        LessThan => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok((left < right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        GreaterThan => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok((left > right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        LessOrEqual => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok((left <= right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        GreaterOrEqual => {
            match (&left, &right) {
                (Literal::Number(left), Literal::Number(right)) => Ok((left >= right).into()),
                _ => Err(InterpreterError::UndefinedBinOperation(left.clone(), op.clone(), right.clone())),
            }
        },
        EqualsEquals => {
            match (&left, &right) {
                (lit1, lit2) => Ok((lit1 == lit2).into())
            }
        },
        NotEqual => {
            match (&left, &right) {
                (lit1, lit2) => Ok((lit1 != lit2).into())
            }
        }
        _ => unimplemented!(),
    }
}

fn exec_reassign(context: &mut InterpreterContext, tok: &Token, field_toks: &Vec<Token>, expr: &Expr) -> InterpreterResult {
    if !context.name_exists(tok) {
        return Err(InterpreterError::VariableDoesNotExists(tok.clone()));
    }

    let result = context.eval(expr);
    let name = context.get_name(tok).unwrap();

    if name.is_const() {
        return Err(InterpreterError::ConstantReassign(tok.clone()));
    }

    if field_toks.len() == 0 {
        name.value = Some(result?);
    } else {
        if let Some(record_lit) = name.value.clone() {
            let record_deref = context.record_field_defef(tok, record_lit, field_toks, 0)?;
            record_deref.replace(result?.borrow().clone());
        } else {
            return Err(InterpreterError::VariableNotInitialized(tok.clone()));
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

fn exec_var_decl(context: &mut InterpreterContext, decls: &Vec<(Token, Option<Literal>)>) -> InterpreterResult {
    for (tok, lit) in decls {
        let lits = lit.clone().map(|lit| mut_rc(lit));
        context.create_var(tok, lits);
    }

    Ok(())
}

fn exec_list_instantiation(context: &mut InterpreterContext, tok: &Token, capacity_expr: &Expr, init_exprs: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
    let capacity = match *context.eval(capacity_expr)?.borrow() {
        Literal::String(_) => return Err(InterpreterError::TypeError(tok.clone(), "String cannot be used as a list".to_string())),
        Literal::Number(num) => {
            if num < 0 {
                return Err(InterpreterError::ValueError(tok.clone(), "Number cannot be negative".to_string()));
            }

            num as usize
        },
        Literal::True => return Err(InterpreterError::TypeError(tok.clone(), "Bool cannot be used as a list".to_string())),
        Literal::False => return Err(InterpreterError::TypeError(tok.clone(), "Bool cannot be used as a list".to_string())),
        Literal::Unit => return Err(InterpreterError::TypeError(tok.clone(), "Unit cannot be used as a list".to_string())),
        Literal::List(_) => return Err(InterpreterError::TypeError(tok.clone(), "List cannot be used as a list".to_string())),
        Literal::RecordInstance(_, _) => return Err(InterpreterError::TypeError(tok.clone(), "Record instance cannot be used as a list".to_string())),
    };

    let mut value = Vec::with_capacity(capacity);
    if capacity < init_exprs.len() {
        return Err(InterpreterError::IndexOutOfBounds(tok.clone()));
    }
    
    let init_lits: Result<Vec<MutRc<Literal>>, InterpreterError> = init_exprs.iter().map(|expr| context.eval(expr)).collect();
    let init_lits = init_lits?;

    value.extend(init_lits);
    for _ in 0..(capacity - init_exprs.len()) {
        value.push(mut_rc(Literal::Number(0)));
    }
    
    Ok(mut_rc(Literal::List(value)))
}

fn exec_if(context: &mut InterpreterContext, cond: &Expr, if_block: &Block, else_block: Option<&Block>) -> InterpreterResult {
    let cond = context.eval(cond)?;
    let cond = cond.borrow();

    if cond.is_truthy() {
        exec_block(context, if_block)?;
    } else if let Some(else_block) = else_block {
        exec_block(context, else_block)?;
    }

    Ok(())
}

fn exec_while(context: &mut InterpreterContext, cond_expr: &Expr, block: &Block) -> InterpreterResult {

    let mut cond = context.eval(cond_expr)?;
    while (*cond.borrow()).is_truthy() {
        exec_block(context, block)?;
        cond = context.eval(cond_expr)?;
    }

    Ok(())
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
        }
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

fn exec_func_call(context: &mut InterpreterContext, func_name: &Token, args_exprs: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
    context.exec_func(func_name, args_exprs)
}

fn exec_expr(context: &mut InterpreterContext, expr: &Expr) -> InterpreterResult {
    let _ = context.eval(expr)?;
    Ok(())
}

fn exec_record_instantiation(context: &mut InterpreterContext, record_name: &Token, field_values: &Vec<Expr>) -> Result<MutRc<Literal>, InterpreterError> {
    context.instance_record(record_name, field_values)
}

fn exec_record_field_deref(context: &mut InterpreterContext, record_tok: &Token, field_toks: &Vec<Token>) -> Result<MutRc<Literal>, InterpreterError> {
    let value = context.get_var_value(record_tok)?;
    context.record_field_defef(record_tok,value, field_toks, 0)
}

fn exec_block(context: &mut InterpreterContext, block: &Block) -> Result<MutRc<Literal>, InterpreterError> {
    for statment in block.0.iter() {
        match statment {
            Statement::Reassign(tok, field_toks, expr) => exec_reassign(context, tok, field_toks, expr)?,
            Statement::ListReassign(tok, idx_expr, val_expr) => exec_list_reassign(context, tok, idx_expr, val_expr)?,
            Statement::ConstAssign(assigns) => exec_const_assign(context, assigns)?,
            Statement::VarDecl(decls) => exec_var_decl(context, decls)?,
            Statement::If(cond, if_block, else_block) => exec_if(context, cond, if_block, else_block.as_ref())?,
            Statement::While(cond, block) => exec_while(context, cond, block)?,
            Statement::FuncDecl(func_name, args, code) => exec_func_decl(context, func_name, args, code)?,
            Statement::RecordDecl(record_name, fields) => exec_record_decl(context, record_name, fields)?,
            Statement::Expr(expr) => exec_expr(context, expr)?,
            Statement::Return(expr) => {
                return Ok(context.eval(expr)?);
            },
        }
    }

    Ok(mut_rc(Literal::Unit))
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