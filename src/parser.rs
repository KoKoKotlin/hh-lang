use std::num::ParseIntError;

use crate::{tokenizer::{Tokenizer, TokenKind, Token}, ast::{Expr, Literal, Block, Statement, Program}, interpreter::mut_rc};

type IntParser = Box<dyn Fn(&str) -> Result<i64, ParseIntError>>;

pub struct Parser {
    tokenizer: Tokenizer,
    look_ahead: Option<Token>,
}

enum ParserError { 
    WrongTokenkind
}

const UNARY_OPERATORS: [TokenKind; 2] = [
    TokenKind::Minus,
    TokenKind::Bang,
];

const COMPARISON_OPERATORS: [TokenKind; 6] = [
    TokenKind::LessThan,
    TokenKind::GreaterThan,
    TokenKind::LessOrEqual,
    TokenKind::GreaterOrEqual,
    TokenKind::EqualsEquals,
    TokenKind::NotEqual,
];

const LOGICAL_OPERATOR: [TokenKind; 3] = [
    TokenKind::And,
    TokenKind::Or,
    TokenKind::Xor,
];

const ADDITIVE_OPERATORS: [TokenKind; 2] = [
    TokenKind::Plus,
    TokenKind::Minus,
];

const MULITPLICATIVE_OPERATORS: [TokenKind; 2] = [
    TokenKind::Times,
    TokenKind::Div,
];

const BUILT_INS: [TokenKind; 6] = [
    TokenKind::Print,
    TokenKind::Println,
    TokenKind::Dbg,
    TokenKind::WriteFile,
    TokenKind::ReadFile,
    TokenKind::AppendFile,
];

impl Parser {
    pub fn new(source_code: &str) -> Self {
        let mut tokenizer = Tokenizer::new(source_code.to_owned());
        let look_ahead = tokenizer.next_token();
        Self { 
            tokenizer,
            look_ahead,
        }
    }

    pub fn parse(&mut self) -> Result<Program, ()> {
        self.program()
    }

    fn program(&mut self) -> Result<Program, ()> {
        let mut blocks: Vec<Block> = vec![];

        while self.look_ahead.is_some() {
            blocks.push(self.block()?);
        }
        Ok(Program(blocks))
    }

    fn block(&mut self) -> Result<Block, ()> {
        use TokenKind::*;
        let mut statements: Vec<Statement> = vec![];
        
        while let Some(head) = self.peek() {
            statements.push(match head {
                Let => self.let_binding()?,
                Var => self.var_decl()?,
                Ident => self.reassign()?,
                If => self.fi()?,
                While => self.elihw()?,
                Func => self.func()?,
                Record => self.record()?,
                Return => self.return_()?,
                End | Else => { break; }
                // if all else fails try to parse the next statement as a standalone expression
                _ => {
                    let expr = self.expr()?;
                    let _ = self.consume(&[Semicolon]).ok_or(());
                    Statement::Expr(expr)
                },
            });
        }

        Ok(Block(statements))
    }

    fn built_in(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;
        let call_tok = self.consume(&BUILT_INS).ok_or(())?;
        let args = self.expr_list(Some(Comma), Semicolon)?;

        return Ok(Expr::BuiltInCall(call_tok, args));
    }

    fn fi(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;

        self.consume(&[If]).ok_or(())?;
        let condition = self.expr()?;
        self.consume(&[Then]).ok_or(())?;
        let if_block = self.block()?;
        
        let mut else_block = None;
        if let Some(kind) = self.peek() {
            if kind == Else {
                self.consume(&[Else]).ok_or(())?;
                else_block = Some(self.block()?);
            }
        }

        self.consume(&[End]).ok_or(())?;
        return Ok(Statement::If(condition, if_block, else_block));
    }

    fn elihw(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;

        self.consume(&[While]).ok_or(())?;
        let condition = self.expr()?;
        self.consume(&[Do]).ok_or(())?;
        let block = self.block()?;
        self.consume(&[End]).ok_or(())?;
        
        return Ok(Statement::While(condition, block));
    }

    fn return_(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;

        self.consume(&[Return]).ok_or(())?;
        let return_expr = self.expr()?;
        self.consume(&[Semicolon]).ok_or(())?;

        return Ok(Statement::Return(return_expr));
    }


    fn func(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;

        self.consume(&[Func]).ok_or(())?;
        let func_name = self.consume(&[Ident]).ok_or(())?;
        let args_list = self.consume_list(Ident, None, Start).ok_or(())?;

        self.consume(&[Start]).ok_or(())?;
        let code = self.block()?;
        self.consume(&[End]).ok_or(())?;
        
        Ok(Statement::FuncDecl(func_name, args_list, code))
    }

    fn record(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;

        self.consume(&[Record]).ok_or(())?;
        let record_name = self.consume(&[Ident]).ok_or(())?;
        self.consume(&[Start]).ok_or(())?;
        let fields = self.consume_list(Ident, None, End).ok_or(())?;
        self.consume(&[End]).ok_or(())?;
        
        return Ok(Statement::RecordDecl(record_name, fields));
    }

    fn call(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;
        
        self.consume(&[Call]).ok_or(())?;
        let func_name = self.consume(&[Ident]).ok_or(())?;
        let args_exprs = self.expr_list(None, Semicolon)?;

        Ok(Expr::FuncCall(func_name, args_exprs))
    }

    fn let_binding(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;
        
        let mut let_bindings: Vec<(Token, Literal)> = vec![];
        self.consume(&[Let]).ok_or(())?;
        loop {
            let ident = self.consume(&[Ident]).ok_or(())?;
            self.consume(&[Equals]).ok_or(())?;
            let value = self.literal()?;

            let_bindings.push((ident, value));

            match self.peek() {
                Some(kind) if kind == Comma => self.consume(&[Comma]).ok_or(())?,
                _ => break,
            };
        }
        
        self.consume(&[Semicolon]).ok_or(())?; 
        Ok(Statement::ConstAssign(let_bindings))
    }
     
    fn var_decl(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;
        
        let mut var_decls: Vec<(Token, Option<Literal>)> = vec![];
        self.consume(&[Var]).ok_or(())?;
        
        loop {
            let ident = self.consume(&[Ident]).ok_or(())?;
            let literal = match self.look_ahead.as_ref() {
                Some(token) => if token.kind == Equals {
                    self.consume(&[Equals]).ok_or(())?;
                    let literal = self.literal()?;
                    Some(literal)
                } else {
                    None
                },
                None => {
                    consume_error(self, None, &[Equals, Semicolon]);
                    return Err(());
                },
            };

            var_decls.push((ident, literal));

            match self.peek() {
                Some(kind) if kind == Comma => self.consume(&[Comma]).ok_or(())?,
                _ => break,
            };
        }

        self.consume(&[Semicolon]).ok_or(())?; 
        return Ok(Statement::VarDecl(var_decls));
    }

    fn reassign(&mut self) -> Result<Statement, ()> {
        use TokenKind::*;
        let ident = self.consume(&[Ident]).ok_or(())?;

        match self.peek() {
            Some(OpeningBracket) => {
                self.consume(&[OpeningBracket]).ok_or(())?;
                let index = self.expr()?;
                self.consume(&[ClosingBracket]).ok_or(())?;
                self.consume(&[Equals]).ok_or(())?;
                let expr = self.expr()?;
                self.consume(&[Semicolon]).ok_or(())?;

                return Ok(Statement::ListReassign(ident, index, expr));
            },
            Some(Dot) => {
                let mut idents = vec![];                        
                while Some(Dot) == self.peek() {
                    self.consume(&[Dot]).ok_or(())?;
                    idents.push(self.consume(&[Ident]).ok_or(())?);
                }

                self.consume(&[Equals]).ok_or(())?;
                let expr = self.expr()?;
                self.consume(&[Semicolon]).ok_or(())?;

                return Ok(Statement::Reassign(ident, idents, expr));
            }
            Some(Equals) => {
                self.consume(&[Equals]).ok_or(())?;
                let expr = self.expr()?;
                self.consume(&[Semicolon]).ok_or(())?;
                
                Ok(Statement::Reassign(ident, vec![], expr))
            },
            _ => {
                let tok = self.get();
                consume_error(self, tok.as_ref(), &[Equals, OpeningBracket]);
                Err(())
            },
        }

    }

    fn list(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;
        let start_token = self.consume(&[OpeningBracket]).ok_or(())?;
        let capacity_expr = self.expr()?;
        self.consume(&[ClosingBracket]).ok_or(())?;

        
        let init_exprs = if let Some(OpeningParan) = self.peek() {
            self.consume(&[OpeningParan]).ok_or(())?;
            let exprs = self.expr_list(Some(Comma), ClosingParan)?;
            self.consume(&[ClosingParan]).ok_or(())?;
            exprs
        } else {
            vec![]
        };

        Ok(Expr::ListInstantiation(start_token, Box::new(capacity_expr), init_exprs))
    }

    fn expr(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;
        
        match self.peek() {
            Some(Call) => return self.call(),
            Some(New) => return self.new_record(),
            Some(OpeningBracket) => return self.list(),
            Some(kind) if BUILT_INS.contains(&kind) => return self.built_in(),
            _ => self.logic_expr(),
        }
    }

    fn logic_expr(&mut self) -> Result<Expr, ()> {
        let left = self.comp_expr()?;

        match self.peek() {
            Some(kind) if LOGICAL_OPERATOR.contains(&kind) => {
                let operand = self.consume(&LOGICAL_OPERATOR).ok_or(())?;
                let right = self.comp_expr()?;
                
                return Ok(Expr::Binary(Box::new(left), operand, Box::new(right)));
            }
            _ => {
                return Ok(left);
            },
        }
    }

    fn comp_expr(&mut self) -> Result<Expr, ()> {
        let left = self.add_expr()?;

        match self.peek() {
            Some(kind) if COMPARISON_OPERATORS.contains(&kind) => {
                let operand = self.consume(&COMPARISON_OPERATORS).ok_or(())?;
                let right = self.add_expr()?;
                
                return Ok(Expr::Binary(Box::new(left), operand, Box::new(right)));
            }
            _ => {
                return Ok(left);
            },
        }
    }

    fn add_expr(&mut self) -> Result<Expr, ()> {
        let left = self.mult_expr()?;

        match self.peek() {
            Some(kind) if ADDITIVE_OPERATORS.contains(&kind) => {
                let operand = self.consume(&ADDITIVE_OPERATORS).ok_or(())?;
                let right = self.mult_expr()?;
                
                return Ok(Expr::Binary(Box::new(left), operand, Box::new(right)));
            }
            _ => {
                return Ok(left);
            },
        }
    }

    fn mult_expr(&mut self) -> Result<Expr, ()> {
        let left = self.factor()?;

        match self.peek() {
            Some(kind) if MULITPLICATIVE_OPERATORS.contains(&kind) => {
                let operand = self.consume(&MULITPLICATIVE_OPERATORS).ok_or(())?;
                let right = self.factor()?;
                
                return Ok(Expr::Binary(Box::new(left), operand, Box::new(right)));
            }
            _ => {
                return Ok(left);
            },
        }
    }

    fn factor(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;
        match self.peek() {
            Some(OpeningParan) => {
                self.consume(&[OpeningParan]).ok_or(())?;
                let expr = self.expr()?;
                self.consume(&[ClosingParan]).ok_or(())?;
                
                return Ok(expr);
            },
            Some(Int) | Some(Float) | Some(True) | Some(False) | Some(String) => {
                let lit = self.literal()?;
                return Ok(Expr::Literal(mut_rc(lit)));
            },
            Some(Ident) => {
                let ident = self.consume(&[Ident]).ok_or(())?;

                match self.peek() {
                    Some(OpeningBracket) => {
                        self.consume(&[OpeningBracket]).ok_or(())?;
                        let index = self.expr()?;
                        self.consume(&[ClosingBracket]).ok_or(())?;
                        
                        return Ok(Expr::IdentIndexed(ident, Box::new(index)));
                    },
                    Some(Dot) => {
                        let mut idents = vec![];                        
                        while Some(Dot) == self.peek() {
                            self.consume(&[Dot]).ok_or(())?;
                            idents.push(self.consume(&[Ident]).ok_or(())?);
                        }

                        return Ok(Expr::RecordFieldDeref(ident, idents));
                    }
                    _ => {}
                }
                
                return Ok(Expr::Ident(ident));
            },
            Some(kind) if UNARY_OPERATORS.contains(&kind) => {
                let op = self.consume(&UNARY_OPERATORS).ok_or(())?;
                let factor = self.factor()?;
                return Ok(Expr::Unary(op, Box::new(factor)));
            },
            _ => {
                let next = self.get();
                consume_error(self, next.as_ref(), &[Int, Float, String, Ident, OpeningParan]);
                return Err(());
            },
        }
    }

    fn literal(&mut self) -> Result<Literal, ()> {
        use TokenKind::*;

        let int_parsers: Vec<IntParser> = vec![
            Box::new(|s: &str| i64::from_str_radix(&s, 10)),
            Box::new(|s: &str| i64::from_str_radix(&s.replace("0b", ""), 2)),
            Box::new(|s: &str| i64::from_str_radix(&s.replace("0c", ""), 8)),
            Box::new(|s: &str| i64::from_str_radix(&s.replace("0x", ""), 16)),
        ];

        let token = self.consume(&[Int, Float, String, True, False]).ok_or(())?;
        Ok(match token.kind {
            Int => {
                let value = int_parsers
                    .iter()
                    .map(|parser| parser(&token.symbols))
                    .find(|res| res.is_ok())
                    .unwrap() // safe because the tokenizer only produces valid int strings that fit one parser 
                    .unwrap();
                Literal::Int(value)
            },
            Float => Literal::Float(token.symbols.parse::<f64>().unwrap()),
            String => Literal::String(token.symbols),
            True   => Literal::True,
            False  => Literal::False,
            _ => unreachable!(),
        })
    }

    fn new_record(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;

        self.consume(&[New]).ok_or(())?;
        let record_name = self.consume(&[Ident]).ok_or(())?;        
        self.consume(&[OpeningParan]).ok_or(())?;
        let exprs = self.expr_list(Some(Comma), ClosingParan)?;
        self.consume(&[ClosingParan]).ok_or(())?;

        Ok(Expr::RecordInstantiation(record_name, exprs))        
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.look_ahead.as_ref().map(|t| t.kind)
    }

    fn consume(&mut self, token_kinds: &[TokenKind]) -> Option<Token> {
        let res = match self.look_ahead.as_ref() {
            Some(token) => {
                if token_kinds.contains(&token.kind) {
                    Some(token.clone())
                } else {
                    consume_error(self, Some(token), &token_kinds);
                    None
                }
            },
            None => {
                consume_error(self, None, &token_kinds);
                None
            },
        };
        self.look_ahead = self.tokenizer.next_token();
        
        res
    }

    fn expr_list(&mut self, sep: Option<TokenKind>, end: TokenKind) -> Result<Vec<Expr>, ()> {
        let mut exprs = vec![];
        while let Some(kind) = self.peek() {
            if kind == end { break; }
            exprs.push(self.expr()?);
 
            if let Some(sep) = sep {
                if Some(end) == self.peek() { break; }
                self.consume(&[sep]).ok_or(())?;
            }
        }

        Ok(exprs)
    }

    fn consume_list(&mut self, token_kind: TokenKind, sep_kind: Option<TokenKind>, end: TokenKind) -> Option<Vec<Token>> {
        let mut tokens = vec![];

        while let Some(kind) = self.peek() {
            if kind == end { break; }
            tokens.push(self.consume(&[token_kind])?);
            
            if let Some(sep_kind) = sep_kind {
                if Some(end) == self.peek() { break; }
                self.consume(&[sep_kind])?;
            }
        }

        Some(tokens)
    }

    fn get(&mut self) -> Option<Token> {
        self.tokenizer.next_token()
    }

    pub fn get_context(&self, tok: &Token) -> String {
        self.tokenizer.get_context(tok.index)
    }
}

fn consume_error(parser: &Parser, token: Option<&Token>, token_kinds: &[TokenKind]) {
    let token_desc = if token_kinds.len() == 1 {
        format!("{}", token_kinds[0])
    } else {
        let mut builder = String::new();
        for (idx, kind) in token_kinds.iter().enumerate() {
            builder.push_str(&format!("{}", kind));

            if idx != token_kinds.len() - 1 {
                builder.push_str(", ");
            }
        }

        builder
    };

    match token {
        Some(t) => eprintln!("Expected {} but got {} at {}.\nContext:\n{}", token_desc, t, t.loc, parser.tokenizer.get_context(t.index)),
        None => eprintln!("Expected {} but no tokens are left!", token_desc),
    }
}