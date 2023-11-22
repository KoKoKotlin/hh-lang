use crate::{tokenizer::{Tokenizer, TokenKind, Token}, ast::{Expr, Literal, Block, Statement, Program}};

pub struct Parser {
    tokenizer: Tokenizer,
    look_ahead: Option<Token>,
}

enum ParserError { 
    WrongTokenkind
}

const UNARY_OPERATORS: [TokenKind; 3] = [
    TokenKind::Plus,
    TokenKind::Minus,
    TokenKind::Bang,
];

const ADDITIVE_OPERATORS: [TokenKind; 2] = [
    TokenKind::Plus,
    TokenKind::Minus,
];

const MULITPLICATIVE_OPERATORS: [TokenKind; 1] = [
    TokenKind::Times,
];

/** Current Grammar:
 * Program   => Block == Vec<Statment>
 * Block     => [ "let" IDENT "=" Literal {, IDENT "=" Literal } ";" ]
 *              [ "var" IDENT { "=" Literal } {, IDENT {"=" Literal}} ";" ]
 *              Statement
 * Statement => IDENT "=" Expr ";"
 *              "if" Expr "then" Block {"else" Block} "end"
 *              "while" Expr "then" Block "end"
 * Expr      => {UN_OP} Term { ADD_OP term } ";"
 * Term      => Factor { MULT_OP Factor } ";"
 * Factor    => IDENT | NUMBER | "(" Expr ")"
 * Literal   => NUMBER | STRING | BOOL
 */

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
                End | Else => { break; }
                _ => {
                    let head = self.get();
                    consume_error(head.as_ref(), &[Let, Var, Ident, If, While]);
                    return Err(());
                },
            });
        }

        Ok(Block(statements))
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
                    consume_error(None, &[Equals, Semicolon]);
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
        
        self.consume(&[Equals]).ok_or(())?;
        let expr = self.expr()?;
        self.consume(&[Semicolon]).ok_or(())?;

        Ok(Statement::Reassign(ident, expr))
    }

    fn expr(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;

        let left = self.term()?;
        match self.peek() {
            Some(Plus) | Some(Minus) => {
                let operator = self.consume(&ADDITIVE_OPERATORS).ok_or(())?;
                let right = self.expr()?;

                return Ok(Expr::Binary(Box::new(left), operator, Box::new(right)));
            },
            _ => {
                return Ok(left);
            }
        }
    }

    fn term(&mut self) -> Result<Expr, ()> {
        use TokenKind::*;

        let left = self.factor()?;

        match self.peek() {
            Some(Times) | Some(Div) => {
                let operand = self.consume(&[Times, Div]).ok_or(())?;
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
            Some(Number) => {
                let number = self.literal()?;
                return Ok(Expr::Literal(number));
            },
            Some(Ident) => {
                let ident = self.consume(&[Ident]).ok_or(())?;
                return Ok(Expr::Ident(ident));
            },
            Some(True) | Some(False) => {
                let bool = self.literal()?;
                return Ok(Expr::Literal(bool));
            },
            _ => {
                let next = self.get();
                consume_error(next.as_ref(), &[Number, Ident, OpeningParan]);
                return Err(());
            },
        }
    }

    fn literal(&mut self) -> Result<Literal, ()> {
        use TokenKind::*;

        let token = self.consume(&[Number, String, True, False]).ok_or(())?;
        
        Ok(match token.kind {
            Number => Literal::Number(token.symbols.parse::<u32>().unwrap()),
            String => Literal::String(token.symbols),
            True   => Literal::True,
            False  => Literal::False,
            _ => unreachable!(),
        })
    }

    // fn unary(&mut self) -> Result<Expr, ()> {
    //     let operator = self.consume(&UNARY_OPERATORS).ok_or(())?;
    //     let operand = self.expr()?;

    //     Ok(Expr::Unary(Unary { operator, operand: Box::new(operand) }))
    // }

    fn peek(&mut self) -> Option<TokenKind> {
        self.look_ahead.as_ref().map(|t| t.kind)
    }

    fn consume(&mut self, token_kinds: &[TokenKind]) -> Option<Token> {
        let res = match self.look_ahead.as_ref() {
            Some(token) => {
                if token_kinds.contains(&token.kind) {
                    Some(token.clone())
                } else {
                    consume_error(Some(token), &token_kinds);
                    None
                }
            },
            None => {
                consume_error(None, &token_kinds);
                None
            },
        };
        self.look_ahead = self.tokenizer.next_token();
        
        res
    }

    fn get(&mut self) -> Option<Token> {
        self.tokenizer.next_token()
    }
}



fn consume_error(token: Option<&Token>, token_kinds: &[TokenKind]) {
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
        Some(t) => eprintln!("Expected {} but got {} at {}", token_desc, t.kind, t.loc),
        None => eprintln!("Expected {} but no tokens are left!", token_desc),
    }
}