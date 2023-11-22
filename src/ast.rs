use crate::tokenizer::Token;


enum Expr {
    Binary(Binary),
    Unary(Unary),
    Literal(Literal),
}

impl Expr {
    pub fn evaluate(&self) -> Expr {
        match self {
            Expr::Binary(binary) => {
                binary.evaluate()
            },
            Expr::Unary(unary) => {
                unary.evaluate()
            },
            Expr::Literal(literal) => Expr::Literal(literal.clone()),
        }
    }
}

struct Binary {
    left: Box<Expr>,
    right: Box<Expr>,
    operator: Token,
}

impl Binary {
    pub fn evaluate(&self) -> Expr {
        let left = self.left.evaluate();
        let right = self.right.evaluate();
        
        match (left, right) {
            (Expr::Literal(lit1), Expr::Literal(lit2)) => {
                Expr::Literal(Literal::Unit)
            },
            _ => unreachable!(),
        }
    }
}


struct Unary {
    operator: Token,
    operand: Box<Expr>,
}

impl Unary {
    pub fn evaluate(&self) -> Expr {
        let operand = self.operand.evaluate();

        match operand {
            Expr::Literal(lit) => {
                Expr::Literal(Literal::Unit)
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
enum Literal {
    String(String),
    Number(u32),
    True,
    False,
    Unit,
}