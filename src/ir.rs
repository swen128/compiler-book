use crate::temp::{Label, Temp};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(i64),
    Name(Label),
    Temp(Temp),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Mem(Box<Expr>),
    Call { address: Box<Expr>, args: Vec<Expr> },
    ESeq(Box<Statement>, Box<Expr>),

    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Move {
        src: Expr,
        dst: Expr,
    },
    Exp(Expr),
    Jump(Expr, Vec<Label>),
    CJump {
        op: RelOp,
        left: Expr,
        right: Expr,
        if_true: Label,
        if_false: Label,
    },
    Seq(Box<Statement>, Box<Statement>),
    Label(Label),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    LShift,
    RShift,
    ARShift,
    Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

impl Expr {
    pub fn mem(e: Expr) -> Self {
        Expr::Mem(Box::new(e))
    }

    pub fn sum(e1: Expr, e2: Expr) -> Self {
        Expr::BinOp(BinOp::Plus, Box::new(e1), Box::new(e2))
    }

    pub fn sub(e1: Expr, e2: Expr) -> Self {
        Expr::BinOp(BinOp::Minus, Box::new(e1), Box::new(e2))
    }

    pub fn mul(e1: Expr, e2: Expr) -> Self {
        Expr::BinOp(BinOp::Mul, Box::new(e1), Box::new(e2))
    }

    pub fn call(address: Expr, args: Vec<Expr>) -> Self {
        Expr::Call {
            address: Box::new(address),
            args,
        }
    }

    pub fn eseq(s: Statement, e: Expr) -> Self {
        Expr::ESeq(Box::new(s), Box::new(e))
    }
}
