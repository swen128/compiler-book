use crate::temp::{Label, Temp};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(i64),
    Name(Label),
    Temp(Temp),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Mem(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    ESeq(Box<Statement>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Move(Expr, Expr),
    Exp(Expr),
    Jump(Expr, Vec<Label>),
    CJump(RelOp, Expr, Expr, Label, Label),
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
