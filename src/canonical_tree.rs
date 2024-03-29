use crate::{
    ir::{BinOp, RelOp},
    temp::{Label, Temp},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(i64),
    Name(Label),
    Temp(Temp),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Mem(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Move {
        src: Expr,
        dst: Dest,
    },

    Call {
        func_address: Expr,
        args: Vec<Expr>,
        dst: Temp,
    },

    Exp(Expr),

    /// Jump to the address of evaluated value of `dst`.
    Jump {
        /// Either a literal label `Expr::Name(Label)`, or an address calculated by arbitrary expression.
        dst: Expr,
        /// All the possible locations that this statement can jump to.
        /// Necessary for dataflow anaylsis.
        possible_locations: Vec<Label>,
    },

    CJump {
        op: RelOp,
        left: Expr,
        right: Expr,
        if_true: Label,
        if_false: Label,
    },

    Label(Label),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Dest {
    Temp(Temp),
    Mem(Expr),
}

impl Dest {
    pub fn new_temp() -> Self {
        Dest::Temp(Temp::new())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ESeq(pub Vec<Statement>, pub Expr);

impl Expr {
    pub fn mem(e: Expr) -> Self {
        Expr::Mem(Box::new(e))
    }

    pub fn binop(op: BinOp, left: Expr, right: Expr) -> Self {
        Expr::BinOp(op, Box::new(left), Box::new(right))
    }

    pub fn new_temp() -> Self {
        Expr::Temp(Temp::new())
    }
}
