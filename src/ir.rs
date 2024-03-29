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
    Seq(Box<Statement>, Box<Statement>),
    Label(Label),

    Noop,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    /// Bitwise `and` operator
    And,
    /// Bitwise `or` operator
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

impl RelOp {
    pub fn negate(&self) -> Self {
        use RelOp::*;
        match self {
            Eq => Ne,
            Ne => Eq,
            Lt => Ge,
            Gt => Le,
            Le => Gt,
            Ge => Lt,
            Ult => Uge,
            Ule => Ugt,
            Ugt => Ule,
            Uge => Ult,
        }
    }
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

    pub fn binop(op: BinOp, left: Expr, right: Expr) -> Self {
        Expr::BinOp(op, Box::new(left), Box::new(right))
    }

    pub fn new_temp() -> Self {
        Expr::Temp(Temp::new())
    }
}

impl Statement {
    pub fn seq(head: Statement, rest: Vec<Statement>) -> Self {
        rest.into_iter()
            .fold(head, |acc, s| Statement::Seq(Box::new(acc), Box::new(s)))
    }

    pub fn optional_seq(mut statements: impl Iterator<Item = Statement>) -> Self {
        match statements.next() {
            Some(head) => Self::non_empty_seq(head, statements),
            None => Statement::Noop,
        }
    }

    fn non_empty_seq(head: Statement, rest: impl Iterator<Item = Statement>) -> Self {
        rest.fold(head, |acc, s| Statement::pair(acc, s))
    }

    pub fn pair(first: Statement, second: Statement) -> Self {
        Self::Seq(Box::new(first), Box::new(second))
    }

    pub fn jump_to_label(label: Label) -> Self {
        Statement::Jump {
            dst: Expr::Name(label.clone()),
            possible_locations: vec![label],
        }
    }
}
