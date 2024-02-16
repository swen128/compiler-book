use std::ops::Deref;

use super::document::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Spanned<Expr>);

type SubExpr = Box<Spanned<Expr>>;
type SubExprs = Vec<Spanned<Expr>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Seq(SubExprs),

    /// An open parenthesis followed immediately by a close parenthesis
    NoValue,
    Nil,
    Num(u64),
    String(String),
    Array(Box<Array>),
    Record(Record),

    LValue(Box<LValue>),
    Assign(Box<Assign>),

    /// An integer-valued expression prefixed by the unary minus operator
    Neg(SubExpr),
    /// A binary operator applied to two expressions
    BiOp(Spanned<BiOp>, SubExpr, SubExpr),
    FuncCall(IdNode, SubExprs),

    If(Box<If>),
    While(Box<While>),
    For(Box<For>),
    Break,

    Let(Let),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub lhs: Spanned<LValue>,
    pub rhs: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Variable(Id),
    RecordField(Box<Spanned<LValue>>, IdNode),
    ArrayIndex(Box<Spanned<LValue>>, Spanned<Expr>),
}

/// An expression of the form: `<ty> [ <size> ] of <init>`
#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub ty: Spanned<TypeId>,
    pub size: Spanned<Expr>,
    pub init: Spanned<Expr>,
}

/// An expression of the form: `<ty> { <fields> }`
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub ty: Spanned<TypeId>,
    pub fields: Spanned<Vec<RecordField>>,
}

/// An expression of the form: `<key> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct RecordField {
    pub key: IdNode,
    pub value: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BiOp {
    // Arithemtic
    Plus,
    Minus,
    Mul,
    Div,

    // Comparison
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    // Boolean
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub cond: Spanned<Expr>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub id: IdNode,
    pub iter: Range,
    pub body: Spanned<Expr>,
    pub escape: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: Spanned<Expr>,
    pub end: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub decs: Vec<Spanned<Dec>>,
    pub body: SubExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Dec {
    TypeDec(TyDec),
    VarDec(VarDec),
    FnDec(FnDec),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyDec {
    pub id: Spanned<TypeId>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDec {
    pub name: IdNode,
    pub params: TyFields,
    pub return_type: TypeHint,
    pub body: SubExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Name(TypeId),
    Record(TyFields),
    Array(Spanned<TypeId>),
}

type TyFields = Vec<Spanned<TyField>>;

#[derive(Debug, Clone, PartialEq)]
pub struct TyField {
    pub key: IdNode,
    pub ty: Spanned<TypeId>,
    pub escape: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDec {
    pub id: IdNode,
    pub ty: TypeHint,
    pub expr: SubExpr,
    pub escape: bool,
}

type IdNode = Spanned<Id>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id(pub String);

impl Deref for Id {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

type TypeHint = Option<Spanned<TypeId>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeId(pub String);

impl Deref for TypeId {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Spanned<Expr>,
    pub then: Spanned<Expr>,
    pub else_: Option<Spanned<Expr>>,
}

impl Expr {
    pub fn assign(lhs: Spanned<LValue>, rhs: Spanned<Expr>) -> Self {
        Expr::Assign(Box::new(Assign { lhs, rhs }))
    }

    pub fn variable(id: &str) -> Self {
        Expr::LValue(Box::new(LValue::Variable(Id(id.to_string()))))
    }

    pub fn lvalue(lvalue: LValue) -> Self {
        Expr::LValue(Box::new(lvalue))
    }

    pub fn neg(expr: Spanned<Expr>) -> Self {
        Expr::Neg(Box::new(expr))
    }

    pub fn num(num: u64) -> Self {
        Expr::Num(num)
    }

    pub fn string(string: String) -> Self {
        Expr::String(string)
    }

    pub fn biop(op: Spanned<BiOp>, left: Spanned<Expr>, right: Spanned<Expr>) -> Self {
        Expr::BiOp(op, Box::new(left), Box::new(right))
    }

    pub fn array(ty: Spanned<TypeId>, size: Spanned<Expr>, init: Spanned<Expr>) -> Self {
        Expr::Array(Box::new(Array { ty, size, init }))
    }

    pub fn record(ty: Spanned<TypeId>, fields: Spanned<Vec<RecordField>>) -> Self {
        Expr::Record(Record { ty, fields })
    }

    pub fn let_(decs: Vec<Spanned<Dec>>, body: Spanned<Expr>) -> Self {
        Expr::Let(Let {
            decs,
            body: Box::new(body),
        })
    }

    pub fn if_(cond: Spanned<Expr>, then: Spanned<Expr>, else_: Option<Spanned<Expr>>) -> Self {
        Expr::If(Box::new(If { cond, then, else_ }))
    }

    pub fn while_(cond: Spanned<Expr>, body: Spanned<Expr>) -> Self {
        Expr::While(Box::new(While { cond, body }))
    }

    pub fn for_(id: IdNode, iter: Range, body: Spanned<Expr>) -> Self {
        Expr::For(Box::new(For {
            id,
            iter,
            body,
            escape: true,
        }))
    }
}

impl LValue {
    pub fn var(id: &str) -> Self {
        LValue::Variable(Id(id.to_string()))
    }

    pub fn array_index(array: Spanned<LValue>, index: Spanned<Expr>) -> Self {
        LValue::ArrayIndex(Box::new(array), index)
    }

    pub fn record_field(record: Spanned<LValue>, field: IdNode) -> Self {
        LValue::RecordField(Box::new(record), field)
    }
}

impl TyField {
    pub fn new(key: IdNode, ty: Spanned<TypeId>) -> Self {
        Self {
            key,
            ty,
            escape: true,
        }
    }
}

impl VarDec {
    pub fn new(id: IdNode, ty: TypeHint, expr: Spanned<Expr>) -> Self {
        Self {
            id,
            ty,
            expr: Box::new(expr),
            escape: true,
        }
    }
}
