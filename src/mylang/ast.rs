use super::document::Span;

pub struct Program(ExprNode);

#[derive(Debug, Clone)]
pub struct ExprNode {
    expr: Expr,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Seq(Vec<ExprNode>),

    /// An open parenthesis followed immediately by a close parenthesis
    NoValue,
    Nil,
    Int(i64),
    String(String),
    LValue(LValue),
    Array(Array),
    Record(Record),

    /// An integer-valued expression prefixed by the unary minus operator
    Neg(Box<ExprNode>),
    /// A binary operator applied to two expressions
    BiOp(BiOpNode, Box<ExprNode>, Box<ExprNode>),
    FuncCall(IdNode, Vec<ExprNode>),

    If(If),
    While(While),
    For(For),
    Break,

    Let(Let),
}

#[derive(Debug, Clone)]
pub enum LValue {
    Id(IdNode),
    RecordField(Box<ExprNode>, IdNode),
    ArrayIndex(Box<ExprNode>, Box<ExprNode>),
}

#[derive(Debug, Clone)]
pub struct BiOpNode {
    op: BiOp,
    span: Span,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Let {
    decs: Vec<DecNode>,
    exps: Vec<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct DecNode {
    dec: Dec,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Dec {
    TypeDec(TyDec),
    VarDec(VarDec),
    FnDec(FnDec),
}

#[derive(Debug, Clone)]
pub struct TyDec {
    id: IdNode,
    ty: TyNode,
}

#[derive(Debug, Clone)]
pub struct FnDec {
    id: IdNode,
    params: Vec<TyFieldNode>,
    ret_ty: Option<IdNode>,
    body: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct TyNode {
    ty: Ty,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Ty {
    Name(IdNode),
    Record(Vec<TyFieldNode>),
    Array(IdNode),
}

#[derive(Debug, Clone)]
pub struct TyFieldNode {
    id: IdNode,
    ty: IdNode,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct VarDec {
    id: IdNode,
    ty: Option<TyNode>,
    expr: ExprNode,
}

#[derive(Debug, Clone)]
pub struct IdNode {
    name: String,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    cond: Box<ExprNode>,
    then: Box<ExprNode>,
    else_: Option<Box<ExprNode>>,
}
