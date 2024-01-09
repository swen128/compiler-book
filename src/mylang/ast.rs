use super::document::Span;

pub struct Program(NExpr);

#[derive(Debug, Clone)]
pub struct Node<T> {
    node: Result<T, InvalidNode>,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct InvalidNode {
    content: String,
    span: Span,
}

pub type NExpr = Node<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    Seq(Vec<NExpr>),

    /// An open parenthesis followed immediately by a close parenthesis
    NoValue,
    Nil,
    Int(i64),
    String(String),
    LValue(LValue),
    Array(Array),
    Record(Record),

    /// An integer-valued expression prefixed by the unary minus operator
    Neg(Box<NExpr>),
    /// A binary operator applied to two expressions
    BiOp(NBiOP, Box<NExpr>, Box<NExpr>),
    FuncCall(IdNode, Vec<NExpr>),

    If(If),
    While(While),
    For(For),
    Break,

    Let(Let),
}

#[derive(Debug, Clone)]
pub enum LValue {
    Id(IdNode),
    RecordField(Box<NExpr>, IdNode),
    ArrayIndex(Box<NExpr>, Box<NExpr>),
}

pub type NBiOP = Node<BiOp>;

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
    exps: Vec<NExpr>,
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
    body: Box<NExpr>,
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
    expr: NExpr,
}

#[derive(Debug, Clone)]
pub struct IdNode {
    name: String,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    cond: Box<NExpr>,
    then: Box<NExpr>,
    else_: Option<Box<NExpr>>,
}
