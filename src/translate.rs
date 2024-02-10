use super::ast;
use super::checker::TypedExpr;
use super::frame::{Byte, Frame};
use super::ir::{self, Statement};
use super::temp::Label;

const ARRAY_ELEMENT_SIZE: Byte = Byte(8);

impl ir::Expr {
    const TRUE: Self = Self::Const(1);
    const FALSE: Self = Self::Const(0);

    fn is_truthy(self) -> Condition {
        Condition::Binary(ir::RelOp::Ne, self, Self::FALSE)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ex(ir::Expr),
    Nx(ir::Statement),

    // The Tiger book defines `Cx` using closure of type `(Label, Label) -> Statement`,
    // but naively translating that to Rust would cause troubles,
    // especially because the type `Box<dyn FnOnce(Label, Label) -> Statement>` is not clonable.
    Cx(Condition),
}

impl Expr {
    const TRUE: Self = Self::Ex(ir::Expr::TRUE);
    const FALSE: Self = Self::Ex(ir::Expr::FALSE);

    pub fn un_ex(self) -> ir::Expr {
        match self {
            Expr::Ex(expr) => expr,
            Expr::Nx(statement) => ir::Expr::eseq(statement, ir::Expr::Const(0)),

            // Returns 1 if the condition is true, 0 otherwise.
            Expr::Cx(condition) => {
                let tmp = ir::Expr::new_temp();

                let true_label = Label::new();
                let false_label = Label::new();

                ir::Expr::eseq(
                    ir::Statement::seq(
                        ir::Statement::Move {
                            dst: tmp.clone(),
                            src: ir::Expr::TRUE,
                        },
                        vec![
                            condition.build_statement(true_label.clone(), false_label.clone()),
                            ir::Statement::Label(false_label),
                            ir::Statement::Move {
                                dst: tmp.clone(),
                                src: ir::Expr::FALSE,
                            },
                            ir::Statement::Label(true_label),
                        ],
                    ),
                    tmp,
                )
            }
        }
    }

    fn un_nx(self) -> ir::Statement {
        match self {
            Expr::Ex(expr) => ir::Statement::Exp(expr),
            Expr::Nx(statement) => statement,

            // Just evaluates the condition.
            Expr::Cx(condition) => {
                let true_label = Label::new();
                let false_label = Label::new();

                ir::Statement::seq(
                    condition.build_statement(true_label.clone(), false_label.clone()),
                    vec![
                        ir::Statement::Label(false_label),
                        ir::Statement::Label(true_label),
                    ],
                )
            }
        }
    }

    fn un_cx(self) -> Condition {
        match self {
            Expr::Ex(e) => e.is_truthy(),
            Expr::Nx(statement) => Condition::Always(statement),
            Expr::Cx(f) => f,
        }
    }

    fn constant(n: i64) -> Self {
        Expr::Ex(ir::Expr::Const(n))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Condition {
    Binary(ir::RelOp, ir::Expr, ir::Expr),
    Always(ir::Statement),
    Never(ir::Statement),

    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
}

impl Condition {
    fn build_statement(self, true_label: Label, false_label: Label) -> ir::Statement {
        match self {
            Condition::Binary(op, left, right) => ir::Statement::CJump {
                op,
                left,
                right,
                if_true: true_label,
                if_false: false_label,
            },
            Condition::Always(statement) => {
                ir::Statement::seq(statement, vec![ir::Statement::jump_to_label(true_label)])
            }
            Condition::Never(statement) => {
                ir::Statement::seq(statement, vec![ir::Statement::jump_to_label(false_label)])
            }

            Condition::And(left, right) => {
                let mid_label = Label::new();
                ir::Statement::seq(
                    left.build_statement(mid_label.clone(), false_label.clone()),
                    vec![
                        ir::Statement::Label(mid_label),
                        right.build_statement(true_label, false_label),
                    ],
                )
            }

            Condition::Or(left, right) => {
                let mid_label = Label::new();
                ir::Statement::seq(
                    left.build_statement(true_label.clone(), mid_label.clone()),
                    vec![
                        ir::Statement::Label(mid_label),
                        right.build_statement(true_label, false_label),
                    ],
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Level<F: Frame + Clone + PartialEq> {
    frame: F,
    parent: Option<Box<Level<F>>>,
}

/// Represents a static function nesting level.
impl<F: Frame + Clone + PartialEq> Level<F> {
    pub fn new(parent: Level<F>, name: Label, mut formals: Vec<bool>) -> Self {
        formals.push(true); // A static link is added as if it is another formal parameter which escapes.

        Self {
            frame: F::new(name, formals),
            parent: Some(Box::new(parent)),
        }
    }

    fn static_link(&self) -> F::Access {
        self.frame.formals().last().unwrap().clone()
    }

    pub fn outermost() -> Self {
        Self {
            frame: F::new(Label::new(), vec![]),
            parent: None,
        }
    }
}

pub struct Access<F: Frame + Clone + PartialEq> {
    level: Level<F>,
    frame: F::Access,
}

pub fn formals<F: Frame + Clone + PartialEq>(level: Level<F>) -> Vec<Access<F>> {
    todo!()
}

pub fn alloc_local<F: Frame + Clone + PartialEq>(level: &mut Level<F>, escape: bool) -> Access<F> {
    let frame = level.frame.alloc_local(escape);
    Access {
        level: level.clone(),
        frame,
    }
}

pub fn unit() -> Expr {
    Expr::Nx(ir::Statement::Noop)
}

pub fn nil() -> Expr {
    Expr::Ex(ir::Expr::Const(0))
}

/// Returns an IR expression representing a variable reference.
///
/// ## Parameters
/// - `access`: An access to the variable.
/// - `level`: The function nesting level where the variable is used.
pub fn simple_var<F: Frame + Clone + PartialEq>(access: &Access<F>, level: &Level<F>) -> Expr {
    Expr::Ex(
        level
            .frame
            .exp(&access.frame, resolve_static_link(level, &access.level)),
    )
}

pub fn array_index(array: Expr, index: Expr) -> Expr {
    Expr::Ex(ir::Expr::mem(ir::Expr::sum(
        array.un_ex(),
        ir::Expr::mul(index.un_ex(), ir::Expr::Const(*ARRAY_ELEMENT_SIZE)),
    )))
}

pub fn field_access(record: Expr, index: usize) -> Expr {
    array_index(record, Expr::constant(index as i64))
}

pub fn literal_number(n: u64) -> Expr {
    Expr::constant(n as i64)
}

pub fn function_call(label: Label, args: impl Iterator<Item = Expr>) -> Expr {
    Expr::Ex(ir::Expr::call(
        ir::Expr::Name(label),
        args.map(Expr::un_ex).collect(),
    ))
}

pub fn negation(expr: Expr) -> Expr {
    Expr::Ex(ir::Expr::sub(ir::Expr::Const(0), expr.un_ex()))
}

pub fn sequence(exprs: &[TypedExpr], last: &TypedExpr) -> Expr {
    exprs.into_iter().rfold(last.expr.clone(), |acc, expr| {
        Expr::Ex(ir::Expr::eseq(
            Statement::Exp(expr.expr.clone().un_ex()),
            acc.un_ex(),
        ))
    })
}

pub fn array_init(size: Expr, init: Expr) -> Expr {
    todo!()
}

pub fn assignment(dst: Expr, src: Expr) -> Expr {
    Expr::Nx(Statement::Move {
        src: src.un_ex(),
        dst: dst.un_ex(),
    })
}

pub fn if_then(cond: Expr, then: Expr) -> Expr {
    let true_label = Label::new();
    let false_label = Label::new();

    Expr::Nx(ir::Statement::seq(
        cond.un_cx()
            .build_statement(true_label.clone(), false_label.clone()),
        vec![
            ir::Statement::Label(true_label),
            then.un_nx(),
            ir::Statement::Label(false_label),
        ],
    ))
}

pub fn if_then_else(cond: Expr, then: Expr, else_: Expr) -> Expr {
    Expr::Ex(if_then_else_(cond.un_cx(), then.un_ex(), else_.un_ex()))
}

fn if_then_else_(cond: Condition, then: ir::Expr, else_: ir::Expr) -> ir::Expr {
    let tmp = ir::Expr::new_temp();

    let true_label = Label::new();
    let false_label = Label::new();
    let done_label = Label::new();

    ir::Expr::eseq(
        ir::Statement::seq(
            cond.build_statement(true_label.clone(), false_label.clone()),
            vec![
                ir::Statement::Label(true_label),
                ir::Statement::Move {
                    dst: tmp.clone(),
                    src: then,
                },
                ir::Statement::jump_to_label(done_label.clone()),
                ir::Statement::Label(false_label),
                ir::Statement::Move {
                    dst: tmp.clone(),
                    src: else_,
                },
                ir::Statement::Label(done_label),
            ],
        ),
        tmp,
    )
}

pub fn binary_operator(op: ast::BiOp, left: Expr, right: Expr) -> Expr {
    match op {
        ast::BiOp::Plus => int_operator(ir::BinOp::Plus, left, right),
        ast::BiOp::Minus => int_operator(ir::BinOp::Minus, left, right),
        ast::BiOp::Mul => int_operator(ir::BinOp::Mul, left, right),
        ast::BiOp::Div => int_operator(ir::BinOp::Div, left, right),

        ast::BiOp::And => boolean_and_operator(left, right),
        ast::BiOp::Or => boolean_or_operator(left, right),

        ast::BiOp::Eq => relation_operator(ir::RelOp::Eq, left, right),
        ast::BiOp::Neq => relation_operator(ir::RelOp::Ne, left, right),
        ast::BiOp::Lt => relation_operator(ir::RelOp::Lt, left, right),
        ast::BiOp::Le => relation_operator(ir::RelOp::Le, left, right),
        ast::BiOp::Gt => relation_operator(ir::RelOp::Gt, left, right),
        ast::BiOp::Ge => relation_operator(ir::RelOp::Ge, left, right),
    }
}

fn relation_operator(op: ir::RelOp, left: Expr, right: Expr) -> Expr {
    Expr::Cx(Condition::Binary(op, left.un_ex(), right.un_ex()))
}

fn int_operator(op: ir::BinOp, left: Expr, right: Expr) -> Expr {
    Expr::Ex(ir::Expr::binop(op, left.un_ex(), right.un_ex()))
}

fn boolean_and_operator(left: Expr, right: Expr) -> Expr {
    Expr::Cx(Condition::And(
        Box::new(left.un_cx()),
        Box::new(right.un_cx()),
    ))
}

fn boolean_or_operator(left: Expr, right: Expr) -> Expr {
    Expr::Cx(Condition::Or(
        Box::new(left.un_cx()),
        Box::new(right.un_cx()),
    ))
}

pub fn error() -> Expr {
    Expr::Ex(ir::Expr::Error)
}

fn resolve_static_link<F: Frame + Clone + PartialEq>(
    level_referenced: &Level<F>,
    level_declared: &Level<F>,
) -> ir::Expr {
    // Static links form a kind of linked list in the stack:
    //
    // |    ....     | /
    // | static link | <╮
    // |   arg n     |  |
    // |   .....     |  |
    // |   arg 2     |  |
    // |   arg 1     | /
    // | static link |  <- Frame Pointer

    let mut var = ir::Expr::Temp(F::fp().clone());
    let mut current_level = level_referenced;

    while current_level != level_declared {
        var = current_level.frame.exp(&current_level.static_link(), var);
        current_level = current_level.parent.as_ref().unwrap();
    }

    var
}
