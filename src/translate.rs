use super::ast;
use super::frame::{Byte, Frame};
use super::ir::{self, Statement};
use super::temp::{Label, Temp};

const ARRAY_ELEMENT_SIZE: Byte = Byte(8);
const RECORD_ELEMENT_SIZE: Byte = Byte(8);

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

    fn un_ex(self) -> ir::Expr {
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

    fn jump_to_label(label: Label) -> Self {
        Expr::Nx(ir::Statement::jump_to_label(label))
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

pub enum Fragment<F: Frame + Clone + PartialEq> {
    Function { body: ir::Statement, frame: F },
    String(Label, String),
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

#[derive(Debug, Clone, PartialEq)]
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

pub fn sequence(exprs: impl DoubleEndedIterator<Item = Expr>, last: &Expr) -> Expr {
    exprs.rfold(last.clone(), |acc, expr| {
        Expr::Ex(ir::Expr::eseq(
            Statement::Exp(expr.clone().un_ex()),
            acc.un_ex(),
        ))
    })
}

fn statements(mut exprs: impl Iterator<Item = Expr>) -> Option<Expr> {
    exprs.next().map(|first| {
        Expr::Nx(Statement::seq(
            first.un_nx(),
            exprs.map(Expr::un_nx).collect(),
        ))
    })
}

pub fn array_creation<F: Frame + Clone + PartialEq>(size: Expr, init: Expr) -> Expr {
    Expr::Ex(F::external_call(
        "initARray",
        vec![size.un_ex(), init.un_ex()],
    ))
}

pub fn record_creation<F: Frame + Clone + PartialEq>(
    fields: impl IntoIterator<Item = Expr, IntoIter = impl ExactSizeIterator<Item = Expr>>,
) -> Expr {
    let tmp_address = Temp::new();
    let allocated_address = ir::Expr::Temp(tmp_address.clone());

    let fields = fields.into_iter();
    let allocation = ir::Statement::Move {
        src: memory_allocation::<F>(ir::Expr::Const(*RECORD_ELEMENT_SIZE * fields.len() as i64)),
        dst: allocated_address.clone(),
    };
    let initializaton = record_initialization(tmp_address, fields);

    Expr::Ex(ir::Expr::eseq(
        ir::Statement::pair(allocation, initializaton),
        allocated_address,
    ))
}

fn memory_allocation<F: Frame + Clone + PartialEq>(byte_size: ir::Expr) -> ir::Expr {
    F::external_call("malloc", vec![byte_size])
}

fn record_initialization(
    starting_address: Temp,
    elements: impl IntoIterator<Item = Expr>,
) -> Statement {
    Statement::optional_seq(
        elements
            .into_iter()
            .enumerate()
            .map(|(i, elem)| Statement::Move {
                src: elem.un_ex(),
                dst: ir::Expr::mem(ir::Expr::sum(
                    ir::Expr::Temp(starting_address.clone()),
                    ir::Expr::Const(*RECORD_ELEMENT_SIZE * i as i64),
                )),
            }),
    )
}

pub fn variable_initialization<F: Frame + Clone + PartialEq>(
    level: &Level<F>,
    variable_access: &Access<F>,
    value: Expr,
) -> Expr {
    let frame_pointer = ir::Expr::Temp(F::frame_pointer().clone());

    Expr::Nx(ir::Statement::Move {
        dst: level.frame.exp(&variable_access.frame, frame_pointer),
        src: value.un_ex(),
    })
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
    Expr::Ex(if_then_else_expr(cond.un_cx(), then.un_ex(), else_.un_ex()))
}

fn if_then_else_expr(cond: Condition, then: ir::Expr, else_: ir::Expr) -> ir::Expr {
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

fn if_then_else_unit(cond: Condition, then: ir::Statement, else_: ir::Statement) -> ir::Statement {
    let true_label = Label::new();
    let false_label = Label::new();
    let done_label = Label::new();

    ir::Statement::seq(
        cond.build_statement(true_label.clone(), false_label.clone()),
        vec![
            ir::Statement::Label(true_label),
            then,
            ir::Statement::jump_to_label(done_label.clone()),
            ir::Statement::Label(false_label),
            else_,
            ir::Statement::Label(done_label),
        ],
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

pub fn while_loop(cond: Expr, body: Expr, done_label: Label) -> Expr {
    let test_label = Label::new();
    let body_label = Label::new();

    Expr::Nx(ir::Statement::seq(
        ir::Statement::Label(test_label.clone()),
        vec![
            cond.un_cx()
                .build_statement(body_label.clone(), done_label.clone()),
            ir::Statement::Label(body_label),
            body.un_nx(),
            ir::Statement::jump_to_label(test_label),
            ir::Statement::Label(done_label),
        ],
    ))
}

pub fn for_loop<F: Frame + Clone + PartialEq>(
    level: &mut Level<F>,
    index_access: Access<F>,
    lo: Expr,
    hi: Expr,
    body: Expr,
    done_label: Label,
) -> Expr {
    // ```tiger
    // for i := lo to hi do
    //   body
    // end
    // ```
    //
    // is roughly equivalent to:
    //
    // ```tiger
    // let
    //   var i := lo
    //   var limit := hi      # In order to avoid evaluating `hi` every iteration
    // in
    //   while i <= limit do
    //     (body; i := i + 1)
    //   end
    // end
    // ```
    //
    // If, however, `hi` is the maximum integer, `i + 1` will overflow.
    // The following code mitigates this issue:
    //
    // ```tiger
    // let
    //   var i := lo
    //   var limit := hi
    // in
    //   if i <= limit then
    //     while 1 do
    //       (body; if i = limit then break else i := i + 1)
    //     end
    //   end
    // end
    // ```
    let limit_access = alloc_local(level, false);
    let limit_initialization = variable_initialization(level, &limit_access, hi);

    let loop_label = Label::new();

    let initial_check = relation_operator(
        ir::RelOp::Le,
        simple_var(&index_access, level),
        simple_var(&limit_access, level),
    );

    let check_every_loop = relation_operator(
        ir::RelOp::Ge,
        simple_var(&index_access, level),
        simple_var(&limit_access, level),
    );

    let break_ = if_then(check_every_loop, Expr::jump_to_label(done_label.clone())).un_nx();

    let increment = ir::Statement::Move {
        dst: simple_var(&index_access, level).un_ex(),
        src: int_operator(
            ir::BinOp::Plus,
            simple_var(&index_access, level),
            Expr::constant(1),
        )
        .un_ex(),
    };

    let loop_body = ir::Statement::seq(
        body.un_nx(),
        vec![break_, increment, ir::Statement::jump_to_label(loop_label)],
    );

    let index_initialization = variable_initialization(level, &index_access, lo);
    Expr::Nx(ir::Statement::seq(
        index_initialization.un_nx(),
        vec![
            limit_initialization.un_nx(),
            if_then(
                initial_check,
                Expr::Nx(unconditional_loop(Expr::Nx(loop_body))),
            )
            .un_nx(),
            ir::Statement::Label(done_label),
        ],
    ))
}

fn unconditional_loop(body: Expr) -> ir::Statement {
    let loop_label = Label::new();

    ir::Statement::seq(
        ir::Statement::Label(loop_label.clone()),
        vec![body.un_nx(), ir::Statement::jump_to_label(loop_label)],
    )
}

pub fn let_expression(initializations: impl IntoIterator<Item = Expr>, body: Expr) -> Expr {
    let initialization = statements(initializations.into_iter());
    match initialization {
        None => body,
        Some(initialization) => Expr::Ex(ir::Expr::eseq(initialization.un_nx(), body.un_ex())),
    }
}

pub fn string_literal<F: Frame + Clone + PartialEq>(str: String) -> (Expr, Fragment<F>) {
    let label = Label::new();
    (
        Expr::Ex(ir::Expr::Name(label.clone())),
        Fragment::String(label, str),
    )
}

pub fn function_definition<F: Frame + Clone + PartialEq>(
    level: &mut Level<F>,
    body: Expr,
) -> Fragment<F> {
    let frame = level.frame.clone();
    Fragment::Function {
        body: frame.proc_entry_exit1(body.un_nx()),
        frame,
    }
}

pub fn break_expression(destination: Label) -> Expr {
    Expr::Nx(ir::Statement::jump_to_label(destination))
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

    let mut var = ir::Expr::Temp(F::frame_pointer().clone());
    let mut current_level = level_referenced;

    while current_level != level_declared {
        var = current_level.frame.exp(&current_level.static_link(), var);
        current_level = current_level.parent.as_ref().unwrap();
    }

    var
}
