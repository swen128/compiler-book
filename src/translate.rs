mod expr;
mod level;

pub use expr::Expr;
use expr::*;
pub use level::*;

use super::ast;
use super::frame::{Byte, Frame};
use super::ir::{self, Statement};
use super::temp::{Label, Temp};

const ARRAY_ELEMENT_SIZE: Byte = Byte(8);
const RECORD_ELEMENT_SIZE: Byte = Byte(8);

pub enum Fragment<F: Frame + Clone + PartialEq> {
    Function { body: ir::Statement, frame: F },
    String(Label, String),
}

pub fn unit() -> Expr {
    Expr::Nx(ir::Statement::Noop)
}

pub fn nil() -> Expr {
    Expr::Ex(ir::Expr::Const(0))
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

pub fn break_expression(destination: Label) -> Expr {
    Expr::Nx(ir::Statement::jump_to_label(destination))
}

pub fn error() -> Expr {
    Expr::Ex(ir::Expr::Error)
}
