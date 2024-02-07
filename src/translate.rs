use crate::checker::TypedExpr;

use super::frame::{Byte, Frame};
use super::ir::{Expr, Statement};
use super::temp::Label;

const ARRAY_ELEMENT_SIZE: Byte = Byte(8);

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
    Expr::Const(0)
}

/// Returns an IR expression representing a variable reference.
///
/// ## Parameters
/// - `access`: An access to the variable.
/// - `level`: The function nesting level where the variable is used.
pub fn simple_var<F: Frame + Clone + PartialEq>(access: &Access<F>, level: &Level<F>) -> Expr {
    level
        .frame
        .exp(&access.frame, resolve_static_link(level, &access.level))
}

pub fn array_index(array: Expr, index: Expr) -> Expr {
    Expr::mem(Expr::sum(
        array,
        Expr::mul(index, Expr::Const(*ARRAY_ELEMENT_SIZE)),
    ))
}

pub fn field_access(record: Expr, index: usize) -> Expr {
    array_index(record, Expr::Const(index as i64))
}

pub fn literal_number(n: u64) -> Expr {
    Expr::Const(n as i64)
}

pub fn function_call(label: Label, args: Vec<Expr>) -> Expr {
    Expr::call(Expr::Name(label), args)
}

pub fn negation(expr: Expr) -> Expr {
    Expr::sub(Expr::Const(0), expr)
}

pub fn sequence(exprs: &[TypedExpr], last: &TypedExpr) -> Expr {
    exprs.into_iter().rfold(last.expr.clone(), |acc, expr| {
        Expr::eseq(Statement::Exp(expr.expr.clone()), acc)
    })
}

pub fn array_init(size: Expr, init: Expr) -> Expr {
    todo!()
}

pub fn assignment(dst: Expr, src: Expr) -> Expr {
    Expr::eseq(Statement::Move { src, dst }, unit())
}

pub fn if_then(cond: Expr, then: Expr) -> Expr {
    todo!()
}

pub fn if_then_else(cond: Expr, then: Expr, else_: Expr) -> Expr {
    todo!()
}

pub fn error() -> Expr {
    Expr::Error
}

fn resolve_static_link<F: Frame + Clone + PartialEq>(
    level_referenced: &Level<F>,
    level_declared: &Level<F>,
) -> Expr {
    // Static links form a kind of linked list in the stack:
    //
    // |    ....     | /
    // | static link | <╮
    // |   arg n     |  |
    // |   .....     |  |
    // |   arg 2     |  |
    // |   arg 1     | /
    // | static link |  <- Frame Pointer

    let mut var = Expr::Temp(F::fp().clone());
    let mut current_level = level_referenced;

    while current_level != level_declared {
        var = current_level.frame.exp(&current_level.static_link(), var);
        current_level = current_level.parent.as_ref().unwrap();
    }

    var
}
