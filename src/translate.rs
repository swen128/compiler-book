use crate::ir::Expr;

use super::frame::Frame;

use super::temp::Label;

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

pub fn alloc_local<F: Frame + Clone + PartialEq>(level: &Level<F>, escape: bool) -> Access<F> {
    todo!()
}

pub fn simple_var<F: Frame + Clone + PartialEq>(access: Access<F>, level: Level<F>) -> Expr {
    todo!()
}
