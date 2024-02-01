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
