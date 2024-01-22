use std::rc::Rc;

use super::{ast, symbol::SymbolTable};

type Depth = usize;

pub struct EscapeFinder {
    env: SymbolTable<DepthEscape>,
}

struct DepthEscape {
    depth: Depth,
    escape: Rc<bool>,
}

impl EscapeFinder {
    pub fn new() -> Self {
        todo!()
    }

    /// TODO: If we go with this interface, the AST has to have the `escape` field.
    pub fn find_escape(&mut self, program: &mut ast::Program) {
        todo!()
    }

    fn traverse_var(&mut self, d: Depth, s: &mut ast::LValue) {
        todo!()
    }

    fn traverse_expr(&mut self, d: Depth, s: &mut ast::Expr) {
        todo!()
    }

    fn traverse_decs(&mut self, d: Depth, s: &Vec<&mut ast::Dec>) {
        todo!()
    }
}
