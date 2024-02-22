use crate::{assembly::Instruction, frame::RiscVFrame, trace::Expr, trace::Statement};

use super::CodeGen;

pub struct RiscVCodeGen {}

impl CodeGen<RiscVFrame> for RiscVCodeGen {
    fn codegen(&self, frame: &mut RiscVFrame, statements: Vec<Statement>) -> Vec<Instruction> {
        todo!()
    }
}

/// Returns assembly instructions for a given intermediate representation,
/// using the "maximal munch" algorithm.
fn munch_statement(statement: Statement) -> Vec<Instruction> {
    todo!()
}

/// Returns assembly instructions for a given intermediate representation,
/// using the "maximal munch" algorithm.
fn munch_expr(expr: Expr) -> Vec<Instruction> {
    todo!()
}
