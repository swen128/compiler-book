use crate::{assembly::Instruction, frame::Frame, trace::Statement};
mod risc_v;

/// Instruction selector for a given target architecture.
/// 
/// ## Type parameters
/// - `F`: The target architecture's frame type.
trait CodeGen<F: Frame> {
    /// Selects assembly instructions for a given intermediate representation.
    /// This does NOT allocate registers yet.
    fn codegen(&self, frame: &mut F, statements: Vec<Statement>) -> Vec<Instruction>;
}
