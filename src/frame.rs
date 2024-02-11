mod risc_v;

pub use risc_v::RiscVFrame;
use std::ops::{Deref, Mul};

use crate::{
    ir,
    temp::{Label, Temp},
};

/// Abstraction of machine-dependent stack frame structure.
///
/// A frame holds information about formal parameters and local variables for a function.
pub trait Frame {
    type Access: Access + Clone + PartialEq;

    const WORD_SIZE: Byte;

    /// Creates a new frame.
    ///
    /// ## Parameters
    /// - `name`: The name of the function associated with this frame.
    /// - `formals`: A list of booleans indicating whether each of the
    ///              formal parameters escapes or not.
    fn new(name: Label, formals: Vec<bool>) -> Self;

    fn frame_pointer() -> &'static Temp;

    fn return_value_location() -> &'static Temp;

    /// Returns the name of the function associated with this frame.
    fn name(&self) -> Label;

    /// Returns a list of locations (as seen from inside the callee) of the formal parameters.
    fn formals(&self) -> Vec<Self::Access>;

    /// Allocates a local variable in the frame.
    ///
    /// ## Parameters
    /// - `escape`: Whether the variable escapes or not.
    fn alloc_local(&mut self, escape: bool) -> Self::Access;

    /// Returns an expression that accesses the given access.
    ///
    /// ## Parameters
    /// - `stack_frame`: An address of the stack frame where the access is located.
    fn exp(&self, access: &Self::Access, stack_frame: ir::Expr) -> ir::Expr;
    
    fn proc_entry_exit1(&self, body: ir::Statement) -> ir::Statement;
}

pub trait Access {
    fn expr(&self, frame_pointer: &Temp) -> ir::Expr;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Byte(pub i64);

impl Deref for Byte {
    type Target = i64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Mul<i64> for Byte {
    type Output = Byte;

    fn mul(self, rhs: i64) -> Self::Output {
        Byte(self.0 * rhs)
    }
}

impl Mul<Byte> for i64 {
    type Output = Byte;

    fn mul(self, rhs: Byte) -> Byte {
        Byte(self * rhs.0)
    }
}
