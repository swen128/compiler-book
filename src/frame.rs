use std::ops::Deref;

use crate::{
    intermediate_repr,
    temp::{Label, Temp},
};

/// Abstraction of machine-dependent stack frame structure.
///
/// A frame holds information about formal parameters and local variables for a function.
pub trait Frame {
    type Access: Access;

    const WORD_SIZE: Byte;

    /// Creates a new frame.
    ///
    /// ## Parameters
    /// - `name`: The name of the function associated with this frame.
    /// - `formals`: A list of booleans indicating whether each of the
    ///              formal parameters escapes or not.
    fn new(name: Label, formals: Vec<bool>) -> Self;

    /// Returns the name of the function associated with this frame.
    fn name(&self) -> String;

    /// Returns a list of locations (as seen from inside the callee) of the formal parameters.
    fn formals(&self) -> Vec<Self::Access>;

    /// Allocates a local variable in the frame.
    ///
    /// ## Parameters
    /// - `escape`: Whether the variable escapes or not.
    fn alloc_local(&mut self, escape: bool) -> Self::Access;
}

pub trait Access {
    fn expr(&self, frame_pointer: &Temp) -> intermediate_repr::Expr;
}

pub struct Byte(usize);

impl Deref for Byte {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
