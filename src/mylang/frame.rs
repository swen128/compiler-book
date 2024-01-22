use super::temp::Temp;

/// Abstraction of machine-dependent stack frame structure.
/// 
/// A frame holds information about formal parameters and local variables for a function.
pub trait Frame {
    /// Creates a new frame.
    /// 
    /// ## Parameters
    /// - `name`: The name of the function associated with this frame.
    /// - `formals`: A list of booleans indicating whether each of the
    ///              formal parameters escapes or not.
    fn new(name: &str, formals: Vec<bool>) -> Self;

    /// Returns the name of the function associated with this frame.
    fn name(&self) -> String;
    
    /// Returns a list of locations (as seen from inside the callee) of the formal parameters.
    fn formals(&self) -> Vec<Access>;

    /// Allocates a local variable in the frame.
    /// 
    /// ## Parameters
    /// - `escape`: Whether the variable escapes or not.
    fn alloc_local(&mut self, escape: bool) -> Access;
}

enum Access {
    /// `InFrame(offset)` means a memory location at `offset` from the frame pointer.
    InFrame(usize),

    InReg(Temp),
}
