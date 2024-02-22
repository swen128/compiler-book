use crate::temp::{Label, Temp};

/// Machine-independent representation of an assembly instruction.
/// This is emitted by the instruction selector and consumed by the register allocator.
pub enum Instruction {
    Operation {
        /// A templated string representing the assembly instruction.
        assembly: String,

        /// A list of result registers.
        /// Some instruction (like `call`) may have no explicit result registers,
        /// but we still want to keep track of the registers that are clobbered.
        dst: Vec<Temp>,

        /// A list of operand registers.
        src: Vec<Temp>,

        jump: Option<Vec<Label>>,
    },

    Label {
        /// A templated string representing the assembly instruction.
        assembly: String,

        label: Label,
    },

    /// Specific type of instruction that moves a value from one register to another.
    Move {
        /// A templated string representing the assembly instruction.
        assembly: String,

        dst: Temp,
        src: Temp,
    },
}

impl Instruction {
    /// Formats the instruction into a string.
    /// 
    /// ## Arguments
    /// - `register_map`: A function that returns the name of the register assigned to a given `Temp`.
    fn format(&self, register_map: impl FnOnce(Temp) -> String) -> String {
        todo!()
    }
}
