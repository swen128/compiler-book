use crate::{assembly::Instruction, temp::Temp};

use super::graph::{Graph, Table};

/// A directed graph used for data flow analysis of a program.
/// Each node represents an instruction in the program,
/// and each edge represents a control flow from one instruction to another.
pub struct FlowGraph {
    /// A directed graph where each node represents an instruction in the program.
    pub control: Graph,
    
    /// A table of the temporaries defined at each instruction.
    pub definitions: Table<Vec<Temp>>,
    
    /// A table of the temporaries used at each instruction.
    pub usages: Table<Vec<Temp>>,
    
    /// Tells whether each instruction is a `Move` instruction.
    pub is_move: Table<bool>,
}

impl From<Vec<Instruction>> for FlowGraph {
    fn from(instructions: Vec<Instruction>) -> Self {
        todo!()
    }
}
