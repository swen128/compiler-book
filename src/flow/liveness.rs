use std::collections::HashMap;

use crate::temp::{Temp, TempTable};

use super::{
    flow_graph::FlowGraph,
    graph::{Graph, Node, Table},
};

/// A graph used for register allocation.
/// Each node represents a temporary (variable that may reside in a register),
/// and each edge represents an interference between two temporaries.
/// That means, if two temporaries have an edge between them,
/// they cannot be assigned to the same register.
pub struct InterferenceGraph {
    /// A graph where each node represents a temporary.
    pub graph: Graph,

    /// A map from a temporary to the node that represents it in the graph.
    pub tnode: HashMap<Temp, Node>,

    // A map from a graph node to the temporary it represents.
    pub gtemp: HashMap<Node, Temp>,

    /// A list of `Move` instructions.
    /// This is a hint for the register allocator;
    /// it would be better to assign the dst and src of the instruction to the same register if possible.
    pub moves: Vec<MoveInstruction>,
}

impl From<FlowGraph> for InterferenceGraph {
    /// Constructs an interference graph of the given program, based on liveness analysis.
    fn from(graph: FlowGraph) -> Self {
        // 1. Calculate `LiveMap`.
        // 2. At each node `n` where there is a newly defined temporary `d` in `graph.definitions(n)`,
        //    and where temporaries {t1, t2, ..., tk} are live at `n`,
        //    add interference edges (d, t1), (d, t2), ..., (d, tk).
        todo!()
    }
}

struct MoveInstruction {
    src: Temp,
    dst: Temp,
}

/// Keeps track of live temporaries at the exit of each instruction.
struct LiveMap(Table<LiveSet>);

struct LiveSet {
    // This is used for efficient membership tests.
    table: TempTable<()>,

    // This is used for iteration over all the live temporaries.
    live_temporaries: Vec<Temp>,
}

impl LiveMap {
    /// Returns `true` if `temp` is live at the exit of `node`.
    pub fn is_live(&self, temp: Temp, node: Node) -> bool {
        todo!()
    }

    /// Returns a list of temporaries that are live at the exit of `node`.
    pub fn live_temporaries(&self, node: Node) -> &[Temp] {
        self.0
            .get(node)
            .map(|set: LiveSet| set.live_temporaries.as_slice())
            .unwrap_or(&[])
    }
}
