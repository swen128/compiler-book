pub struct Graph {

}

/// A generic graph.
/// This can be treated as either directed or undirected.
impl Graph {
    pub fn new() -> Self {
        todo!()
    }
    
    pub fn nodes(&self) -> Vec<Node> {
        todo!()
    }
    
    pub fn make_edge(&self, from: Node, to: Node) {
        todo!()
    }
    
    pub fn remove_edge(&self, from: Node, to: Node) {
        todo!()
    }
}

pub struct Node {

}

impl Node {
    pub fn new() -> Self {
        todo!()
    }
    
    /// Returns nodes which have an outgoing edge to this node.
    /// This is only meaningful in directed graphs.
    /// 
    /// The order of the returned nodes is not guaranteed.
    pub fn successors(&self) -> Vec<Node> {
        todo!()
    }

    /// Returns nodes which have an incoming edge from this node.
    /// This is only meaningful in directed graphs.
    /// 
    /// The order of the returned nodes is not guaranteed.
    pub fn predecessors(&self) -> Vec<Node> {
        todo!()
    }
    
    /// Returns nodes that are directly connected to this node by an edge.
    /// This is useful in undirected graphs.
    /// 
    /// The order of the returned nodes is not guaranteed.
    pub fn neighbors(&self) -> Vec<Node> {
        let mut result = self.successors();
        result.extend(self.predecessors());
        result
    }
}

/// When using a graph in an algorithm, we want each node to represent something
/// (an instruction in a program, for example).
/// This table associates a value to each node in a graph.
pub struct Table<T> {
    
}

impl<T> Table<T> {
    pub fn new() -> Self {
        todo!()
    }
    
    pub fn associate(&self, node: Node, value: T) {
        todo!()
    }
    
    pub fn get(&self, node: Node) -> Option<T> {
        todo!()
    }
} 
