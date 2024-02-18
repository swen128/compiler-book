use std::collections::{HashMap, HashSet};

use crate::{
    basic_block::{BasicBlock, Jump, LinearStatement},
    canonical_tree::{Dest, Expr},
    ir::RelOp,
    temp::{Label, Temp},
};

pub struct Trace {
    statements: Vec<Statement>,
}

// This is basically the same as `canonical_tree::Statement`,
// but with `CJump` not having the `if_false` label.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Move {
        src: Expr,
        dst: Dest,
    },

    Call {
        func_address: Expr,
        args: Vec<Expr>,
        dst: Temp,
    },

    Exp(Expr),

    Jump {
        dst: Expr,
    },

    CJump {
        op: RelOp,
        left: Expr,
        right: Expr,
        if_true: Label,
    },

    Label(Label),
}

/// Orders the basic blocks into a list of *traces*,
/// where every `CJump` is immediately followed by its `false` label.
///
/// As preconditions, the basic blocks must satisfy the following properties:
///   - Each block is assgined a unique label.
///   - The last block jumps to the `done` label.
///   - The list is *closed*, i.e. no block (except for the last one) jumps to a label outside the list.
pub fn trace_schedule(blocks: Vec<BasicBlock>, done: Label) -> Vec<Trace> {
    TraceGenerator::new(&blocks, done).collect()
}

struct TraceGenerator<'a> {
    /// A list containing all unprocessed basic blocks.
    /// This may also contain some blocks that have already been claimed by a trace.
    blocks: &'a [BasicBlock],
    /// The labels of the basic blocks that have already been assgined to one of the traces.
    /// Each basic block is assigned to exactly one trace, so these blocks must not be processed again.
    claimed_blocks: HashSet<Label>,
    label_map: HashMap<&'a Label, &'a BasicBlock>,

    // TODO: The `done` label is unused, which suggests the algorithm may be incorrect.
    done: Label,
}

impl<'a> Iterator for TraceGenerator<'a> {
    type Item = Trace;

    fn next(&mut self) -> Option<Self::Item> {
        let first_block = self.next_unclaimed_block().cloned()?;
        let mut statements = vec![];
        let mut maybe_block = Some(first_block);

        while let Some(block) = maybe_block {
            self.claim(&block);
            statements.push(Statement::Label(block.label.clone()));
            statements.extend(block.statements.clone().into_iter().map(Statement::from));
            let (next_block, jump_statement_to_next) = self.successor(&block);
            if let Some(jump_statement) = jump_statement_to_next {
                statements.push(jump_statement);
            }
            maybe_block = next_block;
        }

        Some(Trace { statements })
    }
}

impl<'a> TraceGenerator<'a> {
    fn new(blocks: &'a Vec<BasicBlock>, done: Label) -> Self {
        Self {
            blocks: blocks.as_slice(),
            done,
            claimed_blocks: HashSet::new(),
            label_map: blocks.iter().map(|block| (&block.label, block)).collect(),
        }
    }

    /// Returns one of the unmarked basic blocks to which the given `block` may jump.
    /// It also returns the jump statement that should be added to the trace.
    fn successor(&mut self, block: &BasicBlock) -> (Option<BasicBlock>, Option<Statement>) {
        match block.jump.to_owned() {
            Jump::Jump {
                dst,
                possible_locations,
            } => {
                let is_trivial = possible_locations.len() == 1;
                let next_block = possible_locations
                    .iter()
                    .find(|label| !self.claimed_blocks.contains(label))
                    .and_then(|label| self.label_map.get(&label))
                    .cloned()
                    .cloned();

                let jump_statement = if is_trivial && next_block.is_some() {
                    None
                } else {
                    Some(Statement::Jump { dst })
                };

                (next_block, jump_statement)
            }

            Jump::CJump {
                op,
                left,
                right,
                if_true,
                if_false,
            } => {
                let if_true_block = self.get_unclaimed_block(&if_true);
                let if_false_block = self.get_unclaimed_block(&if_false);

                match (if_true_block, if_false_block) {
                    (_, Some(if_false_block)) => {
                        let jump_statement = Some(Statement::CJump {
                            op,
                            left,
                            right,
                            if_true: if_true.clone(),
                        });

                        (Some(if_false_block.clone()), jump_statement)
                    }
                    (Some(if_true_block), _) => {
                        let jump_statement = Some(Statement::CJump {
                            op: op.negate(),
                            left,
                            right,
                            if_true: if_false.clone(),
                        });

                        (Some(if_true_block.clone()), jump_statement)
                    }
                    (None, None) => {
                        let label = Label::new();
                        let jump_statement = Some(Statement::CJump {
                            op,
                            left,
                            right,
                            if_true: label.clone(),
                        });
                        let block = BasicBlock {
                            label,
                            statements: vec![],
                            jump: Jump::single_label(if_true),
                        };

                        (Some(block), jump_statement)
                    }
                }
            }
        }
    }

    fn claim(&mut self, block: &BasicBlock) {
        self.claimed_blocks.insert(block.label.clone());
    }

    fn next_unclaimed_block(&mut self) -> Option<&'a BasicBlock> {
        let index = self
            .blocks
            .iter()
            .position(|block| !self.claimed_blocks.contains(&block.label))?;

        let block = &self.blocks[index];
        self.blocks = &self.blocks[index..];

        Some(block)
    }

    fn get_unclaimed_block(&self, label: &Label) -> Option<&'a BasicBlock> {
        self.label_map
            .get(label)
            .filter(|block| !self.claimed_blocks.contains(&block.label))
            .copied()
    }
}

impl From<LinearStatement> for Statement {
    fn from(statement: LinearStatement) -> Self {
        match statement {
            LinearStatement::Move { src, dst } => Statement::Move { src, dst },
            LinearStatement::Call {
                func_address,
                args,
                dst,
            } => Statement::Call {
                func_address,
                args,
                dst,
            },
            LinearStatement::Exp(expr) => Statement::Exp(expr),
        }
    }
}
