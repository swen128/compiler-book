use std::iter::Peekable;

use crate::{
    canonical_tree::{Dest, Expr, Statement},
    ir::RelOp,
    temp::{Label, Temp},
};

/// Part of a program which is considered an atomic unit in the control flow analysis.
/// It is always entered at the beginning and exited at the end.
#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    /// The entry point of the block.
    pub label: Label,
    pub statements: Vec<LinearStatement>,
    /// The exit point of the block.
    pub jump: Jump,
}

/// Subset of IR statement types which is irrelevant to control flow analysis,
/// i.e. conntains no jumps or labels.
#[derive(Debug, Clone, PartialEq)]
pub enum LinearStatement {
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Jump {
    Jump {
        dst: Expr,
        possible_locations: Vec<Label>,
    },
    CJump {
        op: RelOp,
        left: Expr,
        right: Expr,
        if_true: Label,
        if_false: Label,
    },
}

/// Divides the list of canonical trees into *basic blocks*,
/// separated by labels and jump statements.
///
/// This returns a pair of the basic blocks and the special `done` label,
/// to which the last of the basic blocks jumps.
pub fn basic_blocks(statements: Vec<Statement>) -> (Vec<BasicBlock>, Label) {
    let done = Label::named("done");
    let mut blocks = vec![];

    let mut statements = statements.into_iter().map(BasicBlockToken::from).peekable();

    while let Some(block) = next_basic_block(&mut statements, done.clone()) {
        blocks.push(block);
    }

    if blocks.is_empty() {
        blocks.push(BasicBlock {
            label: Label::new(),
            statements: vec![],
            jump: Jump::single_label(done.clone()),
        });
    }

    (blocks, done)
}

/// TODO: Rewrite with chumsky?
fn next_basic_block(
    statements: &mut Peekable<impl Iterator<Item = BasicBlockToken>>,
    done_label: Label,
) -> Option<BasicBlock> {
    let mut linear_statements = vec![];

    let first_statement = statements.next()?;

    let label_top = match first_statement {
        BasicBlockToken::Start(label) => label,

        BasicBlockToken::Mid(linear_statement) => {
            linear_statements.push(linear_statement);
            Label::new()
        }

        BasicBlockToken::End(jump) => {
            return Some(BasicBlock {
                label: Label::new(),
                statements: linear_statements,
                jump,
            });
        }
    };

    while let Some(statement) = statements.peek() {
        match statement {
            BasicBlockToken::Start(label) => {
                return Some(BasicBlock {
                    label: label_top,
                    statements: linear_statements,
                    // TODO: Avoid cloning.
                    jump: Jump::single_label(label.clone()),
                });
            }

            BasicBlockToken::Mid(linear_statement) => {
                // TODO: Avoid cloning.
                linear_statements.push(linear_statement.clone());
            }

            BasicBlockToken::End(jump) => {
                return Some(BasicBlock {
                    label: label_top,
                    statements: linear_statements,
                    jump: jump.clone(),
                });
            }
        }
        statements.next();
    }

    Some(BasicBlock {
        label: label_top,
        statements: linear_statements,
        jump: Jump::single_label(done_label),
    })
}

// A basic block is thought of as a sequence of statements following this grammar:
//     basic_block -> start? mid* end?
enum BasicBlockToken {
    Start(Label),
    Mid(LinearStatement),
    End(Jump),
}

impl From<Statement> for BasicBlockToken {
    fn from(statement: Statement) -> Self {
        // start -> label
        // mid -> move | call | exp
        // end -> jump | cjump
        match statement {
            Statement::Label(label) => BasicBlockToken::Start(label),
            Statement::Move { src, dst } => {
                BasicBlockToken::Mid(LinearStatement::Move { src, dst })
            }
            Statement::Call {
                func_address,
                args,
                dst,
            } => BasicBlockToken::Mid(LinearStatement::Call {
                func_address,
                args,
                dst,
            }),
            Statement::Exp(e) => BasicBlockToken::Mid(LinearStatement::Exp(e)),

            Statement::Jump {
                dst,
                possible_locations,
            } => BasicBlockToken::End(Jump::Jump {
                dst,
                possible_locations,
            }),
            Statement::CJump {
                op,
                left,
                right,
                if_true,
                if_false,
            } => BasicBlockToken::End(Jump::CJump {
                op,
                left,
                right,
                if_true,
                if_false,
            }),
        }
    }
}

impl Jump {
    /// Unconditional jump to the given label.
    pub fn single_label(label: Label) -> Self {
        Self::Jump {
            dst: Expr::Name(label.clone()),
            possible_locations: vec![label],
        }
    }
}
