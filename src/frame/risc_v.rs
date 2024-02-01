use once_cell::sync::Lazy;

use crate::{
    ir::Expr,
    temp::{Label, Temp},
};

use super::{Byte, Frame};

#[derive(Debug, Clone, PartialEq)]
pub struct RiscVFrame {
    name: Label,
    formals: Vec<bool>,
    locals: Vec<bool>,
}

static X8: Lazy<Temp> = Lazy::new(|| Temp::new());

impl Frame for RiscVFrame {
    type Access = RiscVAccess;

    const WORD_SIZE: Byte = Byte(8);

    fn new(name: Label, formals: Vec<bool>) -> Self {
        let frame = Self {
            name,
            formals,
            locals: Vec::new(),
        };
        frame
    }

    fn fp() -> &'static Temp {
        &X8
    }

    fn name(&self) -> Label {
        self.name.clone()
    }

    fn formals(&self) -> Vec<Self::Access> {
        let mut in_frame_count = 0;
        let mut accesses = Vec::new();

        for escape in &self.formals {
            if *escape {
                in_frame_count += 1;
                accesses.push(Self::Access::InFrame(in_frame_count * Self::WORD_SIZE));
            } else {
                accesses.push(Self::Access::InReg(Temp::new()));
            }
        }
        accesses
    }

    fn alloc_local(&mut self, escape: bool) -> Self::Access {
        self.locals.push(escape);

        if escape {
            let in_frame_count = self.locals.iter().filter(|&&b| b).count() as i64;
            Self::Access::InFrame(in_frame_count * Self::WORD_SIZE)
        } else {
            Self::Access::InReg(Temp::new())
        }
    }

    fn exp(&self, access: &Self::Access, stack_frame: Expr) -> Expr {
        match access {
            Self::Access::InReg(reg) => Expr::Temp(reg.clone()),
            Self::Access::InFrame(offset) => {
                Expr::mem(Expr::sum(stack_frame, Expr::Const(**offset)))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RiscVAccess {
    InFrame(Byte),
    InReg(Temp),
}

impl super::Access for RiscVAccess {
    fn expr(&self, frame_pointer: &Temp) -> Expr {
        todo!()
    }
}
