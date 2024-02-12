use crate::{ir, temp::Label};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ex(ir::Expr),
    Nx(ir::Statement),

    // The Tiger book defines `Cx` using closure of type `(Label, Label) -> Statement`,
    // but naively translating that to Rust would cause troubles,
    // especially because the type `Box<dyn FnOnce(Label, Label) -> Statement>` is not clonable.
    Cx(Condition),
}

impl Expr {
    pub(super) const TRUE: Self = Self::Ex(ir::Expr::TRUE);
    pub(super) const FALSE: Self = Self::Ex(ir::Expr::FALSE);

    pub(super) fn un_ex(self) -> ir::Expr {
        match self {
            Expr::Ex(expr) => expr,
            Expr::Nx(statement) => ir::Expr::eseq(statement, ir::Expr::Const(0)),

            // Returns 1 if the condition is true, 0 otherwise.
            Expr::Cx(condition) => {
                let tmp = ir::Expr::new_temp();

                let true_label = Label::new();
                let false_label = Label::new();

                ir::Expr::eseq(
                    ir::Statement::seq(
                        ir::Statement::Move {
                            dst: tmp.clone(),
                            src: ir::Expr::TRUE,
                        },
                        vec![
                            condition.build_statement(true_label.clone(), false_label.clone()),
                            ir::Statement::Label(false_label),
                            ir::Statement::Move {
                                dst: tmp.clone(),
                                src: ir::Expr::FALSE,
                            },
                            ir::Statement::Label(true_label),
                        ],
                    ),
                    tmp,
                )
            }
        }
    }

    pub(super) fn un_nx(self) -> ir::Statement {
        match self {
            Expr::Ex(expr) => ir::Statement::Exp(expr),
            Expr::Nx(statement) => statement,

            // Just evaluates the condition.
            Expr::Cx(condition) => {
                let true_label = Label::new();
                let false_label = Label::new();

                ir::Statement::seq(
                    condition.build_statement(true_label.clone(), false_label.clone()),
                    vec![
                        ir::Statement::Label(false_label),
                        ir::Statement::Label(true_label),
                    ],
                )
            }
        }
    }

    pub(super) fn un_cx(self) -> Condition {
        match self {
            Expr::Ex(e) => e.is_truthy(),
            Expr::Nx(statement) => Condition::Always(statement),
            Expr::Cx(f) => f,
        }
    }

    pub(super) fn constant(n: i64) -> Self {
        Expr::Ex(ir::Expr::Const(n))
    }

    pub(super) fn jump_to_label(label: Label) -> Self {
        Expr::Nx(ir::Statement::jump_to_label(label))
    }
}

impl ir::Expr {
    const TRUE: Self = Self::Const(1);
    const FALSE: Self = Self::Const(0);

    fn is_truthy(self) -> Condition {
        Condition::Binary(ir::RelOp::Ne, self, Self::FALSE)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Condition {
    Binary(ir::RelOp, ir::Expr, ir::Expr),
    Always(ir::Statement),
    Never(ir::Statement),

    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
}

impl Condition {
    pub(super) fn build_statement(self, true_label: Label, false_label: Label) -> ir::Statement {
        match self {
            Condition::Binary(op, left, right) => ir::Statement::CJump {
                op,
                left,
                right,
                if_true: true_label,
                if_false: false_label,
            },
            Condition::Always(statement) => {
                ir::Statement::seq(statement, vec![ir::Statement::jump_to_label(true_label)])
            }
            Condition::Never(statement) => {
                ir::Statement::seq(statement, vec![ir::Statement::jump_to_label(false_label)])
            }

            Condition::And(left, right) => {
                let mid_label = Label::new();
                ir::Statement::seq(
                    left.build_statement(mid_label.clone(), false_label.clone()),
                    vec![
                        ir::Statement::Label(mid_label),
                        right.build_statement(true_label, false_label),
                    ],
                )
            }

            Condition::Or(left, right) => {
                let mid_label = Label::new();
                ir::Statement::seq(
                    left.build_statement(true_label.clone(), mid_label.clone()),
                    vec![
                        ir::Statement::Label(mid_label),
                        right.build_statement(true_label, false_label),
                    ],
                )
            }
        }
    }
}
