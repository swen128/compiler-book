use crate::{
    canonical_tree::{Dest, ESeq, Expr, Statement},
    ir,
    temp::Temp,
};

fn canonicalize(expr: ir::Expr) -> ESeq {
    match expr {
        ir::Expr::BinOp(op, left, right) => {
            let ESeq(s1, e1) = canonicalize(*left);
            let ESeq(s2, e2) = canonicalize(*right);

            if commutes(&s2, &e1) {
                let mut s = s1;
                s.extend(s2);
                return ESeq(s, Expr::binop(op, e1, e2));
            }
            print!("s2: {:?}, e1: {:?}", s2, e1);

            let temp = Temp::new();

            let s = {
                let mut s = s1;
                s.push(Statement::Move {
                    dst: Dest::Temp(temp.clone()),
                    src: e1,
                });
                s.extend(s2);
                s
            };
            let e = Expr::binop(op, Expr::Temp(temp), e2);

            ESeq(s, e)
        }

        ir::Expr::Call { address, args } => {
            let ESeq(s1, address) = canonicalize(*address);
            let (s2, args) = sequence(args.into_iter().map(canonicalize).collect());

            let temp_dest = Temp::new();

            let s = {
                let mut s = s1;
                let temp_func_address = Temp::new();

                s.push(Statement::Move {
                    dst: Dest::Temp(temp_func_address.clone()),
                    src: address,
                });
                s.extend(s2);
                s.push(Statement::Call {
                    dst: temp_dest.clone(),
                    func_address: Expr::Temp(temp_func_address.clone()),
                    args,
                });
                s
            };

            let e = Expr::Temp(temp_dest);

            ESeq(s, e)
        }

        ir::Expr::Const(n) => ESeq(vec![], Expr::Const(n)),

        ir::Expr::Name(label) => ESeq(vec![], Expr::Name(label)),

        ir::Expr::Temp(temp) => ESeq(vec![], Expr::Temp(temp)),

        ir::Expr::Mem(address) => {
            let ESeq(s, e) = canonicalize(*address);
            ESeq(s, Expr::mem(e))
        }

        ir::Expr::ESeq(s, e) => {
            let mut s1 = canonicalize_statement(*s);
            let ESeq(s2, e) = canonicalize(*e);
            s1.extend(s2);
            ESeq(s1, e)
        }

        ir::Expr::Error => unreachable!("Invalid expression"),
    }
}

fn canonicalize_statement(statement: ir::Statement) -> Vec<Statement> {
    match statement {
        ir::Statement::Move {
            dst: ir::Expr::Temp(temp),
            src,
        } => {
            let ESeq(mut s, e) = canonicalize(src);
            s.push(Statement::Move {
                src: e,
                dst: Dest::Temp(temp),
            });
            s
        }

        ir::Statement::Move {
            dst: ir::Expr::Mem(address),
            src,
        } => {
            let ESeq(s1, address) = canonicalize(*address);
            let ESeq(s2, e) = canonicalize(src);

            let temp = Temp::new();

            let mut s = s1;

            s.push(Statement::Move {
                dst: Dest::Temp(temp.clone()),
                src: address,
            });
            s.extend(s2);

            let temp_address = Expr::mem(Expr::Temp(temp));

            s.push(Statement::Move {
                dst: Dest::Mem(temp_address),
                src: e,
            });

            s
        }

        ir::Statement::Move { .. } => unreachable!("Invalid move statement"),

        ir::Statement::Exp(expr) => {
            let ESeq(mut s, e) = canonicalize(expr);
            s.push(Statement::Exp(e));
            s
        }

        ir::Statement::Jump {
            dst,
            possible_locations,
        } => {
            let ESeq(mut s, e) = canonicalize(dst);
            s.push(Statement::Jump {
                dst: e,
                possible_locations,
            });
            s
        }

        ir::Statement::CJump {
            op,
            left,
            right,
            if_true,
            if_false,
        } => {
            let ESeq(s1, e1) = canonicalize(left);
            let ESeq(s2, e2) = canonicalize(right);

            if commutes(&s2, &e1) {
                let mut s = s1;
                s.extend(s2);
                s.push(Statement::CJump {
                    op,
                    left: e1,
                    right: e2,
                    if_true,
                    if_false,
                });
                return s;
            }

            let temp = Temp::new();

            let mut s = s1;
            s.push(Statement::Move {
                dst: Dest::Temp(temp.clone()),
                src: e1,
            });
            s.extend(s2);
            s.push(Statement::CJump {
                op,
                left: Expr::Temp(temp),
                right: e2,
                if_true,
                if_false,
            });
            s
        }

        ir::Statement::Seq(s1, s2) => {
            let mut s1 = canonicalize_statement(*s1);
            let s2 = canonicalize_statement(*s2);
            s1.extend(s2);
            s1
        }

        ir::Statement::Label(label) => vec![Statement::Label(label)],

        ir::Statement::Noop => vec![],
    }
}

fn sequence(exprs: Vec<ESeq>) -> (Vec<Statement>, Vec<Expr>) {
    let mut statements = vec![];
    let mut expressions = vec![];

    for ESeq(s, e) in exprs {
        let temp = Temp::new();
        statements.extend(s);
        statements.push(Statement::Move {
            dst: Dest::Temp(temp.clone()),
            src: e,
        });
        expressions.push(Expr::Temp(temp));
    }

    (statements, expressions)
}

/// Returns `true` if the given statement and expression *definitely* commute,
/// i.e. the order of evaluation does not matter.
///
/// This is a conservative approximation,
/// and may return `false` for commutative statements and expressions.
fn commutes(statement: &Vec<Statement>, expression: &Expr) -> bool {
    matches!(
        (statement.as_slice(), expression),
        (_, Expr::Name(_)) | (_, Expr::Const(_)) | ([Statement::Exp(Expr::Const(_))], _)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::canonical_tree::Expr as CExpr;
    use crate::ir::{BinOp, Expr};

    #[test]
    fn canonicalize_calls() {
        let input = Expr::binop(
            BinOp::Plus,
            Expr::call(Expr::Const(0), vec![Expr::Const(1)]),
            Expr::call(Expr::Const(0), vec![Expr::Const(2)]),
        );

        let output = canonicalize(input);

        assert!(matches!(output, ESeq(_, CExpr::BinOp(BinOp::Plus, _, _))));
    }
}
