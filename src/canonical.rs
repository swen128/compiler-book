use crate::{
    canonical_tree::{self, Dest, ESeq},
    ir::{Expr, Statement},
    temp::{Label, Temp},
};

/// Returns a list of *canonicalized trees*, which are free from `Seq` and `ESeq` nodes.
fn linearize(statement: Statement) -> Vec<Statement> {
    todo!()
}

/// Groups the list of canonical trees into a set of *basic blocks*,
/// which contain no internal jumps or labels.
fn basic_blocks(statements: Vec<Statement>) -> (Vec<Vec<Statement>>, Label) {
    todo!()
}

/// Orders the basic blocks into a list of *traces*,
/// where every `CJump` is immediately followed by its `false` label.
fn trace_schedule(blocks: Vec<Vec<Statement>>, done: Label) -> Vec<Statement> {
    todo!()
}

fn canonicalize(expr: Expr) -> canonical_tree::ESeq {
    match expr {
        Expr::BinOp(op, left, right) => {
            let ESeq(s1, e1) = canonicalize(*left);
            let ESeq(s2, e2) = canonicalize(*right);

            if commutes(&s2, &e1) {
                let mut s = s1;
                s.extend(s2);
                return ESeq(s, canonical_tree::Expr::binop(op, e1, e2));
            }
            print!("s2: {:?}, e1: {:?}", s2, e1);

            let temp = Temp::new();

            let s = {
                let mut s = s1;
                s.push(canonical_tree::Statement::Move {
                    dst: Dest::Temp(temp.clone()),
                    src: e1,
                });
                s.extend(s2);
                s
            };
            let e = canonical_tree::Expr::binop(op, canonical_tree::Expr::Temp(temp), e2);

            canonical_tree::ESeq(s, e)
        }

        Expr::Call { address, args } => {
            let temp = Expr::new_temp();

            // Succession of two ore more function calls will clobber the same return value register,
            // leading to a loss of the previous return value.
            // In order to mitigate this, we replace any occurrence of function call with the following sequence.
            //
            // Care must be taken not to pass the `Call` ast directly to `canonicalize`,
            // as it will cause infinite recursion.
            let e = Expr::eseq(
                Statement::Move {
                    dst: temp.clone(),
                    src: Expr::call(*address, args),
                },
                temp,
            );
            canonicalize(e)
        }

        Expr::Const(n) => canonical_tree::ESeq(vec![], canonical_tree::Expr::Const(n)),

        Expr::Name(label) => canonical_tree::ESeq(vec![], canonical_tree::Expr::Name(label)),

        Expr::Temp(temp) => canonical_tree::ESeq(vec![], canonical_tree::Expr::Temp(temp)),

        Expr::Mem(address) => {
            let ESeq(s, e) = canonicalize(*address);
            canonical_tree::ESeq(s, canonical_tree::Expr::mem(e))
        }

        Expr::ESeq(s, e) => {
            let mut s1 = canonicalize_statement(*s);
            let ESeq(s2, e) = canonicalize(*e);
            s1.extend(s2);
            canonical_tree::ESeq(s1, e)
        }

        Expr::Error => unreachable!("Invalid expression"),
    }
}

fn canonicalize_statement(statement: Statement) -> Vec<canonical_tree::Statement> {
    match statement {
        // This case is specially handled in order to prevent infinite recursion.
        Statement::Move {
            dst: Expr::Temp(temp_dest),
            src: Expr::Call { address, args },
        } => {
            let ESeq(s1, address) = canonicalize(*address);
            let (s2, args) = sequence(args.into_iter().map(canonicalize).collect());

            let temp_func_address = Temp::new();
            let mut s = s1;

            s.push(canonical_tree::Statement::Move {
                dst: canonical_tree::Dest::Temp(temp_func_address.clone()),
                src: address,
            });
            s.extend(s2);
            s.push(canonical_tree::Statement::Move {
                dst: canonical_tree::Dest::Temp(temp_dest),
                src: canonical_tree::Expr::call(
                    canonical_tree::Expr::Temp(temp_func_address),
                    args,
                ),
            });

            s
        }

        Statement::Move {
            dst: Expr::Temp(temp),
            src,
        } => {
            let ESeq(mut s, e) = canonicalize(src);
            s.push(canonical_tree::Statement::Move {
                src: e,
                dst: canonical_tree::Dest::Temp(temp),
            });
            s
        }

        Statement::Move {
            dst: Expr::Mem(address),
            src,
        } => {
            let ESeq(s1, address) = canonicalize(*address);
            let ESeq(s2, e) = canonicalize(src);

            let temp = Temp::new();

            let mut s = s1;

            s.push(canonical_tree::Statement::Move {
                dst: canonical_tree::Dest::Temp(temp.clone()),
                src: address,
            });
            s.extend(s2);

            let temp_address = canonical_tree::Expr::mem(canonical_tree::Expr::Temp(temp));

            s.push(canonical_tree::Statement::Move {
                dst: canonical_tree::Dest::Mem(temp_address),
                src: e,
            });

            s
        }

        Statement::Move { .. } => unreachable!("Invalid move statement"),

        Statement::Exp(expr) => {
            let ESeq(mut s, e) = canonicalize(expr);
            s.push(canonical_tree::Statement::Exp(e));
            s
        }

        Statement::Jump {
            dst,
            possible_locations,
        } => {
            let ESeq(mut s, e) = canonicalize(dst);
            s.push(canonical_tree::Statement::Jump {
                dst: e,
                possible_locations,
            });
            s
        }

        Statement::CJump {
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
                s.push(canonical_tree::Statement::CJump {
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
            s.push(canonical_tree::Statement::Move {
                dst: canonical_tree::Dest::Temp(temp.clone()),
                src: e1,
            });
            s.extend(s2);
            s.push(canonical_tree::Statement::CJump {
                op,
                left: canonical_tree::Expr::Temp(temp),
                right: e2,
                if_true,
                if_false,
            });
            s
        }

        Statement::Seq(s1, s2) => {
            let mut s1 = canonicalize_statement(*s1);
            let s2 = canonicalize_statement(*s2);
            s1.extend(s2);
            s1
        }

        Statement::Label(label) => vec![canonical_tree::Statement::Label(label)],

        Statement::Noop => vec![],
    }
}

fn sequence(
    exprs: Vec<canonical_tree::ESeq>,
) -> (Vec<canonical_tree::Statement>, Vec<canonical_tree::Expr>) {
    let mut statements = vec![];
    let mut expressions = vec![];

    for ESeq(s, e) in exprs {
        let temp = Temp::new();
        statements.extend(s);
        statements.push(canonical_tree::Statement::Move {
            dst: Dest::Temp(temp.clone()),
            src: e,
        });
        expressions.push(canonical_tree::Expr::Temp(temp));
    }

    (statements, expressions)
}

/// Returns `true` if the given statement and expression *definitely* commute,
/// i.e. the order of evaluation does not matter.
///
/// This is a conservative approximation,
/// and may return `false` for commutative statements and expressions.
fn commutes(statement: &Vec<canonical_tree::Statement>, expression: &canonical_tree::Expr) -> bool {
    use canonical_tree::Expr;
    use canonical_tree::Statement;

    matches!(
        (statement.as_slice(), expression),
        (_, Expr::Name(_)) | (_, Expr::Const(_)) | ([Statement::Exp(Expr::Const(_))], _)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::*;
    use canonical_tree::Expr as CExpr;

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
