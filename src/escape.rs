use std::collections::HashMap;

use super::{ast, symbol::Symbol, Spanned};

type Depth = usize;

pub struct EscapeFinder {
    usage: HashMap<Symbol, Depth>,
}

impl EscapeFinder {
    pub fn new() -> Self {
        Self {
            usage: HashMap::new(),
        }
    }

    fn add_var_usage(&mut self, id: ast::Id, depth: Depth) {
        let symbol = Symbol::from(id);
        match self.usage.get(&symbol) {
            None => {
                self.usage.insert(symbol, depth);
            }

            Some(current) => {
                if current < &depth {
                    self.usage.insert(symbol, depth);
                }
            }
        }
    }

    /// Finds all "escaping" variables in the program.
    /// That is, variables used in a deeper scope than they are defined.
    ///
    /// This mutates the `escape` fields in the AST in-place.
    pub fn find_escape(&mut self, program: &mut ast::Program) {
        let ast::Program(expr) = program;
        let depth = 0;
        self.traverse_expr(depth, &mut expr.value);
    }

    fn traverse_decs(&mut self, d: Depth, s: &mut Vec<Spanned<ast::Dec>>) {
        for dec in s {
            match &mut dec.value {
                ast::Dec::FnDec(ast::FnDec {
                    ref mut body,
                    params,
                    ..
                }) => {
                    self.traverse_expr(d + 1, &mut body.value);

                    for param in params {
                        let symbol = Symbol::from(param.value.key.value.clone());
                        param.value.escape = self
                            .usage
                            .get(&symbol)
                            .map_or(false, |depth| depth > &(d + 1));
                        self.usage.remove(&symbol);
                    }
                }

                ast::Dec::VarDec(ref mut v) => {
                    self.traverse_expr(d, &mut v.expr.value);

                    let symbol = Symbol::from(v.id.value.clone());
                    v.escape = self
                        .usage
                        .get(&symbol)
                        .map_or(false, |depth| depth > &(d + 1));
                    self.usage.remove(&symbol);
                }

                ast::Dec::TypeDec(..) => (),
            }
        }
    }

    fn traverse_var(&mut self, d: Depth, s: &mut ast::LValue) {
        match s {
            ast::LValue::Variable(id) => self.add_var_usage(id.clone(), d),
            ast::LValue::ArrayIndex(..) => todo!(),
            ast::LValue::RecordField(..) => todo!(),
        }
    }

    fn traverse_expr(&mut self, d: Depth, s: &mut ast::Expr) {
        match s {
            ast::Expr::LValue(lvalue) => self.traverse_var(d, lvalue),

            ast::Expr::Let(ast::Let { decs, body }) => {
                self.traverse_expr(d + 1, &mut body.value);
                self.traverse_decs(d, decs);
            }

            ast::Expr::For(f) => {
                self.traverse_expr(d, &mut f.iter.start.value);
                self.traverse_expr(d, &mut f.iter.end.value);
                self.traverse_expr(d + 1, &mut f.body.value);

                let symbol = Symbol::from(f.id.value.clone());
                f.escape = self
                    .usage
                    .get(&symbol)
                    .map_or(false, |depth| depth > &(d + 1));
                self.usage.remove(&symbol);
            }

            // Just traverse the subexpressions
            ast::Expr::Array(arr) => {
                self.traverse_expr(d, &mut arr.size.value);
                self.traverse_expr(d, &mut arr.init.value);
            }
            ast::Expr::Record(record) => {
                let ast::Record { ty: _, fields } = record;
                for field in fields {
                    self.traverse_expr(d, &mut field.value.value);
                }
            }
            ast::Expr::Seq(exprs) => {
                for expr in exprs {
                    self.traverse_expr(d, &mut expr.value);
                }
            }
            ast::Expr::Assign(assign) => {
                self.traverse_var(d, &mut assign.lhs.value);
                self.traverse_expr(d, &mut assign.rhs.value);
            }
            ast::Expr::Neg(expr) => {
                self.traverse_expr(d, &mut expr.value);
            }
            ast::Expr::BiOp(_, left, right) => {
                self.traverse_expr(d, &mut left.value);
                self.traverse_expr(d, &mut right.value);
            }
            ast::Expr::FuncCall(_, args) => {
                for arg in args {
                    self.traverse_expr(d, &mut arg.value);
                }
            }
            ast::Expr::If(if_) => {
                self.traverse_expr(d, &mut if_.cond.value);
                self.traverse_expr(d, &mut if_.then.value);
                if_.else_.as_mut().map(|else_| {
                    self.traverse_expr(d, &mut else_.value);
                });
            }
            ast::Expr::While(while_) => {
                self.traverse_expr(d, &mut while_.cond.value);
                self.traverse_expr(d, &mut while_.body.value);
            }

            ast::Expr::Break => (),
            ast::Expr::NoValue => (),
            ast::Expr::Nil => (),
            ast::Expr::Num(_) => (),
            ast::Expr::String(_) => (),
        }
    }
}

impl From<ast::Id> for Symbol {
    fn from(id: ast::Id) -> Self {
        Self::from(id.0.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Span;
    use ast::*;

    #[test]
    fn non_escaping_var() {
        // let x := 1 in x end
        let mut program = Program(spanned(Expr::let_(
            vec![spanned(Dec::VarDec(VarDec::new(
                spanned(Id("x".to_string())),
                None,
                spanned(Expr::num(1)),
            )))],
            spanned(Expr::variable("x")),
        )));

        let mut escape_finder = EscapeFinder::new();
        escape_finder.find_escape(&mut program);

        let expected = Program(spanned(Expr::let_(
            vec![spanned(Dec::VarDec(VarDec {
                id: spanned(Id("x".to_string())),
                ty: None,
                expr: Box::new(spanned(Expr::num(1))),
                escape: false,
            }))],
            spanned(Expr::variable("x")),
        )));

        assert_eq!(program, expected);
    }

    #[test]
    fn escaping_var() {
        // let x := 1 in
        //  let y := 2 in
        //   x
        //  end
        // end
        let mut program = Program(spanned(Expr::let_(
            vec![spanned(Dec::VarDec(VarDec::new(
                spanned(Id("x".to_string())),
                None,
                spanned(Expr::num(1)),
            )))],
            spanned(Expr::let_(
                vec![spanned(Dec::VarDec(VarDec::new(
                    spanned(Id("y".to_string())),
                    None,
                    spanned(Expr::num(2)),
                )))],
                spanned(Expr::variable("x")),
            )),
        )));

        let mut escape_finder = EscapeFinder::new();
        escape_finder.find_escape(&mut program);

        let expected = Program(spanned(Expr::let_(
            vec![spanned(Dec::VarDec(VarDec {
                id: spanned(Id("x".to_string())),
                ty: None,
                expr: Box::new(spanned(Expr::num(1))),
                escape: true,
            }))],
            spanned(Expr::let_(
                vec![spanned(Dec::VarDec(VarDec {
                    id: spanned(Id("y".to_string())),
                    ty: None,
                    expr: Box::new(spanned(Expr::num(2))),
                    escape: false,
                }))],
                spanned(Expr::variable("x")),
            )),
        )));

        assert_eq!(program, expected);
    }

    #[test]
    fn dummy_variable_in_for_loop() {
        // for i := 0 to 10 do i
        fn target_program(escape: bool) -> Program {
            Program(spanned(Expr::For(Box::new(For {
                id: spanned(Id("i".to_string())),
                iter: Range {
                    start: spanned(Expr::num(0)),
                    end: spanned(Expr::num(10)),
                },
                body: spanned(Expr::variable("i")),
                escape,
            }))))
        }

        let mut program = target_program(true);

        let mut escape_finder = EscapeFinder::new();
        escape_finder.find_escape(&mut program);

        let expected = target_program(false);

        assert_eq!(program, expected);
    }

    #[test]
    fn dummy_variable_in_nested_for_loop() {
        // for i := 0 to 10 do
        //  for j := 0 to 10 do i end
        // end
        let mut program = Program(spanned(Expr::For(Box::new(For {
            id: spanned(Id("i".to_string())),
            iter: Range {
                start: spanned(Expr::num(0)),
                end: spanned(Expr::num(10)),
            },
            body: spanned(Expr::For(Box::new(For {
                id: spanned(Id("j".to_string())),
                iter: Range {
                    start: spanned(Expr::num(0)),
                    end: spanned(Expr::num(10)),
                },
                body: spanned(Expr::variable("i")),
                escape: true,
            }))),

            escape: true,
        }))));

        let mut escape_finder = EscapeFinder::new();
        escape_finder.find_escape(&mut program);

        let expected = Program(spanned(Expr::For(Box::new(For {
            id: spanned(Id("i".to_string())),
            iter: Range {
                start: spanned(Expr::num(0)),
                end: spanned(Expr::num(10)),
            },
            body: spanned(Expr::For(Box::new(For {
                id: spanned(Id("j".to_string())),
                iter: Range {
                    start: spanned(Expr::num(0)),
                    end: spanned(Expr::num(10)),
                },
                body: spanned(Expr::variable("i")),
                escape: false,
            }))),

            escape: true,
        }))));

        assert_eq!(program, expected);
    }

    fn spanned<T>(value: T) -> Spanned<T> {
        let placeholder = Span::new(0, 0);
        Spanned::new(value, placeholder)
    }
}
