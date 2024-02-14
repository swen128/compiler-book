mod array;
mod error;
mod record;

use self::array::array_index_access;

use super::document::{Span, Spanned};
use super::env::{ValueEntry, ValueTable};
use super::frame::Frame;
use super::symbol::Symbol;
use super::temp::Label;
use super::translate::{self, *};
use super::types::{FunctionSignature, IdGenerator, Ty};
use super::{
    ast,
    env::{Environment, Scope, TypeTable},
    types,
};
pub use error::SemanticError;
use record::*;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    pub expr: translate::Expr,
    pub ty: types::Ty,
    pub span: Span,
}

pub fn trans_program<F: Frame + Clone + PartialEq>(
    program: ast::Program,
) -> (TypedExpr, Vec<SemanticError>) {
    let mut analyzer = Checker::<F>::new();
    let mut env = Environment::<F>::base();
    let mut level = Level::<F>::outermost();

    let typed_expr = analyzer.trans_expr(program.0, &mut level, &mut env);

    (typed_expr, analyzer.errors)
}

struct Checker<F: Frame + Clone + PartialEq> {
    /// Any method in the `Checker` must push to this vector as it encounters semantic errors in the input program.
    /// Elements in this vector must not be removed or modified.
    errors: Vec<SemanticError>,

    id_generator: IdGenerator,
    fragments: Vec<Fragment<F>>,

    /// The label placed at the end of the nearest loop.
    /// The `break` expression should jump to this label.
    nearest_loop: Option<Label>,
}

impl<F: Frame + Clone + PartialEq> Checker<F> {
    fn new() -> Self {
        Self {
            errors: vec![],
            id_generator: IdGenerator::new(),
            fragments: vec![],
            nearest_loop: None,
        }
    }

    /// Enters a new loop, returning the label of the previous loop if present.
    fn enter_loop(&mut self, label: Label) -> Option<Label> {
        self.nearest_loop.replace(label)
    }

    fn leave_loop(&mut self, previous_loop: Option<Label>) {
        self.nearest_loop = previous_loop;
    }

    fn trans_expr(
        &mut self,
        expr: Spanned<ast::Expr>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let span = expr.span.clone();

        match expr.value {
            ast::Expr::Let(expr) => self.trans_let(Spanned::new(expr, span), parent_level, env),
            ast::Expr::LValue(var) => self.trans_var(Spanned::new(*var, span), parent_level, env),
            ast::Expr::Seq(exprs) => self.trans_seq(exprs, span, parent_level, env),

            ast::Expr::NoValue => TypedExpr {
                expr: unit(),
                ty: types::Ty::Unit,
                span,
            },
            ast::Expr::Nil => TypedExpr {
                expr: nil(),
                ty: types::Ty::Nil,
                span,
            },
            ast::Expr::Num(n) => TypedExpr {
                expr: literal_number(n),
                ty: types::Ty::Int,
                span,
            },

            ast::Expr::String(str) => self.trans_string_literal(str, span),
            ast::Expr::Array(array) => {
                self.trans_array(Spanned::new(*array, span), parent_level, env)
            }
            ast::Expr::Record(record) => {
                self.trans_record(Spanned::new(record, span), parent_level, env)
            }
            ast::Expr::Assign(assign) => {
                self.trans_assign(Spanned::new(*assign, span), parent_level, env)
            }
            ast::Expr::Neg(arg) => {
                let arg = self.trans_specific_type_expr(types::Ty::Int, *arg, parent_level, env);
                TypedExpr {
                    expr: negation(arg.expr),
                    ty: types::Ty::Int,
                    span,
                }
            }
            ast::Expr::BiOp(op, left, right) => {
                self.trans_binary_operator(op.value, *left, *right, parent_level, env)
            }
            ast::Expr::FuncCall(name, args) => {
                self.trans_func_call(name, args, span, parent_level, env)
            }
            ast::Expr::If(if_) => self.trans_if(Spanned::new(*if_, span), parent_level, env),
            ast::Expr::While(while_) => {
                self.trans_while(Spanned::new(*while_, span), parent_level, env)
            }
            ast::Expr::For(for_) => self.trans_for(Spanned::new(*for_, span), parent_level, env),
            ast::Expr::Break => self.trans_break(span),
        }
    }

    fn trans_break(&mut self, span: Span) -> TypedExpr {
        TypedExpr {
            span,
            ty: types::Ty::Unit,
            expr: match self.nearest_loop.clone() {
                Some(label) => break_expression(label),
                None => {
                    self.errors.push(SemanticError::BreakOutsideLoop);
                    error()
                }
            },
        }
    }

    fn trans_string_literal(&mut self, str: String, span: Span) -> TypedExpr {
        let (expr, fragment) = string_literal(str);
        self.fragments.push(fragment);
        TypedExpr {
            expr,
            span,
            ty: types::Ty::String,
        }
    }

    fn trans_while(
        &mut self,
        while_: Spanned<ast::While>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::While { cond, body } = while_.value;
        let cond = self.trans_specific_type_expr(Ty::Int, cond, parent_level, env);

        let done_label = Label::new();
        let previous_loop = self.enter_loop(done_label.clone());
        let body = self.trans_expr(body, parent_level, env);
        self.leave_loop(previous_loop);

        TypedExpr {
            expr: while_loop(cond.expr, body.expr, done_label),
            ty: Ty::Unit,
            span: while_.span,
        }
    }

    fn trans_for(
        &mut self,
        for_: Spanned<ast::For>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::For {
            id,
            iter,
            body,
            escape,
        } = for_.value;

        let start = self.trans_specific_type_expr(Ty::Int, iter.start, parent_level, env);
        let end = self.trans_specific_type_expr(Ty::Int, iter.end, parent_level, env);

        let mut scope = Scope::new(env);
        let access = declare_variable(
            Symbol::from(id.value),
            Ty::Int,
            escape,
            parent_level,
            &mut scope,
        );
        let done_label = Label::new();
        let previous_loop = self.enter_loop(done_label.clone());
        let body = self.trans_expr(body, parent_level, &mut scope);
        self.leave_loop(previous_loop);

        TypedExpr {
            expr: for_loop(
                parent_level,
                access,
                start.expr,
                end.expr,
                body.expr,
                done_label,
            ),
            ty: Ty::Unit,
            span: for_.span,
        }
    }

    fn trans_binary_operator(
        &mut self,
        op: ast::BiOp,
        left: Spanned<ast::Expr>,
        right: Spanned<ast::Expr>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let left = self.trans_expr(left, parent_level, env);
        let right = self.trans_expr(right, parent_level, env);

        let (expr, errors) = check_binary_operator(op, left, right);
        self.errors.extend(errors);
        expr
    }

    fn trans_if(
        &mut self,
        if_: Spanned<ast::If>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::If { cond, then, else_ } = if_.value;
        let cond = self.trans_specific_type_expr(Ty::Int, cond, parent_level, env);

        if let Some(else_) = else_ {
            let then = self.trans_expr(then, parent_level, env);
            let else_ = self.trans_expr(else_, parent_level, env);
            let ty = Ty::common_type(&then.ty, &else_.ty);

            match ty {
                Some(ty) => TypedExpr {
                    expr: if_then_else(cond.expr, then.expr, else_.expr),
                    ty,
                    span: if_.span,
                },
                None => {
                    self.errors.push(SemanticError::TypeError {
                        expected: then.ty.clone(),
                        found: else_.ty.clone(),
                        span: else_.span,
                    });
                    TypedExpr {
                        expr: error(),
                        ty: Ty::Unknown,
                        span: if_.span,
                    }
                }
            }
        } else {
            let then = self.trans_specific_type_expr(Ty::Unit, then, parent_level, env);
            TypedExpr {
                expr: if_then(cond.expr, then.expr),
                ty: Ty::Unit,
                span: if_.span,
            }
        }
    }

    /// Translates an array expression like `intArray [10] of 0`.
    fn trans_array(
        &mut self,
        array: Spanned<ast::Array>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ty = self.lookup_type(array.value.ty, &env.types);

        let size = self.trans_specific_type_expr(Ty::Int, array.value.size, parent_level, env);
        let init = self.trans_expr(array.value.init, parent_level, env);

        if let types::Ty::Array(_, ref element_ty) = ty.value {
            let element_ty = element_ty.as_ref();

            if init.ty.is_subtype_of(&element_ty) {
                TypedExpr {
                    expr: array_creation::<F>(size.expr, init.expr),
                    ty: ty.value,
                    span: array.span,
                }
            } else {
                self.errors.push(SemanticError::TypeError {
                    expected: element_ty.clone(),
                    found: init.ty,
                    span: init.span,
                });
                TypedExpr {
                    expr: error(),
                    ty: ty.value,
                    span: array.span,
                }
            }
        } else {
            self.errors.push(SemanticError::UnexpectedNonArray {
                found: ty.value,
                span: ty.span,
            });
            TypedExpr {
                expr: error(),
                ty: types::Ty::Unknown,
                span: array.span,
            }
        }
    }

    /// Translates a record expression like `point { x = 0, y = 0 }`.
    fn trans_record(
        &mut self,
        record: Spanned<ast::Record>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::Record { fields, ty } = record.value;

        let ty = self.lookup_type(ty.clone(), &env.types);
        let fields = Spanned {
            span: fields.span,
            value: fields
                .value
                .into_iter()
                .map(|field| {
                    let key = Spanned::new(Symbol::from(field.key.value), field.key.span);
                    let value = self.trans_expr(field.value, parent_level, env);
                    (key, value)
                })
                .collect(),
        };

        match check_record_type(fields, &ty) {
            Ok(fields) => TypedExpr {
                expr: record_creation::<F>(fields),
                ty: ty.value,
                span: record.span,
            },
            Err(errors) => {
                self.errors.extend(errors);
                TypedExpr {
                    expr: error(),
                    ty: types::Ty::Unknown,
                    span: record.span,
                }
            }
        }
    }

    fn trans_assign(
        &mut self,
        assign: Spanned<ast::Assign>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::Assign { lhs, rhs } = assign.value;
        let lhs = self.trans_var(lhs, parent_level, env);
        let rhs = self.trans_expr(rhs, parent_level, env);

        if rhs.ty.is_subtype_of(&lhs.ty) {
            TypedExpr {
                expr: assignment(lhs.expr, rhs.expr),
                ty: types::Ty::Unit,
                span: assign.span,
            }
        } else {
            self.errors.push(SemanticError::TypeError {
                expected: lhs.ty.clone(),
                found: rhs.ty.clone(),
                span: rhs.span,
            });
            TypedExpr {
                expr: error(),
                ty: types::Ty::Unit,
                span: assign.span,
            }
        }
    }

    fn trans_func_call(
        &mut self,
        name: Spanned<ast::Id>,
        args: Vec<Spanned<ast::Expr>>,
        span: Span,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let args_typed = args
            .into_iter()
            .map(|arg| self.trans_expr(arg, parent_level, env))
            .collect();

        let signature = self.lookup_function(&name, env);

        match signature {
            Some((FunctionSignature { params, result }, label)) => {
                let arg_type_errors = check_function_arg_types(&args_typed, &params, span);
                self.errors.extend(arg_type_errors);

                let translated_args = args_typed.into_iter().map(|arg| arg.expr);

                TypedExpr {
                    expr: function_call(label.clone(), translated_args),
                    ty: result.clone(),
                    span,
                }
            }

            None => TypedExpr {
                expr: error(),
                ty: types::Ty::Unknown,
                span,
            },
        }
    }

    fn trans_specific_type_expr(
        &mut self,
        expected_ty: types::Ty,
        expr: Spanned<ast::Expr>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let expr = self.trans_expr(expr, parent_level, env);

        if expr.ty.is_subtype_of(&expected_ty) {
            TypedExpr {
                ty: expected_ty,
                ..expr
            }
        } else {
            self.errors.push(SemanticError::TypeError {
                expected: expected_ty.clone(),
                found: expr.ty,
                span: expr.span,
            });
            TypedExpr {
                expr: error(),
                ty: expected_ty,
                span: expr.span,
            }
        }
    }

    fn trans_var(
        &mut self,
        var: Spanned<ast::LValue>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let span = var.span;
        match var.value {
            ast::LValue::Variable(id) => {
                let result = env.values.get(&Symbol::from(&id)).map_or_else(
                    || {
                        Err(vec![SemanticError::UndefinedVariable {
                            name: Symbol::from(&id),
                            span,
                        }])
                    },
                    |entry| match entry {
                        ValueEntry::Variable { ty, access } => Ok(TypedExpr {
                            expr: simple_var(access, parent_level),
                            ty: ty.clone(),
                            span,
                        }),
                        _ => Err(vec![SemanticError::UnexpectedFunction {
                            name: Symbol::from(&id),
                            span,
                        }]),
                    },
                );

                match result {
                    Ok(typed_expr) => typed_expr,
                    Err(new_errors) => {
                        self.errors.extend(new_errors);
                        TypedExpr {
                            expr: error(),
                            ty: types::Ty::Unknown,
                            span,
                        }
                    }
                }
            }

            ast::LValue::RecordField(lvalue, field) => {
                let lvalue = self.trans_var(*lvalue, parent_level, env);

                record_field_access(lvalue, field, span).unwrap_or_else(|err| {
                    self.errors.push(err);
                    TypedExpr {
                        expr: error(),
                        ty: types::Ty::Unknown,
                        span,
                    }
                })
            }

            ast::LValue::ArrayIndex(lvalue, index) => {
                let lvalue = self.trans_var(*lvalue, parent_level, env);
                let index = self.trans_expr(index, parent_level, env);
                array_index_access(lvalue, index, span, &mut self.errors)
            }
        }
    }

    fn trans_seq(
        &mut self,
        sub_exprs: Vec<Spanned<ast::Expr>>,
        span: Span,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let sub_exprs = sub_exprs
            .into_iter()
            .map(|sub_expr| self.trans_expr(sub_expr, parent_level, env))
            .collect::<Vec<_>>();

        match sub_exprs.split_last() {
            // `sub_exprs` is empty, return no value.
            None => TypedExpr {
                expr: unit(),
                ty: types::Ty::Unit,
                span,
            },

            Some((last, exprs)) => TypedExpr {
                expr: sequence(
                    exprs.into_iter().map(|typed| typed.expr.clone()),
                    &last.expr,
                ),
                ty: last.ty.clone(),
                span,
            },
        }
    }

    fn trans_let(
        &mut self,
        expr: Spanned<ast::Let>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::Let { decs, body } = expr.value;
        let mut scope = Scope::new(env);

        // Translating the declarations mus precede the translation of the body,
        // as it causes the side effect of adding new bindings to the environment.
        let initializations: Vec<_> = decs
            .into_iter()
            .flat_map(|dec| self.trans_dec(dec, parent_level, &mut scope))
            .collect();

        let body = self.trans_expr(*body, parent_level, &mut scope);

        TypedExpr {
            expr: let_expression(initializations, body.expr),
            ty: body.ty,
            span: expr.span,
        }
    }

    /// Type-check the given declaration and add the resulting binding to the environment.
    /// It may also return an initialization expression, which must be evaluated at the beginning of the scope.
    fn trans_dec(
        &mut self,
        dec: Spanned<ast::Dec>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> Option<translate::Expr> {
        match dec.value {
            ast::Dec::TypeDec(ast::TyDec { id, ty }) => {
                env.types.insert(
                    Symbol::from(&id.value),
                    trans_type(ty.value, &env.types, &mut self.id_generator),
                );
                None
            }

            ast::Dec::VarDec(ast::VarDec {
                id,
                ty,
                expr,
                escape,
            }) => {
                let symbol = Symbol::from(id.value);
                let declared_ty = ty
                    .and_then(|type_id| env.types.get(&Symbol::from(&type_id.value)))
                    .cloned();
                let rhs = self.trans_expr(*expr, parent_level, env);

                let rhs_ty = match (declared_ty, rhs.ty) {
                    (None, Ty::Nil) => {
                        self.errors
                            .push(SemanticError::UntypedNilError { span: rhs.span });
                        Ty::Unknown
                    }

                    (None, rhs_ty) => rhs_ty,

                    (Some(declared), rhs_ty) => {
                        if !rhs_ty.is_subtype_of(&declared) {
                            self.errors.push(SemanticError::TypeError {
                                expected: declared.clone(),
                                found: rhs_ty,
                                span: rhs.span,
                            })
                        }
                        declared
                    }
                };

                let access = declare_variable(symbol, rhs_ty, escape, parent_level, env);
                let initialization = variable_initialization(&parent_level, &access, rhs.expr);

                Some(initialization)
            }

            ast::Dec::FnDec(ast::FnDec {
                name,
                params,
                return_type,
                body,
            }) => {
                let symbol = Symbol::from(name.value);
                let declared_return_ty = return_type
                    .and_then(|type_id| env.types.get(&Symbol::from(&type_id.value)))
                    .cloned();
                let params = trans_function_params(params, &env.types, &mut self.errors);
                let params_types = params.iter().map(|(_, ty, _)| ty.clone()).collect();

                let mut level = Level::<F>::new(
                    parent_level.clone(),
                    Label::new(),
                    params.iter().map(|(_, _, escape)| *escape).collect(),
                );
                let body = {
                    let mut scope = Scope::new(env);
                    for (symbol, ty, escape) in params {
                        let access = alloc_local(&mut level, escape);

                        scope
                            .values
                            .insert(symbol, ValueEntry::Variable { ty, access });
                    }
                    self.trans_expr(*body, &mut level, &mut scope)
                };

                let is_return_type_compatible = declared_return_ty
                    .as_ref()
                    .map_or(true, |ty| body.ty.is_subtype_of(ty));

                if !is_return_type_compatible {
                    self.errors.push(SemanticError::TypeError {
                        expected: declared_return_ty.clone().unwrap(),
                        found: body.ty.clone(),
                        span: body.span,
                    });
                }

                let return_ty = declared_return_ty.unwrap_or(body.ty);
                env.values
                    .insert(symbol, ValueEntry::func(params_types, return_ty));

                let fragment = function_definition(&mut level, body.expr);
                self.fragments.push(fragment);

                None
            }
        }
    }

    fn lookup_type(
        &mut self,
        type_id: Spanned<ast::TypeId>,
        env: &TypeTable,
    ) -> Spanned<types::Ty> {
        let span = type_id.span;
        lookup_type(type_id, &env).unwrap_or_else(|err| {
            self.errors.push(err);
            Spanned::new(types::Ty::Unknown, span)
        })
    }

    fn lookup_variable<'a>(
        &mut self,
        id: &Spanned<ast::Id>,
        env: &'a ValueTable<F>,
    ) -> Option<(&'a Ty, &'a Access<F>)> {
        match lookup_variable(id, env) {
            Ok((ty, access)) => Some((ty, access)),
            Err(err) => {
                self.errors.push(err);
                None
            }
        }
    }

    fn lookup_function<'a>(
        &mut self,
        name: &Spanned<ast::Id>,
        env: &'a Environment<F>,
    ) -> Option<(&'a FunctionSignature, &'a Label)> {
        match lookup_function(name, env) {
            Ok(function) => Some(function),
            Err(err) => {
                self.errors.push(err);
                None
            }
        }
    }
}

fn lookup_variable<'a, F: Frame + Clone + PartialEq>(
    id: &Spanned<ast::Id>,
    env: &'a ValueTable<F>,
) -> Result<(&'a Ty, &'a Access<F>), SemanticError> {
    let symbol = Symbol::from(&id.value);
    env.get(&symbol)
        .map(|entry| match entry {
            ValueEntry::Variable { ty, access } => Ok((ty, access)),
            ValueEntry::Function { .. } => Err(SemanticError::UnexpectedFunction {
                name: symbol.clone(),
                span: id.span,
            }),
        })
        .unwrap_or_else(|| {
            Err(SemanticError::UndefinedVariable {
                name: symbol,
                span: id.span,
            })
        })
}

/// Looks up the given function name in the environment.
fn lookup_function<'a, F: Frame + Clone + PartialEq>(
    name: &Spanned<ast::Id>,
    env: &'a Environment<F>,
) -> Result<(&'a FunctionSignature, &'a Label), SemanticError> {
    let symbol = Symbol::from(&name.value);

    match env.values.get(&symbol) {
        Some(ValueEntry::Function { signature, label }) => Ok((signature, label)),
        Some(ValueEntry::Variable { .. }) => Err(SemanticError::UnexpectedVariable {
            name: symbol.clone(),
            span: name.span,
        }),
        None => Err(SemanticError::UndefinedFunction {
            name: symbol.clone(),
            span: name.span,
        }),
    }
}

fn lookup_type(
    type_id: Spanned<ast::TypeId>,
    env: &TypeTable,
) -> Result<Spanned<types::Ty>, SemanticError> {
    let symbol = Symbol::from(&type_id.value);
    env.get(&symbol)
        .cloned()
        .map(|ty| Spanned::new(ty, type_id.span))
        .ok_or_else(|| SemanticError::UndefinedType {
            name: symbol,
            span: type_id.span,
        })
}

fn trans_function_params(
    params: Vec<Spanned<ast::TyField>>,
    env: &TypeTable,
    errors: &mut Vec<SemanticError>,
) -> Vec<(Symbol, types::Ty, bool)> {
    let entries = params
        .into_iter()
        .map(|spanned| spanned.value)
        .map(|param| {
            let param_name = Symbol::from(&param.key.value);
            let type_id = Symbol::from(&param.ty.value);

            let ty = env.get(&type_id).cloned().unwrap_or({
                errors.push(SemanticError::UndefinedType {
                    name: type_id.clone(),
                    span: param.ty.span,
                });
                types::Ty::Name(type_id, Box::new(types::Ty::Unknown))
            });

            (param_name, ty, param.escape)
        })
        .collect();

    entries
}

fn trans_type(ty: ast::Ty, env: &TypeTable, id_generator: &mut IdGenerator) -> types::Ty {
    match ty {
        ast::Ty::Name(id) => trans_type_name(id, env),
        ast::Ty::Record(fields) => {
            types::Ty::record(fields.into_iter().map(|Spanned { value: field, .. }| {
                let key = Symbol::from(&field.key.value);
                let ty = trans_type_name(field.ty.value, env);
                (key, ty)
            }))
        }
        ast::Ty::Array(Spanned {
            span: _,
            value: element_type_id,
        }) => {
            let element_type = trans_type_name(element_type_id, env);
            types::Ty::Array(id_generator.next(), Box::new(element_type))
        }
    }
}

fn trans_type_name(id: ast::TypeId, env: &TypeTable) -> types::Ty {
    let symbol = Symbol::from(&id);
    let ty = env
        .get(&symbol)
        .map(|t| Box::new(t.clone()))
        .unwrap_or(Box::new(types::Ty::Unknown));
    types::Ty::Name(symbol, ty)
}

fn check_function_arg_types(
    args: &Vec<TypedExpr>,
    param_types: &Vec<Ty>,
    whole_args_span: Span,
) -> Vec<SemanticError> {
    let mut errors = vec![];

    if args.len() != param_types.len() {
        errors.push(SemanticError::WrongNumberOfArguments {
            expected: param_types.len(),
            found: args.len(),
            span: whole_args_span,
        });
    }

    args.iter()
        .map(|arg| Spanned::new(arg.ty.clone(), arg.span))
        .zip(param_types.iter())
        .filter(|(arg, param)| !arg.value.is_subtype_of(param))
        .for_each(|(arg, param)| {
            errors.push(SemanticError::TypeError {
                expected: param.clone(),
                found: arg.value.clone(),
                span: arg.span,
            });
        });

    errors
}

fn check_binary_operator(
    op: ast::BiOp,
    left: TypedExpr,
    right: TypedExpr,
) -> (TypedExpr, Vec<SemanticError>) {
    use ast::BiOp::*;

    let errors = match &op {
        Eq | Neq => {
            if left.ty.is_comparable_to(&right.ty) {
                vec![]
            } else {
                vec![SemanticError::TypeError {
                    expected: left.ty.clone(),
                    found: right.ty.clone(),
                    span: right.span,
                }]
            }
        }
        Plus | Minus | Mul | Div | Lt | Le | Gt | Ge | And | Or => {
            let mut errors = vec![];
            if !left.ty.is_subtype_of(&Ty::Int) {
                errors.push(SemanticError::TypeError {
                    expected: types::Ty::Int,
                    found: left.ty.clone(),
                    span: left.span,
                });
            }
            if !right.ty.is_subtype_of(&Ty::Int) {
                errors.push(SemanticError::TypeError {
                    expected: types::Ty::Int,
                    found: right.ty.clone(),
                    span: right.span,
                });
            }
            errors
        }
    };

    let expr = TypedExpr {
        ty: types::Ty::Int,
        span: left.span.merge(&right.span),
        expr: if errors.is_empty() {
            binary_operator(op, left.expr, right.expr)
        } else {
            error()
        },
    };

    (expr, errors)
}

fn declare_variable<F: Frame + Clone + PartialEq>(
    symbol: Symbol,
    ty: types::Ty,
    escape: bool,
    level: &mut Level<F>,
    env: &mut Environment<F>,
) -> Access<F> {
    let access = alloc_local(level, escape);
    let entry = ValueEntry::Variable {
        ty,
        access: access.clone(),
    };

    env.values.insert(symbol, entry);
    access
}

impl From<&ast::TypeId> for Symbol {
    fn from(id: &ast::TypeId) -> Self {
        Self::from(id.0.as_str())
    }
}

impl From<&ast::Id> for Symbol {
    fn from(id: &ast::Id) -> Self {
        Self::from(id.0.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Expr, ast::*, frame::RiscVFrame as Frame, types, Span, Spanned};

    #[test]
    fn valid_variable() {
        let input = Program(spanned(
            0,
            6,
            Expr::Let(Let {
                decs: vec![spanned(
                    0,
                    4,
                    Dec::VarDec(VarDec::new(
                        spanned(0, 1, Id("x".to_string())),
                        None,
                        spanned(2, 3, Expr::Num(0)),
                    )),
                )],
                body: Box::new(spanned(5, 6, Expr::variable("x"))),
            }),
        ));

        let (typed_expr, errors) = trans_program::<Frame>(input);

        let expected_ty = types::Ty::Int;
        let expected_errors = vec![];

        assert_eq!(typed_expr.ty, expected_ty);
        assert_eq!(errors, expected_errors);
    }

    #[test]
    fn type_error() {
        let input = Program(spanned(
            0,
            6,
            Expr::Let(Let {
                decs: vec![spanned(
                    0,
                    4,
                    Dec::VarDec(VarDec::new(
                        spanned(0, 1, Id("x".to_string())),
                        Some(spanned(1, 2, TypeId("string".to_string()))),
                        spanned(2, 3, Expr::Num(0)),
                    )),
                )],
                body: Box::new(spanned(5, 6, Expr::variable("x"))),
            }),
        ));

        let (typed_expr, errors) = trans_program::<Frame>(input);

        let expected_ty = types::Ty::String;
        let expected_errors = vec![SemanticError::TypeError {
            expected: types::Ty::String,
            found: types::Ty::Int,
            span: Span::new(2, 3),
        }];

        assert_eq!(typed_expr.ty, expected_ty);
        assert_eq!(errors, expected_errors);
    }

    fn spanned<T>(start: usize, end: usize, value: T) -> Spanned<T> {
        Spanned::new(value, Span::new(start, end))
    }
}
