use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::types::RecordFields;

use super::document::{Span, Spanned};
use super::env::ValueEntry;
use super::frame::Frame;
use super::symbol::Symbol;
use super::temp::Label;
use super::translate::{
    alloc_local, array_init, error, field_access, function_call, literal_number, negation,
    sequence, simple_var, unit, Level,
};
use super::types::{FunctionSignature, IdGenerator, Ty};
use super::{
    ast,
    env::{Environment, Scope, TypeTable},
    ir, types,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    pub expr: ir::Expr,
    pub ty: types::Ty,
}

pub fn trans_program<F: Frame + Clone + PartialEq>(
    program: ast::Program,
) -> (TypedExpr, Vec<SemanticError>) {
    let mut analyzer = Checker::new();
    let mut env = Environment::<F>::base();
    let mut level = Level::<F>::outermost();

    let typed_expr = analyzer.trans_expr(program.0, &mut level, &mut env);

    (typed_expr, analyzer.errors)
}

struct Checker {
    /// Any method in the `Checker` must push to this vector as it encounters semantic errors in the input program.
    /// Elements in this vector must not be removed or modified.
    errors: Vec<SemanticError>,
    id_generator: IdGenerator,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            errors: vec![],
            id_generator: IdGenerator::new(),
        }
    }

    pub fn trans_expr<F: Frame + Clone + PartialEq>(
        &mut self,
        expr: Spanned<ast::Expr>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        match expr.value {
            ast::Expr::Let(expr) => self.trans_let(expr, parent_level, env),
            ast::Expr::LValue(var) => self.trans_var(*var, expr.span, parent_level, env),
            ast::Expr::Seq(exprs) => self.trans_seq(exprs, parent_level, env),

            ast::Expr::NoValue => TypedExpr {
                expr: unit(),
                ty: types::Ty::Unit,
            },
            ast::Expr::Nil => TypedExpr {
                expr: todo!(),
                ty: types::Ty::Nil,
            },
            ast::Expr::Num(n) => TypedExpr {
                expr: literal_number(n),
                ty: types::Ty::Int,
            },
            ast::Expr::String(_) => TypedExpr {
                expr: todo!(),
                ty: types::Ty::String,
            },
            ast::Expr::Break => TypedExpr {
                expr: todo!(),
                ty: types::Ty::Unit,
            },

            ast::Expr::Array(array) => self.trans_array(*array, parent_level, env),
            ast::Expr::Record(record) => self.trans_record(record, parent_level, env),
            ast::Expr::Assign(_) => todo!(),
            ast::Expr::Neg(arg) => {
                let arg = self.trans_specific_type_expr(types::Ty::Int, *arg, parent_level, env);
                TypedExpr {
                    expr: negation(arg.expr),
                    ty: types::Ty::Int,
                }
            }
            ast::Expr::BiOp(_, _, _) => todo!(),
            ast::Expr::FuncCall(name, args) => self.trans_func_call(name, args, parent_level, env),
            ast::Expr::If(_) => todo!(),
            ast::Expr::While(_) => todo!(),
            ast::Expr::For(_) => todo!(),
        }
    }

    /// Translates an array expression like `intArray [10] of 0`.
    fn trans_array<F: Frame + Clone + PartialEq>(
        &mut self,
        array: ast::Array,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let init_span = array.init.span.clone();
        let ty_span = array.ty.span.clone();

        let ty = self.lookup_type(array.ty, &env.types);

        let size = self.trans_specific_type_expr(Ty::Int, array.size, parent_level, env);
        let init = self.trans_expr(array.init, parent_level, env);

        if let types::Ty::Array(_, ref element_ty) = ty {
            let element_ty = element_ty.as_ref();

            if init.ty.is_subtype_of(&element_ty) {
                TypedExpr {
                    expr: array_init(size.expr, init.expr),
                    ty,
                }
            } else {
                self.errors.push(SemanticError::TypeError {
                    expected: element_ty.clone(),
                    found: init.ty,
                    span: init_span,
                });
                TypedExpr { expr: error(), ty }
            }
        } else {
            self.errors.push(SemanticError::UnexpectedNonArray {
                found: ty,
                span: ty_span,
            });
            TypedExpr {
                expr: error(),
                ty: types::Ty::Unknown,
            }
        }
    }

    /// Translates a record expression like `point { x = 0, y = 0 }`.
    fn trans_record<F: Frame + Clone + PartialEq>(
        &mut self,
        record: ast::Record,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        // Possible cases:
        // 1. The given type ID is not defined.
        // 2. The given type ID is not a record type.
        // 3. The given type ID is a record type.

        let ast::Record { fields, ty } = record;
        let ty_span = ty.span.clone();
        let ty = self.lookup_type(ty, &env.types);

        let actual_fields: HashMap<Symbol, Spanned<TypedExpr>> = fields
            .into_iter()
            .map(|field| {
                let key = Symbol::from(field.key.value);
                let span = field.value.span;
                let value = self.trans_expr(field.value, parent_level, env);
                (key, Spanned::new(value, span))
            })
            .collect();

        match ty {
            // The case 3.
            types::Ty::Record(_, ref expected_fields) => {
                let errors = check_record_fields(
                    &actual_fields,
                    expected_fields,
                    Spanned::new(ty.clone(), ty_span),
                    ty_span,
                );

                if errors.is_empty() {
                    TypedExpr { expr: todo!(), ty }
                } else {
                    self.errors.extend(errors);
                    TypedExpr {
                        expr: error(),
                        ty: types::Ty::Unknown,
                    }
                }
            }

            // The case 1.
            types::Ty::Unknown => TypedExpr {
                expr: error(),
                ty: types::Ty::Unknown,
            },

            // The case 2.
            _ => {
                self.errors.push(SemanticError::UnexpectedNonRecord {
                    found: ty,
                    span: ty_span,
                });

                TypedExpr {
                    expr: error(),
                    ty: types::Ty::Unknown,
                }
            }
        }
    }

    fn lookup_type(&mut self, type_id: Spanned<ast::TypeId>, env: &TypeTable) -> types::Ty {
        let symbol = Symbol::from(&type_id.value);
        env.get(&symbol).cloned().unwrap_or_else(|| {
            self.errors.push(SemanticError::UndefinedType {
                name: symbol,
                span: type_id.span,
            });
            types::Ty::Unknown
        })
    }

    fn trans_func_call<F: Frame + Clone + PartialEq>(
        &mut self,
        name: Spanned<ast::Id>,
        args: Vec<Spanned<ast::Expr>>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let args_typed = args
            .into_iter()
            .map(|arg| {
                let span = arg.span;
                Spanned::new(self.trans_expr(arg, parent_level, env), span)
            })
            .collect();

        let signature = get_function(&name, env, &mut self.errors);

        // TODO: We cannot correctly calculate the span of the whole arguments with current AST structure.
        let whole_args_span = name.span;

        match signature {
            Some((FunctionSignature { params, result }, label)) => {
                let arg_type_errors =
                    check_function_arg_types(&args_typed, &params, whole_args_span);
                self.errors.extend(arg_type_errors);

                let translated_args = args_typed.into_iter().map(|arg| arg.value.expr).collect();

                TypedExpr {
                    expr: function_call(label.clone(), translated_args),
                    ty: result.clone(),
                }
            }

            None => TypedExpr {
                expr: error(),
                ty: types::Ty::Unknown,
            },
        }
    }

    fn trans_specific_type_expr<F: Frame + Clone + PartialEq>(
        &mut self,
        expected_ty: types::Ty,
        expr: Spanned<ast::Expr>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let span = expr.span;
        let TypedExpr { ty: found_ty, expr } = self.trans_expr(expr, parent_level, env);

        if found_ty.is_subtype_of(&expected_ty) {
            TypedExpr {
                ty: expected_ty,
                expr,
            }
        } else {
            self.errors.push(SemanticError::TypeError {
                expected: expected_ty,
                found: found_ty,
                span,
            });
            TypedExpr {
                expr: error(),
                ty: types::Ty::Unknown,
            }
        }
    }

    fn trans_var<F: Frame + Clone + PartialEq>(
        &mut self,
        var: ast::LValue,
        span: Span,
        parent_level: &Level<F>,
        env: &Environment<F>,
    ) -> TypedExpr {
        match var {
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
                        }
                    }
                }
            }

            ast::LValue::RecordField(lvalue, field) => {
                let TypedExpr {
                    expr: record,
                    ty: record_ty,
                } = self.trans_var(lvalue.value, lvalue.span, parent_level, env);

                let resolved = lookup_field(record_ty, Symbol::from(field.value), span);

                match resolved {
                    Ok(ResolvedRecordField { index, ty }) => TypedExpr {
                        expr: field_access(record, index),
                        ty,
                    },
                    Err(err) => {
                        self.errors.push(err);
                        TypedExpr {
                            expr: error(),
                            ty: types::Ty::Unknown,
                        }
                    }
                }
            }
            ast::LValue::ArrayIndex(_, _) => todo!(),
        }
    }

    fn trans_seq<F: Frame + Clone + PartialEq>(
        &mut self,
        sub_exprs: Vec<Spanned<ast::Expr>>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let results = sub_exprs
            .into_iter()
            .map(|sub_expr| self.trans_expr(sub_expr, parent_level, env))
            .collect::<Vec<_>>();

        match results.split_last() {
            // `sub_exprs` is empty, return no value.
            None => TypedExpr {
                expr: unit(),
                ty: types::Ty::Unit,
            },

            Some((last, exprs)) => TypedExpr {
                expr: sequence(exprs, last),
                ty: last.ty.clone(),
            },
        }
    }

    fn trans_let<F: Frame + Clone + PartialEq>(
        &mut self,
        expr: ast::Let,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) -> TypedExpr {
        let ast::Let { decs, body } = expr;
        let mut scope = Scope::new(env);

        for dec in decs {
            self.trans_dec(dec, parent_level, &mut scope);
        }

        self.trans_expr(*body, parent_level, &mut scope)
    }

    /// Type-check the given declaration and add the resulting binding to the environment.
    fn trans_dec<F: Frame + Clone + PartialEq>(
        &mut self,
        dec: Spanned<ast::Dec>,
        parent_level: &mut Level<F>,
        env: &mut Environment<F>,
    ) {
        let span = dec.span;

        match dec.value {
            ast::Dec::TypeDec(ast::TyDec {
                id: Spanned { span: _, value: id },
                ty: Spanned { span: _, value: ty },
            }) => {
                env.types.insert(
                    Symbol::from(&id),
                    trans_type(ty, &env.types, &mut self.id_generator),
                );
            }

            ast::Dec::VarDec(ast::VarDec {
                id: Spanned { value: id, .. },
                ty: declared_ty,
                expr,
                escape,
            }) => {
                let symbol = Symbol::from(&id);
                let declared_ty = declared_ty
                    .and_then(|type_id| env.types.get(&Symbol::from(&type_id.value)))
                    .cloned();
                let rhs_span = expr.span;
                let rhs = self.trans_expr(*expr, parent_level, env);

                let ty = match (declared_ty, rhs.ty) {
                    (Some(declared), rhs_ty) => {
                        if !rhs_ty.is_subtype_of(&declared) {
                            self.errors.push(SemanticError::TypeError {
                                expected: declared.clone(),
                                found: rhs_ty,
                                span: rhs_span,
                            });
                        }
                        declared
                    }

                    (None, Ty::Nil) => {
                        self.errors.push(SemanticError::UntypedNilError { span });
                        Ty::Unknown
                    }

                    (None, rhs_ty) => rhs_ty,
                };

                let access = alloc_local(parent_level, escape);

                env.values
                    .insert(symbol, ValueEntry::Variable { ty, access });
            }

            ast::Dec::FnDec(ast::FnDec {
                name:
                    Spanned {
                        span: _,
                        value: name,
                    },
                params,
                return_type,
                body,
            }) => {
                let symbol = Symbol::from(&name);
                let body_span = body.span;
                let declared_return_ty = return_type
                    .and_then(|type_id| env.types.get(&Symbol::from(&type_id.value)))
                    .cloned();
                let params = trans_function_params(params, &env.types, &mut self.errors);

                let params_types = params.iter().map(|(_, ty, _)| ty.clone()).collect();
                let typed_body = {
                    let mut level = Level::<F>::new(
                        parent_level.clone(),
                        Label::new(),
                        params.iter().map(|(_, _, escape)| *escape).collect(),
                    );

                    let mut scope = Scope::new(env);
                    for (symbol, ty, escape) in params {
                        let access = alloc_local(&mut level, escape);

                        scope
                            .values
                            .insert(symbol, ValueEntry::Variable { ty, access });
                    }
                    self.trans_expr(*body, parent_level, &mut scope)
                };

                let is_return_type_compatible = declared_return_ty
                    .clone()
                    .map_or(true, |ty| typed_body.ty.is_subtype_of(&ty));

                if !is_return_type_compatible {
                    self.errors.push(SemanticError::TypeError {
                        expected: declared_return_ty.clone().unwrap(),
                        found: typed_body.ty.clone(),
                        span: body_span,
                    });
                }

                let return_ty = declared_return_ty.unwrap_or(typed_body.ty);
                env.values
                    .insert(symbol, ValueEntry::func(params_types, return_ty));
            }
        }
    }
}

struct ResolvedRecordField {
    index: usize,
    ty: Ty,
}

/// Tries to find the type of the given field in the given type.
fn lookup_field(
    ty: types::Ty,
    field: Symbol,
    span: Span,
) -> Result<ResolvedRecordField, SemanticError> {
    match ty {
        types::Ty::Record(_, ref fields) => fields
            .get(&field)
            .map(|(i, ty)| ResolvedRecordField {
                index: *i,
                ty: ty.clone(),
            })
            .ok_or_else(|| SemanticError::UndefinedField { ty, field, span }),

        _ => Err(SemanticError::UndefinedField { ty, field, span }),
    }
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
    args: &Vec<Spanned<TypedExpr>>,
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
        .map(|arg| Spanned::new(arg.value.ty.clone(), arg.span))
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

fn check_record_fields(
    field_values: &HashMap<Symbol, Spanned<TypedExpr>>,
    field_types: &RecordFields,
    record_type: Spanned<types::Ty>,
    fields_span: Span,
) -> Vec<SemanticError> {
    // Possible errors:
    // 1. The given record has a field undefined in the record type.
    // 2. The given record has a field with a type different from the record type.
    // 3. The given record is missing a field defined in the record type.

    let mut errors = vec![];

    let mut missing_fields: HashSet<Symbol> = field_types.keys().cloned().collect();

    for (key, expr) in field_values {
        match field_types.get(key) {
            Some((_, expected_ty)) => {
                // The case 2.
                if !expr.value.ty.is_subtype_of(expected_ty) {
                    errors.push(SemanticError::TypeError {
                        expected: expected_ty.clone(),
                        found: expr.value.ty.clone(),
                        span: expr.span,
                    });
                }

                missing_fields.remove(key);
            }

            // The case 1.
            None => {
                errors.push(SemanticError::UndefinedField {
                    ty: record_type.value.clone(),
                    field: key.clone(),
                    span: record_type.span,
                });
            }
        }
    }

    // The case 3.
    if !missing_fields.is_empty() {
        errors.push(SemanticError::MissingRecordFields {
            fields: missing_fields.into_iter().collect(),
            span: fields_span,
        });
    }

    errors
}

/// Looks up the given function name in the environment.
fn get_function<'a, F: Frame + Clone + PartialEq>(
    name: &Spanned<ast::Id>,
    env: &'a Environment<F>,
    errors: &mut Vec<SemanticError>,
) -> Option<(&'a FunctionSignature, &'a Label)> {
    let symbol = Symbol::from(&name.value);

    match env.values.get(&symbol) {
        Some(ValueEntry::Function { signature, label }) => Some((signature, label)),
        Some(ValueEntry::Variable { .. }) => {
            errors.push(SemanticError::UnexpectedVariable {
                name: symbol.clone(),
                span: name.span,
            });
            None
        }
        None => {
            errors.push(SemanticError::UndefinedFunction {
                name: symbol.clone(),
                span: name.span,
            });
            None
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SemanticError {
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    TypeError {
        expected: types::Ty,
        found: types::Ty,
        span: Span,
    },

    #[error("`nil` must be used in a context where its type can be determined.")]
    UntypedNilError { span: Span },

    #[error("Undefined type: {name:?}")]
    UndefinedType { name: Symbol, span: Span },

    #[error("Undefined variable: {name:?}")]
    UndefinedVariable { name: Symbol, span: Span },

    #[error("Undefined function: {name:?}")]
    UndefinedFunction { name: Symbol, span: Span },

    #[error("The type '{ty:?}' does not have the field '{field:?}'")]
    UndefinedField {
        ty: types::Ty,
        field: Symbol,
        span: Span,
    },

    #[error("Attempted to use a function '{name:?}' as a variable")]
    UnexpectedFunction { name: Symbol, span: Span },

    #[error("Attempted to call a variable '{name:?}' as a function")]
    UnexpectedVariable { name: Symbol, span: Span },

    #[error("Expected an array type, found '{found:?}'")]
    UnexpectedNonArray { found: types::Ty, span: Span },

    #[error("Expected a record type, found '{found:?}'")]
    UnexpectedNonRecord { found: types::Ty, span: Span },

    #[error("Missing record fields: {fields:?}")]
    MissingRecordFields { fields: Vec<Symbol>, span: Span },

    #[error("Wrong number of arguments: expected {expected:?}, found {found:?}")]
    WrongNumberOfArguments {
        expected: usize,
        found: usize,
        span: Span,
    },
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
    use crate::{ast::*, frame::RiscVFrame as Frame, types, Span, Spanned};

    use super::*;

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
