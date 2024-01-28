use thiserror::Error;

use crate::types::{FunctionSignature, Ty};

use super::document::{Span, Spanned};
use super::env::ValueEntry;
use super::symbol::Symbol;
use super::types::{IdGenerator, RecordField};
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

fn trans_expr(
    expr: Spanned<ast::Expr>,
    env: &mut Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
) -> TypedExpr {
    match expr.value {
        ast::Expr::Let(expr) => trans_let(expr, env, errors, id_generator),
        ast::Expr::LValue(var) => trans_var(*var, expr.span, env, errors, id_generator),
        ast::Expr::Seq(exprs) => trans_seq(exprs, env, errors, id_generator),

        ast::Expr::NoValue => TypedExpr {
            expr: todo!(),
            ty: types::Ty::Unit,
        },
        ast::Expr::Nil => TypedExpr {
            expr: todo!(),
            ty: types::Ty::Nil,
        },
        ast::Expr::Num(_) => TypedExpr {
            expr: todo!(),
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

        ast::Expr::Array(_) => todo!(),
        ast::Expr::Record(_) => todo!(),
        ast::Expr::Assign(_) => todo!(),
        ast::Expr::Neg(_) => {
            trans_specific_type_expr(types::Ty::Int, expr, env, errors, id_generator)
        }
        ast::Expr::BiOp(_, _, _) => todo!(),
        ast::Expr::FuncCall(name, args) => trans_func_call(name, args, env, errors, id_generator),
        ast::Expr::If(_) => todo!(),
        ast::Expr::While(_) => todo!(),
        ast::Expr::For(_) => todo!(),
    }
}

/// Looks up the given function name in the environment and returns its signature.
fn get_function<'a>(
    name: &Spanned<ast::Id>,
    env: &'a Environment,
    errors: &mut Vec<SemanticError>,
) -> Option<&'a FunctionSignature> {
    let symbol = Symbol::from(&name.value);

    match env.values.get(&symbol) {
        Some(ValueEntry::Function(signature)) => Some(signature),
        Some(ValueEntry::Variable(_)) => {
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

fn trans_func_call(
    name: Spanned<ast::Id>,
    args: Vec<Spanned<ast::Expr>>,
    env: &mut Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
) -> TypedExpr {
    let signature = get_function(&name, env, errors).cloned();

    let args_typed = args.into_iter().map(|arg| {
        let span = arg.span;
        Spanned::new(trans_expr(arg, env, errors, id_generator), span)
    });

    // TODO: We cannot correctly calculate the span of the whole arguments with current AST structure.
    let whole_args_span = name.span;

    match signature {
        Some(FunctionSignature { params, result }) => {
            let arg_type_errors =
                check_function_arg_types(&args_typed.collect(), &params, whole_args_span);
            errors.extend(arg_type_errors);

            TypedExpr {
                expr: todo!(),
                ty: result.clone(),
            }
        }

        None => TypedExpr {
            expr: todo!(),
            ty: types::Ty::Unknown,
        },
    }
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

fn trans_specific_type_expr(
    expected_ty: types::Ty,
    expr: Spanned<ast::Expr>,
    env: &mut Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
) -> TypedExpr {
    let span = expr.span;
    let TypedExpr { ty: found_ty, expr } = trans_expr(expr, env, errors, id_generator);

    if !found_ty.is_subtype_of(&expected_ty) {
        errors.push(SemanticError::TypeError {
            expected: expected_ty.clone(),
            found: found_ty,
            span,
        });
    }

    TypedExpr {
        ty: expected_ty,
        expr,
    }
}

fn trans_var(
    var: ast::LValue,
    span: Span,
    env: &Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
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
                    ValueEntry::Variable(ty) => Ok(TypedExpr {
                        expr: todo!(),
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
                    errors.extend(new_errors);
                    TypedExpr {
                        expr: todo!(),
                        ty: types::Ty::Unknown,
                    }
                }
            }
        }

        ast::LValue::RecordField(_, _) => todo!(),
        ast::LValue::ArrayIndex(_, _) => todo!(),
    }
}

fn trans_seq(
    sub_exprs: Vec<Spanned<ast::Expr>>,
    env: &mut Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
) -> TypedExpr {
    let results = sub_exprs
        .into_iter()
        .map(|sub_expr| trans_expr(sub_expr, env, errors, id_generator))
        .collect::<Vec<_>>();

    match results.last().cloned() {
        // `sub_exprs` is empty, return no value.
        None => TypedExpr {
            expr: todo!(),
            ty: types::Ty::Unit,
        },

        Some(result) => TypedExpr {
            expr: todo!(),
            ty: result.ty,
        },
    }
}

fn trans_let(
    expr: ast::Let,
    env: &mut Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
) -> TypedExpr {
    let ast::Let { decs, body } = expr;
    let mut scope = Scope::new(env);

    for dec in decs {
        trans_dec(dec, &mut scope, errors, id_generator);
    }

    trans_expr(*body, &mut scope, errors, id_generator)
}

/// Type-check the given declaration and add the resulting binding to the environment.
fn trans_dec(
    dec: Spanned<ast::Dec>,
    env: &mut Environment,
    errors: &mut Vec<SemanticError>,
    id_generator: &mut IdGenerator,
) {
    let span = dec.span;

    match dec.value {
        ast::Dec::TypeDec(ast::TyDec {
            id: Spanned { span: _, value: id },
            ty: Spanned { span: _, value: ty },
        }) => {
            env.types
                .insert(Symbol::from(&id), trans_type(ty, &env.types, id_generator));
        }

        ast::Dec::VarDec(ast::VarDec {
            id: Spanned { value: id, .. },
            ty: declared_ty,
            expr,
            ..
        }) => {
            let symbol = Symbol::from(&id);
            let declared_ty = declared_ty
                .and_then(|type_id| env.types.get(&Symbol::from(&type_id.value)))
                .cloned();
            let rhs = trans_expr(*expr, env, errors, id_generator);
            let ty = declared_ty.unwrap_or(rhs.ty);

            env.values.insert(symbol, ValueEntry::Variable(ty));
        }

        ast::Dec::FnDec(ast::FnDec {
            name: Spanned {
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
            let params = trans_function_params(params, &env.types, errors);

            let params_types = params.iter().map(|(_, ty)| ty.clone()).collect();
            let typed_body = {
                let mut scope = Scope::new(env);
                for (symbol, ty) in params {
                    scope.values.insert(symbol, ValueEntry::Variable(ty));
                }
                trans_expr(*body, &mut scope, errors, id_generator)
            };

            let is_return_type_comatible = declared_return_ty
                .clone()
                .map_or(true, |ty| typed_body.ty.is_subtype_of(&ty));

            if !is_return_type_comatible {
                errors.push(SemanticError::TypeError {
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

fn trans_function_params(
    params: Vec<Spanned<ast::TyField>>,
    env: &TypeTable,
    errors: &mut Vec<SemanticError>,
) -> Vec<(Symbol, types::Ty)> {
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

            (param_name, ty)
        })
        .collect();

    entries
}

fn trans_type(ty: ast::Ty, env: &TypeTable, id_generator: &mut IdGenerator) -> types::Ty {
    match ty {
        ast::Ty::Name(id) => trans_type_name(id, env),
        ast::Ty::Record(fields) => {
            let fields = fields
                .into_iter()
                .map(|Spanned { value: field, .. }| RecordField {
                    key: Symbol::from(&field.key.value),
                    ty: trans_type_name(field.ty.value, env),
                })
                .collect();
            types::Ty::Record(id_generator.next(), fields)
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

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SemanticError {
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    TypeError {
        expected: types::Ty,
        found: types::Ty,
        span: Span,
    },

    #[error("Undefined type: {name:?}")]
    UndefinedType { name: Symbol, span: Span },

    #[error("Undefined variable: {name:?}")]
    UndefinedVariable { name: Symbol, span: Span },

    #[error("Undefined function: {name:?}")]
    UndefinedFunction { name: Symbol, span: Span },

    #[error("Attempted to use a function '{name:?}' as a variable")]
    UnexpectedFunction { name: Symbol, span: Span },

    #[error("Attempted to call a variable '{name:?}' as a function")]
    UnexpectedVariable { name: Symbol, span: Span },

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
    use crate::{ast::*, env::Environment, ir, types, types::IdGenerator, Span, Spanned};

    use super::*;

    #[test]
    fn valid_variable() {
        let mut env = Environment::base();
        let mut errors = vec![];
        let mut id_generator = IdGenerator::new();

        let input = spanned(
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
        );

        let output = trans_expr(input, &mut env, &mut errors, &mut id_generator);

        let expected = TypedExpr {
            expr: todo!(),
            ty: types::Ty::Int,
        };
        let expected_errors = vec![];

        assert_eq!(output, expected);
        assert_eq!(errors, expected_errors);
    }

    #[test]
    fn type_error() {
        let mut env = Environment::base();
        let mut errors = vec![];
        let mut id_generator = IdGenerator::new();

        let input = spanned(
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
        );

        let output = trans_expr(input, &mut env, &mut errors, &mut id_generator);

        let expected = TypedExpr {
            expr: todo!(),
            ty: types::Ty::String,
        };
        let expected_errors = vec![SemanticError::TypeError {
            expected: types::Ty::String,
            found: types::Ty::Int,
            span: Span::new(0, 4),
        }];

        assert_eq!(output, expected);
        assert_eq!(errors, expected_errors);
    }

    fn spanned<T>(start: usize, end: usize, value: T) -> Spanned<T> {
        Spanned::new(value, Span::new(start, end))
    }
}
