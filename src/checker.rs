use thiserror::Error;

use super::ast::TyField;
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
    id_generator: &mut IdGenerator,
) -> Result<TypedExpr, Vec<SemanticError>> {
    match expr.value {
        ast::Expr::Let(expr) => trans_let(expr, env, id_generator),
        ast::Expr::LValue(var) => trans_var(*var, expr.span, env),
        ast::Expr::Seq(exprs) => trans_seq(exprs, env, id_generator),

        ast::Expr::NoValue => Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::Unit,
        }),
        ast::Expr::Nil => Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::Nil,
        }),
        ast::Expr::Num(_) => Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::Int,
        }),
        ast::Expr::String(_) => Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::String,
        }),
        ast::Expr::Break => Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::Unit,
        }),

        ast::Expr::Array(_) => todo!(),
        ast::Expr::Record(_) => todo!(),
        ast::Expr::Assign(_) => todo!(),
        ast::Expr::Neg(_) => trans_specific_type_expr(types::Ty::Int, expr, env, id_generator),
        ast::Expr::BiOp(_, _, _) => todo!(),
        ast::Expr::FuncCall(name, args) => trans_func_call(name, args, env, id_generator),
        ast::Expr::If(_) => todo!(),
        ast::Expr::While(_) => todo!(),
        ast::Expr::For(_) => todo!(),
    }
}

fn trans_func_call(
    name: Spanned<ast::Id>,
    args: Vec<Spanned<ast::Expr>>,
    env: &mut Environment,
    id_generator: &mut IdGenerator,
) -> Result<TypedExpr, Vec<SemanticError>> {
    let (params_ty, return_ty) = env.values.get(&Symbol::from(&name.value)).map_or_else(
        || {
            Err(vec![SemanticError::UndefinedFunction {
                name: Symbol::from(&name.value),
                span: name.span,
            }])
        },
        |entry| match entry {
            ValueEntry::Function { params, result } => Ok((params.clone(), result.clone())),

            ValueEntry::Variable(_) => Err(vec![SemanticError::UnexpectedFunction {
                name: Symbol::from(&name.value),
                span: name.span,
            }]),
        },
    )?;

    if params_ty.len() != args.len() {
        // TODO: Even in this case, we can and should continue type-checking the arguments.
        return Err(vec![SemanticError::WrongNumberOfArguments {
            expected: params_ty.len(),
            found: args.len(),

            // TODO: This should instead be the span of all the arguments.
            //       We cannot infer the span if the `args` is empty, though.
            //       We should fix the AST to make this possible.
            span: name.span,
        }]);
    }

    let args_ty = args
        .into_iter()
        .map(|arg| trans_expr(arg, env, id_generator));

    let (args_ty, errors) = partition(args_ty);

    todo!()
}

fn partition<T, E>(iter: impl Iterator<Item = Result<T, E>>) -> (Vec<T>, Vec<E>) {
    iter.fold((vec![], vec![]), |(mut ok, mut err), result| match result {
        Ok(x) => {
            ok.push(x);
            (ok, err)
        }
        Err(e) => {
            err.push(e);
            (ok, err)
        }
    })
}

fn trans_specific_type_expr(
    expected_ty: types::Ty,
    expr: Spanned<ast::Expr>,
    env: &mut Environment,
    id_generator: &mut IdGenerator,
) -> Result<TypedExpr, Vec<SemanticError>> {
    let span = expr.span;
    let result = trans_expr(expr, env, id_generator);

    match result {
        Ok(TypedExpr { ty: found_ty, expr }) if found_ty.is_subtype_of(&expected_ty) => {
            Ok(TypedExpr {
                expr,
                ty: expected_ty,
            })
        }
        Ok(TypedExpr { ty: found_ty, .. }) => Err(vec![SemanticError::TypeError {
            expected: expected_ty,
            found: found_ty,
            span,
        }]),
        Err(errors) => Err(errors),
    }
}

fn trans_var(
    var: ast::LValue,
    span: Span,
    env: &Environment,
) -> Result<TypedExpr, Vec<SemanticError>> {
    match var {
        ast::LValue::Variable(id) => env.values.get(&Symbol::from(&id)).map_or_else(
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
        ),

        ast::LValue::RecordField(_, _) => todo!(),
        ast::LValue::ArrayIndex(_, _) => todo!(),
    }
}

fn trans_seq(
    sub_exprs: Vec<Spanned<ast::Expr>>,
    env: &mut Environment,
    id_generator: &mut IdGenerator,
) -> Result<TypedExpr, Vec<SemanticError>> {
    let results = sub_exprs
        .into_iter()
        .map(|sub_expr| trans_expr(sub_expr, env, id_generator))
        .collect::<Vec<_>>();

    match results.last().cloned() {
        // `sub_exprs` is empty, return no value.
        None => Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::Unit,
        }),

        Some(result) => {
            let errors = results
                .into_iter()
                .filter_map(Result::err)
                .flatten()
                .collect::<Vec<_>>();

            match result {
                Ok(expr) if errors.is_empty() => Ok(expr),
                _ => Err(errors),
            }
        }
    }
}

fn trans_let(
    expr: ast::Let,
    env: &mut Environment,
    id_generator: &mut IdGenerator,
) -> Result<TypedExpr, Vec<SemanticError>> {
    let ast::Let { decs, body } = expr;

    let mut scope = Scope::new(env);
    let mut errors: Vec<SemanticError> = decs
        .into_iter()
        .flat_map(|dec| trans_dec(dec, &mut scope, id_generator))
        .collect();

    match trans_expr(*body, &mut scope, id_generator) {
        Ok(x) => {
            if errors.is_empty() {
                Ok(x)
            } else {
                Err(errors)
            }
        }

        Err(new_errors) => {
            errors.extend(new_errors);
            Err(errors)
        }
    }
}

/// Type-check the given declaration and add the resulting binding to the environment.
fn trans_dec(
    dec: Spanned<ast::Dec>,
    env: &mut Environment,
    id_generator: &mut IdGenerator,
) -> Vec<SemanticError> {
    let span = dec.span;

    match dec.value {
        ast::Dec::TypeDec(ast::TyDec {
            id: Spanned { span: _, value: id },
            ty: Spanned { span: _, value: ty },
        }) => {
            env.types
                .insert(Symbol::from(&id), trans_type(ty, &env.types, id_generator));
            vec![]
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
            let rhs = trans_expr(*expr, env, id_generator);

            let mut errors = vec![];

            let ty = match (declared_ty, rhs) {
                // Return type omitted, infer from right-hand side.
                (None, Ok(TypedExpr { ty, .. })) => ty,

                (Some(declared_ty), Ok(TypedExpr { ty, .. })) => {
                    if !ty.is_subtype_of(&declared_ty) {
                        errors.push(SemanticError::TypeError {
                            expected: declared_ty.clone(),
                            found: ty,
                            span,
                        });
                    }
                    declared_ty
                }

                (Some(declared_ty), Err(new_errors)) => {
                    errors.extend(new_errors);
                    declared_ty
                }

                (None, Err(new_errors)) => {
                    errors.extend(new_errors);
                    types::Ty::Unknown
                }
            };
            env.values.insert(symbol, ValueEntry::Variable(ty));

            errors
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
            let return_ty = return_type
                .and_then(|type_id| env.types.get(&Symbol::from(&type_id.value)))
                .cloned();
            let (params, mut errors) = trans_function_params(params, &env.types);

            let params_types = params.iter().map(|(_, ty)| ty.clone()).collect();
            let body_type = {
                let mut scope = Scope::new(env);
                for (symbol, ty) in params {
                    scope.values.insert(symbol, ValueEntry::Variable(ty));
                }

                match trans_expr(*body, &mut scope, id_generator) {
                    Ok(TypedExpr { ty, .. }) => ty,

                    Err(new_errors) => {
                        errors.extend(new_errors);
                        types::Ty::Unknown
                    }
                }
            };

            match return_ty {
                None => {
                    env.values.insert(
                        symbol,
                        ValueEntry::Function {
                            params: params_types,
                            result: body_type,
                        },
                    );
                    errors
                }
                Some(return_ty) => {
                    if !body_type.is_subtype_of(&return_ty) {
                        errors.push(SemanticError::TypeError {
                            expected: return_ty.clone(),
                            found: body_type,
                            span,
                        });
                    }
                    env.values.insert(
                        symbol,
                        ValueEntry::Function {
                            params: params_types,
                            result: return_ty,
                        },
                    );
                    errors
                }
            }
        }
    }
}

fn trans_function_params(
    params: Vec<Spanned<ast::TyField>>,
    env: &TypeTable,
) -> (Vec<(Symbol, types::Ty)>, Vec<SemanticError>) {
    let mut errors = vec![];

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

    (entries, errors)
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
    use crate::{
        ast::*, env::Environment, ir, types, types::IdGenerator, Span, Spanned,
    };

    use super::{trans_expr, TypedExpr};

    #[test]
    fn valid_variable() {
        let mut env = Environment::base();
        let mut id_generator = IdGenerator::new();

        let input = spanned(
            0,
            6,
            Expr::Let(Let {
                decs: vec![spanned(
                    0,
                    4,
                    Dec::VarDec(VarDec {
                        id: spanned(0, 1, Id("x".to_string())),
                        ty: None,
                        expr: Box::new(spanned(2, 3, Expr::Num(0))),
                    }),
                )],
                body: Box::new(spanned(5, 6, Expr::variable("x"))),
            }),
        );

        let output = trans_expr(input, &mut env, &mut id_generator);

        let expected = Ok(TypedExpr {
            expr: todo!(),
            ty: types::Ty::Int,
        });

        assert_eq!(output, expected);
    }

    #[test]
    fn type_error() {
        let mut env = Environment::base();
        let mut id_generator = IdGenerator::new();

        let input = spanned(
            0,
            6,
            Expr::Let(Let {
                decs: vec![spanned(
                    0,
                    4,
                    Dec::VarDec(VarDec {
                        id: spanned(0, 1, Id("x".to_string())),
                        ty: Some(spanned(1, 2, TypeId("string".to_string()))),
                        expr: Box::new(spanned(2, 3, Expr::Num(0))),
                    }),
                )],
                body: Box::new(spanned(5, 6, Expr::variable("x"))),
            }),
        );

        let output = trans_expr(input, &mut env, &mut id_generator);

        let expected = Err(vec![super::SemanticError::TypeError {
            expected: types::Ty::String,
            found: types::Ty::Int,
            span: Span::new(0, 4),
        }]);

        assert_eq!(output, expected);
    }

    fn spanned<T>(start: usize, end: usize, value: T) -> Spanned<T> {
        Spanned::new(value, Span::new(start, end))
    }
}
