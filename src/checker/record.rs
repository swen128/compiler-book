use std::collections::HashSet;

use crate::{
    ast::Id,
    symbol::Symbol,
    types::{RecordField, RecordFields, Ty},
    Span, Spanned,
};

use super::{field_access, Expr, SemanticError, TypedExpr};

pub fn record_field_access(
    maybe_record: TypedExpr,
    field: Spanned<Id>,
    span: Span,
) -> Result<TypedExpr, SemanticError> {
    let symbol = Symbol::from(field.value);
    match maybe_record.ty {
        Ty::Record(_, ref fields) => fields
            .get(&symbol)
            .map(|RecordField { index, ty }| TypedExpr {
                ty: ty.clone(),
                expr: field_access(maybe_record.expr, *index),
                span,
            })
            .ok_or_else(|| SemanticError::UndefinedField {
                ty: maybe_record.ty,
                field: symbol,
                span: field.span,
            }),

        _ => Err(SemanticError::UndefinedField {
            ty: maybe_record.ty,
            field: symbol,
            span: field.span,
        }),
    }
}

type TypedField = (Spanned<Symbol>, TypedExpr);

pub fn check_record_type(
    field_values: Spanned<Vec<TypedField>>,
    ty: &Spanned<Ty>,
) -> Result<Vec<Expr>, Vec<SemanticError>> {
    match ty.value {
        Ty::Record(_, ref field_types) => check_record_fields(
            &field_values,
            field_types,
            Spanned::new(ty.value.clone(), ty.span),
        ),

        // The error should have already been reported elsewhere.
        Ty::Unknown => Err(vec![]),

        _ => Err(vec![SemanticError::UnexpectedNonRecord {
            found: ty.value.clone(),
            span: ty.span,
        }]),
    }
}

fn check_record_fields(
    field_values: &Spanned<Vec<TypedField>>,
    field_types: &RecordFields,
    record_type: Spanned<Ty>,
) -> Result<Vec<Expr>, Vec<SemanticError>> {
    // Possible errors:
    // 1. The given record has a field undefined in the record type.
    // 2. The given record has a field with a type different from the record type.
    // 3. The given record is missing a field defined in the record type.

    let mut errors = vec![];
    let mut fields = vec![];
    let mut missing_fields: HashSet<Symbol> = field_types.keys().cloned().collect();

    for (key, expr) in &field_values.value {
        match field_types.get(&key.value) {
            // The case 1.
            None => {
                errors.push(SemanticError::UndefinedField {
                    ty: record_type.value.clone(),
                    field: key.value.clone(),
                    span: key.span.clone(),
                });
            }

            Some(RecordField { index, ty }) => {
                // The case 2.
                if !expr.ty.is_subtype_of(ty) {
                    errors.push(SemanticError::TypeError {
                        expected: ty.clone(),
                        found: expr.ty.clone(),
                        span: expr.span,
                    });
                }

                // The field is valid.
                fields.push((index, expr.expr.clone()));
                missing_fields.remove(&key.value);
            }
        }
    }

    // The case 3.
    if !missing_fields.is_empty() {
        errors.push(SemanticError::MissingRecordFields {
            fields: missing_fields.into_iter().collect(),
            span: field_values.span,
        });
    }

    if errors.is_empty() {
        fields.sort_by_key(|(i, _)| *i);
        Ok(fields.into_iter().map(|(_, expr)| expr).collect())
    } else {
        Err(errors)
    }
}
