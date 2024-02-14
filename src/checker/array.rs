use crate::{types::Ty, Span};

use super::{array_index, SemanticError, TypedExpr};

pub fn array_index_access(
    maybe_array: TypedExpr,
    maybe_index: TypedExpr,
    span: Span,
    errors: &mut Vec<SemanticError>,
) -> TypedExpr {
    let element_ty = match maybe_array.ty {
        Ty::Array(_, element_ty) => *element_ty,
        Ty::Unknown => Ty::Unknown,
        _ => {
            errors.push(SemanticError::UnexpectedNonArray {
                found: maybe_array.ty.clone(),
                span: maybe_array.span,
            });
            Ty::Unknown
        }
    };

    if !maybe_index.ty.is_subtype_of(&Ty::Int) {
        errors.push(SemanticError::TypeError {
            expected: Ty::Int,
            found: maybe_index.ty.clone(),
            span: maybe_index.span,
        });
    }

    TypedExpr {
        expr: array_index(maybe_array.expr, maybe_index.expr),
        ty: element_ty,
        span,
    }
}
