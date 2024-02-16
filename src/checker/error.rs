use thiserror::Error;

use crate::{
    ast::{Id, TypeId},
    types::Ty,
    Span,
};

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SemanticError {
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    TypeError { expected: Ty, found: Ty, span: Span },

    #[error("`nil` must be used in a context where its type can be determined.")]
    UntypedNilError { span: Span },

    #[error("Undefined type: {name:?}")]
    UndefinedType { name: TypeId, span: Span },

    #[error("Undefined variable: {name:?}")]
    UndefinedVariable { name: Id, span: Span },

    #[error("Undefined function: {name:?}")]
    UndefinedFunction { name: Id, span: Span },

    #[error("The type '{ty:?}' does not have the field '{field:?}'")]
    UndefinedField { ty: Ty, field: Id, span: Span },

    #[error("Attempted to use a function '{name:?}' as a variable")]
    UnexpectedFunction { name: Id, span: Span },

    #[error("Attempted to call a variable '{name:?}' as a function")]
    UnexpectedVariable { name: Id, span: Span },

    #[error("Expected an array type, found '{found:?}'")]
    UnexpectedNonArray { found: Ty, span: Span },

    #[error("Expected a record type, found '{found:?}'")]
    UnexpectedNonRecord { found: Ty, span: Span },

    #[error("Missing record fields: {fields:?}")]
    MissingRecordFields { fields: Vec<Id>, span: Span },

    #[error("Wrong number of arguments: expected {expected:?}, found {found:?}")]
    WrongNumberOfArguments {
        expected: usize,
        found: usize,
        span: Span,
    },

    #[error("Break expression outside of a loop")]
    BreakOutsideLoop { span: Span },
}
