use crate::ast::{BinOp, UnaryOp};
use crate::errors::{Diag, Diagnostic};
use crate::span::Span;
use crate::types::Type;

use BuildMIRErr::*;
pub enum BuildMIRErr {
    IdentifiersUnsupported(Span),
    InvalidTypeUnary(UnaryOp, Type),
    InvalidTypeBinary(BinOp, Type, Type),
}

impl Diagnostic for BuildMIRErr {
    fn into_diag(self) -> Diag {
        match self {
            IdentifiersUnsupported(span) => Diag {
                span,
                message: format!("Identifier? I hardly know her."),
            },
            InvalidTypeUnary(op, arg_type) => Diag {
                span: op.span,
                message: format!("Type error: Cannot perform {}{}", op.node, arg_type),
            },
            InvalidTypeBinary(op, left_type, right_type) => Diag {
                span: op.span,
                message: format!(
                    "Type error: Cannot perform {} {} {}",
                    left_type, op.node, right_type
                ),
            },
        }
    }
}
