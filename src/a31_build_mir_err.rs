use crate::errors::{Diag, Diagnostic};
use crate::span::Span;

use BuildMIRErr::*;
pub enum BuildMIRErr {
    IdentifiersUnsupported(Span),
}

impl Diagnostic for BuildMIRErr {
    fn into_diag(self) -> Diag {
        match self {
            IdentifiersUnsupported(span) => Diag {
                span,
                message: format!("Identifier? I hardly know her."),
            },
        }
    }
}
