use crate::errors::{Diag, Diagnostic};

use BuildMIRErr::*;
pub enum BuildMIRErr {
    IdentifiersUnsupported,
}

impl Diagnostic for BuildMIRErr {
    fn into_diag(self) -> Diag {
        let message = match self {
            IdentifiersUnsupported => format!("Identifier? I hardly know her."),
        };
        Diag { message }
    }
}
