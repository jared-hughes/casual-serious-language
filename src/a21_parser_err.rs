use crate::errors::{Diag, Diagnostic};
use ParseErr::*;
pub enum ParseErr {
    UnknownChar(char),
    ExpectedConsequent,
    OpenParenMissingCloseParen,
    NoUnaryPlusMinus,
    UnexpectedBinaryInitial,
    UnmatchedCloseParen,
    UnexpectedEOF,
}

impl Diagnostic for ParseErr {
    fn into_diag(self) -> Diag {
        let message = match self {
            UnknownChar(c) => format!("Invalid character {:x}.", u32::from(c)),
            ExpectedConsequent => {
                format!("Unexpected token here. A binary operator like + may be preferred.")
            }
            OpenParenMissingCloseParen => format!("Expected to see a ')' here."),
            NoUnaryPlusMinus => format!("Unary '+' or '-' is not supported yet."),
            UnexpectedBinaryInitial => format!("Unexpected binary operator in initial position."),
            UnmatchedCloseParen => format!("What's this ')' doing here? I don't see a '('"),
            UnexpectedEOF => format!("Hold your horses. An EOF already?"),
        };
        Diag { message }
    }
}
