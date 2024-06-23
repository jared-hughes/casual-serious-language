use crate::errors::{Diag, Diagnostic};
use crate::span::Span;
use crate::token;
use ParseErr::*;

pub enum ParseErr {
    ExpectedConsequent(Span),
    OpenParenMissingCloseParen(Span),
    NoUnaryPlusMinus(Span),
    UnexpectedBinaryInitial(Span),
    UnmatchedCloseParen(Span),
    UnexpectedEOF(Span),
    InvalidToken(Span, token::InvalidToken),
}

impl Diagnostic for ParseErr {
    fn into_diag(self) -> Diag {
        match self {
            ExpectedConsequent(span) => Diag {
                span,
                message: format!(
                    "Unexpected token here. A binary operator like + may be preferred."
                ),
            },
            OpenParenMissingCloseParen(span) => Diag {
                span,
                message: format!("Expected to see a ')' here."),
            },
            NoUnaryPlusMinus(span) => Diag {
                span,
                message: format!("Unary '+' or '-' is not supported yet."),
            },
            UnexpectedBinaryInitial(span) => Diag {
                span,
                message: format!("Unexpected binary operator in initial position."),
            },
            UnmatchedCloseParen(span) => Diag {
                span,
                message: format!("What's this ')' doing here? I don't see a '('"),
            },
            UnexpectedEOF(span) => Diag {
                span,
                message: format!("Hold your horses. An EOF already?"),
            },
            InvalidToken(span, token) => Diag {
                span,
                message: format!("{}", token.msg),
            },
        }
    }
}
