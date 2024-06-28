use crate::ast::Ident;
use crate::errors::{Diag, Diagnostic};
use crate::span::Span;
use crate::token;

use ParseErrKind::*;
pub enum ParseErrKind {
    /* Expression context */
    ExpectedConsequent,
    OpenParenMissingCloseParen,
    UnaryPlusDisallowed,
    UnexpectedBinaryInitial,
    UnmatchedCloseParen,
    UnexpectedEOF,
    InvalidToken(token::InvalidToken),
    GeneralUnexpected,
    /* Function calls */
    CallExpComma,
    CallExpCloseParen,
    /* Statement context */
    ExpectedSemi,
    /* Function context */
    FnExpectedName,
    // TODO: make this into structs
    // String is function name
    FnExpOpenParen(Ident),
    FnExpParameter(Ident),
    // (function name, parameter name)
    FnExpColon(Ident, Ident),
    FnExpParamType(Ident, Ident),
    FnExpComma(Ident, Ident),
    FnBadComma,
    // Function name
    FnExpCloseParen(Ident),
    FnExpThinArrow(Ident),
    FnExpReturnType(Ident),
    FnExpOpenCurly(Ident),
    FnExpCloseCurly(Ident),
}

pub struct ParseErr {
    pub kind: ParseErrKind,
    pub span: Span,
}

const EXAMPLE_FN: &str = "For example: `fn add(x: u64, y: u64) -> u64 { x + y }`";

impl ParseErrKind {
    fn msg(self) -> String {
        match self {
            /* Expression context */
            ExpectedConsequent => {
                format!("Unexpected token here. A binary operator like + may be preferred.")
            }
            OpenParenMissingCloseParen => format!("Expected to see a ')' here."),
            UnaryPlusDisallowed => format!("Leading '+' is not supported."),
            UnexpectedBinaryInitial => format!("Unexpected binary operator in initial position."),
            UnmatchedCloseParen => format!("What's this ')' doing here? I don't see a '('"),
            UnexpectedEOF => format!("Hold your horses. An EOF already?"),
            InvalidToken(token) => format!("{}", token.msg),
            GeneralUnexpected => format!("Unexpected token."),
            /* Function calls */
            CallExpComma => format!("Expected ',' after function argument."),
            CallExpCloseParen => format!("Expected ')' to end function arguments."),
            FnBadComma => format!("Comma ',' is not allowed before first argument."),
            /* Statement context */
            ExpectedSemi => format!("Expected semicolon to end the statement"),
            /* Functions */
            FnExpectedName => {
                format!("Expected identifier. Functions must have a name. {EXAMPLE_FN}")
            }
            FnExpOpenParen(fn_name) => {
                format!(
                    "Expected '(' to begin parameter declaration \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpParameter(fn_name) => {
                format!(
                    "Expected an identifier to serve as a function parameter \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpColon(fn_name, param) => {
                format!(
                    "Expected ':' to provide the type of the parameter '{param}' \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpParamType(fn_name, param) => {
                format!(
                    "Expected an identifier to provide the type of the parameter '{param}' \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpComma(fn_name, param) => {
                format!(
                    "Expected ',' after function parameter '{param}' \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpCloseParen(fn_name) => {
                format!(
                    "Expected ')' to end function parameters \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpThinArrow(fn_name) => {
                format!(
                    "Expected '->' to declare return type \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpReturnType(fn_name) => {
                format!(
                    "Expected an identifier to provide the return type \
                    for function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpOpenCurly(fn_name) => {
                format!(
                    "Expected '{{' to begin the body \
                    of function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
            FnExpCloseCurly(fn_name) => {
                format!(
                    "Expected '}}' to end the body \
                    of function '{fn_name}'. {EXAMPLE_FN}"
                )
            }
        }
    }

    pub fn span(self, span: Span) -> Diag {
        ParseErr { kind: self, span }.into_diag()
    }
}

impl Diagnostic for ParseErr {
    fn into_diag(self) -> Diag {
        Diag {
            span: self.span,
            message: self.kind.msg(),
        }
    }
}
