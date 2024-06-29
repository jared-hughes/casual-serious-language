use crate::ast::*;
use crate::errors::{Diag, Diagnostic};
use crate::span::Span;
use crate::types::Type;

pub struct ArgCountWrong {
    pub fn_name: String,
    pub count_expected: usize,
    pub count_actual: usize,
}

use BuildMIRErr::*;
pub enum BuildMIRErr {
    IdentifierNotFound(Span, String),
    FunctionNotFound(Span, String),
    CallNotIdent(Span),
    FnInExpr(Span),
    TopLevelExpr(Span),
    UnrecognizedTypeName(Span, String),
    MisplacedRet(Span),
    MisplacedLet(Span),
    MissingRet(Span),
    DuplicateParameter(Span, String),
    DuplicateDefinition(Span, String),
    DuplicateFnName(Span, String),
    /* Type errors */
    InvalidTypeUnary(UnaryOp, Type),
    InvalidTypeBinary(BinOp, Type, Type),
    TooManyArgs(Span, ArgCountWrong),
    TooFewArgs(Span, ArgCountWrong),
    // (sp, actual, expected)
    WrongArgType(Span, String, Type, Type),
    WrongReturnType(Span, String, Type, Type),
}

impl BuildMIRErr {
    pub fn from_arg_count(span: Span, acw: ArgCountWrong) -> Self {
        if acw.count_actual > acw.count_expected {
            Self::TooManyArgs(span, acw)
        } else {
            Self::TooFewArgs(span, acw)
        }
    }
}

impl Diagnostic for BuildMIRErr {
    fn into_diag(self) -> Diag {
        match self {
            IdentifierNotFound(span, name) => Diag {
                span,
                message: format!(
                    "Identifier '{name}' not found in the local scope of the function."
                ),
            },
            FunctionNotFound(span, name) => Diag {
                span,
                message: format!(
                    "Function '{name}' not found. Try defining it with \
                    `fn {name}(x: u64) -> u64 {{ ret x * x; }}`"
                ),
            },
            CallNotIdent(span) => Diag {
                span,
                message: format!("Function name must be an identifier."),
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
            FnInExpr(span) => Diag {
                span,
                message: format!(
                    "Function expressions are not yet supported. \
                    Try moving this to be a global function."
                ),
            },
            TopLevelExpr(span) => Diag {
                span,
                message: format!(
                    "Top-level exprs are not yet supported. \
                    Try a main function instead: `fn main() -> i64 {{ ret 1 + 2; }}`"
                ),
            },
            UnrecognizedTypeName(span, name) => Diag {
                span,
                message: format!(
                    "Type '{}' is not recognized. Try 'i64' or 'f64' instead.",
                    name
                ),
            },
            MisplacedLet(span) => Diag {
                span,
                message: format!(
                    "Misplaced 'let'. For now, 'let' can only be used in statement position."
                ),
            },
            MisplacedRet(span) => Diag {
                span,
                message: format!(
                    "Misplaced 'ret'. The keyword 'ret' can only be \
                    applied to the final statement in a block."
                ),
            },
            MissingRet(span) => Diag {
                span,
                message: format!(
                    "Unit type '()' has not yet been implemented, so \
                    all functions must have a return. Try adding 'ret'."
                ),
            },
            TooManyArgs(span, acw) => Diag {
                span,
                message: format!(
                    "Too many args for function '{}': expected {} but got {}.",
                    acw.fn_name, acw.count_expected, acw.count_actual
                ),
            },
            TooFewArgs(span, acw) => Diag {
                span,
                message: format!(
                    "Too few args for function '{}': expected {} but got only {}.",
                    acw.fn_name, acw.count_expected, acw.count_actual
                ),
            },
            WrongArgType(span, fn_name, actual, expected) => Diag {
                span,
                message: format!(
                    "Function '{}' expected an argument of type '{}' here, \
                    but you passed in '{}'",
                    fn_name, expected, actual
                ),
            },
            WrongReturnType(span, fn_name, actual, expected) => Diag {
                span,
                message: format!(
                    "Expected function '{}' to return type '{}', \
                    but it returned type '{}'",
                    fn_name, expected, actual
                ),
            },
            DuplicateDefinition(span, name) => Diag {
                span,
                message: format!("Cannot redefine '{name}' since it is already defined."),
            },
            DuplicateParameter(span, name) => Diag {
                span,
                message: format!("Duplicate parameter '{name}' already defined."),
            },
            DuplicateFnName(span, name) => Diag {
                span,
                message: format!("Duplicate function '{name}' already defined."),
            },
        }
    }
}
