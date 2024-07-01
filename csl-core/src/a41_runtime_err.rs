use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::{OP1, OP2};
use crate::runtime_value::RuntimeValue;
use crate::span::Span;

pub(crate) struct TypeAssertionFailedBinary {
    pub(crate) op: OP2,
    pub(crate) args: (RuntimeValue, RuntimeValue),
}

impl TypeAssertionFailedBinary {
    pub(crate) fn up(self) -> RuntimeErrorInner {
        RuntimeErrorInner::TypeAssertionFailedBinary(self)
    }
}

pub(crate) struct TypeAssertionFailedUnary {
    pub(crate) op: OP1,
    pub(crate) args: RuntimeValue,
}

impl TypeAssertionFailedUnary {
    pub(crate) fn up(self) -> RuntimeErrorInner {
        RuntimeErrorInner::TypeAssertionFailedUnary(self)
    }
}

use RuntimeErrorInner::*;
pub(crate) enum RuntimeErrorInner {
    TypeAssertionFailedBinary(TypeAssertionFailedBinary),
    TypeAssertionFailedUnary(TypeAssertionFailedUnary),
    MissingFunction(String),
    IncorrectArgs(String),
}

impl RuntimeErrorInner {
    pub(crate) fn span(self, span: Span) -> Diag {
        RuntimeError { inner: self, span }.into_diag()
    }
}

pub(crate) struct RuntimeError {
    pub(crate) inner: RuntimeErrorInner,
    pub(crate) span: Span,
}

impl Diagnostic for RuntimeError {
    fn into_diag(self) -> Diag {
        let message = match self.inner {
            TypeAssertionFailedUnary(x) => format!(
                "Runtime Type Error: Tried to execute {:#?} on {}.",
                x.op,
                x.args.get_type()
            ),
            TypeAssertionFailedBinary(x) => format!(
                "Runtime Type Error: Tried to execute {:#?} on {} and {}.",
                x.op,
                x.args.0.get_type(),
                x.args.1.get_type()
            ),
            MissingFunction(name) => {
                format!(
                    "Function '{name}' is not defined. \
                    Try `fn main() -> i64 {{ ret 1 + 2; }}`"
                )
            }
            IncorrectArgs(name) => {
                format!("Incorrect args to function '{name}")
            }
        };
        Diag {
            span: self.span,
            message,
        }
    }
}
