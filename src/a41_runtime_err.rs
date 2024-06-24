use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::{OP1, OP2};
use crate::runtime_value::RuntimeValue;
use crate::span::Span;

pub struct TypeAssertionFailedBinary {
    pub op: OP2,
    pub args: (RuntimeValue, RuntimeValue),
}

impl TypeAssertionFailedBinary {
    pub fn up(self) -> RuntimeErrorInner {
        RuntimeErrorInner::TypeAssertionFailedBinary(self)
    }
}

pub struct TypeAssertionFailedUnary {
    pub op: OP1,
    pub args: RuntimeValue,
}

impl TypeAssertionFailedUnary {
    pub fn up(self) -> RuntimeErrorInner {
        RuntimeErrorInner::TypeAssertionFailedUnary(self)
    }
}

use RuntimeErrorInner::*;
pub enum RuntimeErrorInner {
    TypeAssertionFailedBinary(TypeAssertionFailedBinary),
    TypeAssertionFailedUnary(TypeAssertionFailedUnary),
}

pub struct RuntimeError {
    pub inner: RuntimeErrorInner,
    pub span: Span,
}

impl Diagnostic for RuntimeError {
    fn into_diag(self) -> Diag {
        let message = match self.inner {
            TypeAssertionFailedUnary(x) => format!(
                "Runtime Type Error: Tried to execute {:#?} on {}.",
                x.op,
                x.args.to_type()
            ),
            TypeAssertionFailedBinary(x) => format!(
                "Runtime Type Error: Tried to execute {:#?} on {} and {}.",
                x.op,
                x.args.0.to_type(),
                x.args.1.to_type()
            ),
        };
        return Diag {
            span: self.span,
            message,
        };
    }
}
