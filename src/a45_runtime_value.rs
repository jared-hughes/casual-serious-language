use crate::errors::Diag;
use crate::types::Type;
use std::fmt;

pub use RuntimeValue::*;
#[derive(Clone, Copy, Debug)]
pub enum RuntimeValue {
    I64(i64),
    F64(f64),
}

pub type RuntimeResult = Result<RuntimeValue, Diag>;

impl RuntimeValue {
    pub fn to_type(&self) -> Type {
        match self {
            I64(_) => Type::I64,
            F64(_) => Type::I64,
        }
    }
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            I64(x) => write!(f, "{x}"),
            F64(x) => write!(f, "{x}"),
        }
    }
}
