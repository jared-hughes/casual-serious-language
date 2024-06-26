use crate::errors::Diag;
use crate::mir::Lit;
use crate::types::Type;
use std::fmt;

pub use RuntimeValue::*;
#[derive(Clone, Copy, Debug)]
pub enum RuntimeValue {
    I64(i64),
    F64(f64),
    Bool(bool),
    UnitValue,
}

pub type RuntimeResult = Result<RuntimeValue, Diag>;

impl RuntimeValue {
    pub(crate) fn get_type(&self) -> Type {
        match self {
            I64(_) => Type::I64,
            F64(_) => Type::F64,
            Bool(_) => Type::Bool,
            UnitValue => Type::Unit,
        }
    }
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            I64(x) => write!(f, "{x}"),
            F64(x) => write!(f, "{x}"),
            Bool(x) => write!(f, "{x}"),
            UnitValue => write!(f, "()"),
        }
    }
}

pub(crate) fn cook_lit(lit: Lit) -> RuntimeValue {
    match lit {
        Lit::Integer(x) => I64(x),
        Lit::Float(x) => F64(x),
        Lit::Unit => UnitValue,
    }
}
