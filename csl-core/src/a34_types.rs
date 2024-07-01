use std::fmt;

pub(crate) use Type::*;
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Type {
    /// An unsigned 64-bit integer.
    I64,
    /// A 64-bit float (binary64 from IEEE-754).
    F64,
    /// A boolean value
    Bool,
    /// `()` from Rust
    Unit,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            I64 => "i64",
            F64 => "f64",
            Bool => "bool",
            Unit => "()",
        };
        write!(f, "{}", s)
    }
}
