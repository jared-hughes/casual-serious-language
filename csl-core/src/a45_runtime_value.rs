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
            F64(x) => {
                if 1e-6 <= x.abs() && x.abs() < 1e21 {
                    write!(f, "{}", x)
                } else {
                    write!(f, "{:.e}", x)
                }
            }
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

#[cfg(test)]
mod test_runtime_value {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_display(input: RuntimeValue, expect: Expect) {
        let out = format!("{input}");
        expect.assert_eq(&out)
    }

    #[test]
    fn display_int() {
        check_display(I64(3), expect!["3"]);
        check_display(I64(-1234), expect!["-1234"]);
    }

    #[test]
    fn display_float() {
        check_display(F64(1.0), expect!["1"]);
        check_display(F64(473.0), expect!["473"]);
        check_display(F64(-3.2), expect!["-3.2"]);
        check_display(F64(0.0034), expect!["0.0034"]);
        check_display(F64(std::f64::consts::PI), expect!["3.141592653589793"]);
        check_display(F64(1.3e40), expect!["1.3e40"]);
        check_display(F64(1.23e307), expect!["1.23e307"]);
        check_display(F64(1.23e-307), expect!["1.23e-307"]);
        // cutoff for e notatation
        check_display(F64(0.9999e21), expect!["999900000000000000000"]);
        check_display(F64(1e21), expect!["1e21"]);
        check_display(F64(1.0001e21), expect!["1.0001e21"]);
        check_display(F64(1.0001e-6), expect!["0.0000010001"]);
        check_display(F64(1e-6), expect!["0.000001"]);
        check_display(F64(0.9999e-6), expect!["9.999e-7"]);
        // Special values
        check_display(F64(f64::NEG_INFINITY), expect!["-inf"]);
        check_display(F64(f64::INFINITY), expect!["inf"]);
        check_display(F64(f64::NAN), expect!["NaN"]);
    }

    #[test]
    fn display_misc() {
        check_display(Bool(true), expect!["true"]);
        check_display(Bool(false), expect!["false"]);
        check_display(UnitValue, expect!["()"]);
    }
}
