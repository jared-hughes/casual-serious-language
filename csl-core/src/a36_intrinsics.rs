use crate::runtime_err::*;
use crate::runtime_value::*;
use crate::types::Type;

type FunctionResult = Result<RuntimeValue, RuntimeErrorInner>;

pub(crate) struct Intrinsic1 {
    pub(crate) param_types: Type,
    pub(crate) return_type: Type,
    pub(crate) compute: fn(RuntimeValue) -> FunctionResult,
}

pub(crate) struct Intrinsic2 {
    pub(crate) param_types: (Type, Type),
    pub(crate) return_type: Type,
    pub(crate) compute: fn(RuntimeValue, RuntimeValue) -> FunctionResult,
}

macro_rules! def_int {
    (
        [$OP:ident, $Intrinsic:ident, $TypeAssertionFailed:ident]
        $(
            $op:ident =>
            fn $fn:ident($($a:ident = $av:ident: $at:ident),*) -> $ret:ident
                $body:block
        )+
    ) => {
        #[derive(Clone, Copy, Debug)]
        pub(crate) enum $OP {
            $($op),*
        }

        impl $OP {
            // Lookup into the functions
            pub(crate) fn get_intrinsic(self) -> $Intrinsic {
                match self {
                    $(
                        $OP::$op => $Intrinsic {
                            param_types: ($(Type::$at),*),
                            return_type: Type::$ret,
                            compute: Self::$fn,
                        },
                    )+
                }
            }

            // Define the functions
            $(fn $fn($($av: RuntimeValue),*) -> FunctionResult {
                // One-arg tuple syntax is just unused parens.
                #[allow(unused_parens)]
                if let ($($at($a)),*) = ($($av),*) {
                    Ok($ret($body))
                } else {
                    Err($TypeAssertionFailed{op: $OP::$op, args: ($($av),*)}.up())
                }
            })+
        }
    };
}

def_int! {
[OP1, Intrinsic1, TypeAssertionFailedUnary]
NegI64 => fn add_i64(x=xv: I64) -> I64 {
    -x
}
NegF64 => fn add_f64(x=xv: F64) -> F64 {
    -x
}
NotBool => fn not_bool(x=xv: Bool) -> Bool {
    !x
}
}

// Each function definition inside the macro here produces:
//   1. An actual function, but of type `fn(RuntimeValue, RuntimeValue) -> FunctionResult`.
//      It does runtime type checking first to unwrap the `RuntimeValue`s to the
//      expected types like `i64` or `f64`. Then it does the computation, then
//      wraps again into a `RuntimeValue`.
//   2. One arm of a `match` statement that provides a lookup table from the
//      name of the intrinsic (`OP2`) to the structure `Intrinsic2`,
//      which provides the parameter and return types, as well as a pointer to
//      the function generated in (1).
def_int! {
[OP2, Intrinsic2, TypeAssertionFailedBinary]
/* i64 */
AddI64 => fn add_i64(a=av: I64, b=bv: I64) -> I64 {
    a + b
}
SubI64 => fn sub_i64(a=av: I64, b=bv: I64) -> I64 {
    a - b
}
MulI64 => fn mul_i64(a=av: I64, b=bv: I64) -> I64 {
    a * b
}
FloorDivI64 => fn floordiv_i64(a=av: I64, b=bv: I64) -> I64 {
    if b < 0 { (-a).div_euclid(-b) }
    else { a.div_euclid(b) }
}
LtI64 => fn lt_i64(a=av: I64, b=bv: I64) -> Bool {
    a < b
}
LtEqI64 => fn lteq_i64(a=av: I64, b=bv: I64) -> Bool {
    a <= b
}
GtI64 => fn gt_i64(a=av: I64, b=bv: I64) -> Bool {
    a > b
}
GtEqI64 => fn gteq_i64(a=av: I64, b=bv: I64) -> Bool {
    a >= b
}
NeqI64 => fn neq_i64(a=av: I64, b=bv: I64) -> Bool {
    a != b
}
EqI64 => fn eq_i64(a=av: I64, b=bv: I64) -> Bool {
    a == b
}

/* f64 */
AddF64 => fn add_f64(a=av: F64, b=bv: F64) -> F64 {
    a + b
}
SubF64 => fn sub_f64(a=av: F64, b=bv: F64) -> F64 {
    a - b
}
MulF64 => fn mul_f64(a=av: F64, b=bv: F64) -> F64 {
    a * b
}
TrueDivF64 => fn truediv_f64(a=av: F64, b=bv: F64) -> F64 {
    a / b
}
FloorDivF64 => fn floordiv_f64(a=av: F64, b=bv: F64) -> F64 {
    (a / b).floor()
}
LtF64 => fn lt_f64(a=av: F64, b=bv: F64) -> Bool {
    a < b
}
LtEqF64 => fn lteq_f64(a=av: F64, b=bv: F64) -> Bool {
    a <= b
}
GtF64 => fn gt_f64(a=av: F64, b=bv: F64) -> Bool {
    a > b
}
GtEqF64 => fn gteq_f64(a=av: F64, b=bv: F64) -> Bool {
    a >= b
}
NeqF64 => fn neq_f64(a=av: F64, b=bv: F64) -> Bool {
    a != b
}
EqF64 => fn eq_f64(a=av: F64, b=bv: F64) -> Bool {
    a == b
}

/* bool */
LtBool => fn lt_bool(a=av: Bool, b=bv: Bool) -> Bool {
    !a & b
}
LtEqBool => fn lteq_bool(a=av: Bool, b=bv: Bool) -> Bool {
    a <= b
}
GtBool => fn gt_bool(a=av: Bool, b=bv: Bool) -> Bool {
    a & !b
}
GtEqBool => fn gteq_bool(a=av: Bool, b=bv: Bool) -> Bool {
    a >= b
}
NeqBool => fn neq_bool(a=av: Bool, b=bv: Bool) -> Bool {
    a != b
}
EqBool => fn eq_bool(a=av: Bool, b=bv: Bool) -> Bool {
    a == b
}
}
