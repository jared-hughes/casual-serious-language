use crate::runtime_err::*;
use crate::runtime_value::*;
use crate::types::Type;

type FunctionResult = Result<RuntimeValue, RuntimeErrorInner>;

pub struct Intrinsic2 {
    pub param_types: (Type, Type),
    pub return_type: Type,
    pub compute: fn(RuntimeValue, RuntimeValue) -> FunctionResult,
}

#[derive(Clone, Copy, Debug)]
pub enum OP2 {
    AddI64,
    SubI64,
    MulI64,
    DivI64,
    AddF64,
    SubF64,
    MulF64,
    DivF64,
}

macro_rules! def_int2 {
    (
        [$OP:ident]
        $(
            $op:ident =>
            fn $fn:ident($a:ident: $at:ident, $b:ident: $bt:ident) -> $ret:ident
                $body:block
        )+
    ) => {
        impl $OP {
            // Lookup into the functions
            pub fn get_intrinsic(self) -> Intrinsic2 {
                match self {
                    $(
                        $OP::$op => Intrinsic2 {
                            param_types: (Type::$at, Type::$bt),
                            return_type: Type::$ret,
                            compute: Self::$fn,
                        },
                    )+
                }
            }

            // Define the functions
            $(fn $fn(av: RuntimeValue, bv: RuntimeValue) -> FunctionResult {
                if let ($at($a), $bt($b)) = (av, bv) {
                    Ok($ret($body))
                } else {
                    Err(TypeAssertionFailedBinary{op: $OP::$op, args: (av, bv)}.up())
                }
            })+
        }
    };
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
def_int2! {
[OP2]
AddI64 => fn add_i64(a: I64, b: I64) -> I64 {
    a + b
}
SubI64 => fn sub_i64(a: I64, b: I64) -> I64 {
    a - b
}
MulI64 => fn mul_i64(a: I64, b: I64) -> I64 {
    a * b
}
DivI64 => fn div_i64(a: I64, b: I64) -> I64 {
    a / b
}
AddF64 => fn add_f64(a: F64, b: F64) -> F64 {
    a + b
}
SubF64 => fn sub_f64(a: F64, b: F64) -> F64 {
    a - b
}
MulF64 => fn mul_f64(a: F64, b: F64) -> F64 {
    a * b
}
DivF64 => fn div_f64(a: F64, b: F64) -> F64 {
    a / b
}
}
