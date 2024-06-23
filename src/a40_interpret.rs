use crate::build_mir::ast_to_mir;
use crate::errors::{Diag, Diagnostic};
use crate::mir::*;
use crate::parser::parse;
use crate::runtime_err::RuntimeError;
use crate::runtime_value::*;
use index_vec::{index_vec, IndexVec};

pub fn compile_and_interpret(input: &str) -> Result<RuntimeValue, Diag> {
    let program = ast_to_mir(*parse(input)?)?;
    Ok(interpret_mir(program)?)
}

pub fn interpret_mir(program: BasicBlock) -> RuntimeResult {
    Interpreter::interpret(program)
}

struct Interpreter {
    mem: IndexVec<IP, RuntimeValue>,
}

impl Interpreter {
    pub fn interpret(program: BasicBlock) -> RuntimeResult {
        let mut interpreter = Interpreter {
            mem: index_vec![I64(0); program.len()],
        };
        interpreter.run(program)
    }

    fn run(&mut self, program: BasicBlock) -> RuntimeResult {
        // TODO: Only go up to return index?
        // OR: should BasicBlock.set_return trim the vec?
        // Doesn't matter since return index is always the last index.
        for (i, inst) in program.iter().enumerate() {
            self.mem[i] = self.eval(*inst)?;
        }
        Ok(self.mem[program.get_return()])
    }

    fn eval(&self, inst: Inst) -> RuntimeResult {
        Ok(match inst.kind {
            Literal(Integer(x)) => I64(x),
            Literal(Float(x)) => F64(x),
            Binary(op, a_ip, b_ip) => {
                let a = self.mem[a_ip];
                let b = self.mem[b_ip];
                let info = op.get_intrinsic();
                (info.compute)(a, b).map_err(|e| {
                    RuntimeError {
                        span: inst.span,
                        inner: e,
                    }
                    .into_diag()
                })?
            }
        })
    }
}

#[cfg(test)]
mod interpret_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_interpret_mir(input: &str, expect: Expect) {
        let actual = match compile_and_interpret(input) {
            Ok(mir) => format!("{:?}", mir),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_interpret_mir("12*3+8/4-2", expect!["I64(36)"]);
    }

    #[test]
    fn binary_operators() {
        check_interpret_mir("1. + 2.", expect!["F64(3.0)"]);
        check_interpret_mir("1 + 2", expect!["I64(3)"]);
        check_interpret_mir("1. - 2.", expect!["F64(-1.0)"]);
        check_interpret_mir("1 - 2", expect!["I64(-1)"]);
        check_interpret_mir("1. * 2.", expect!["F64(2.0)"]);
        check_interpret_mir("1 * 2", expect!["I64(2)"]);
        check_interpret_mir("1. / 2.", expect!["F64(0.5)"]);
        check_interpret_mir("1 / 2", expect!["I64(0)"]);
    }

    // TODO: negatives and saturation/wrapping
    #[test]
    fn division_is_floor() {
        check_interpret_mir("7/4", expect!["I64(1)"]);
    }
}
