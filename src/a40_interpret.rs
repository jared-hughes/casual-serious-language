use crate::build_mir::ast_to_mir;
use crate::errors::Diag;
use crate::mir::*;
use crate::parser::parse;

pub fn compile_and_interpret(input: &str) -> Result<u32, Diag> {
    let program = ast_to_mir(*parse(input)?)?;
    Ok(interpret_mir(program))
}

pub fn interpret_mir(program: BasicBlock) -> u32 {
    Interpreter::interpret(program)
}

struct Interpreter {
    // TODO: Make this an IndexVec too, and remove .as_usize() below.
    mem: Vec<u32>,
}

impl Interpreter {
    pub fn interpret(program: BasicBlock) -> u32 {
        let mut interpreter = Interpreter {
            mem: vec![0; program.len()],
        };
        interpreter.run(program)
    }

    fn run(&mut self, program: BasicBlock) -> u32 {
        // TODO: Only go up to return index?
        // OR: should BasicBlock.set_return trim the vec?
        // Doesn't matter since return index is always the last index.
        for (i, inst) in program.iter().enumerate() {
            self.mem[i] = self.eval(*inst);
        }
        self.mem[program.get_return().as_usize()]
    }

    fn eval(&self, inst: Inst) -> u32 {
        match inst {
            Literal(Integer(x)) => x,
            Binary(op, a_ip, b_ip) => {
                let a = self.mem[a_ip.as_usize()];
                let b = self.mem[b_ip.as_usize()];
                match op {
                    BinOpKind::Add => a + b,
                    BinOpKind::Sub => a - b,
                    BinOpKind::Mul => a * b,
                    BinOpKind::Div => a / b,
                }
            }
        }
    }
}

#[cfg(test)]
mod interpret_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_interpret_mir(input: &str, expect: Expect) {
        let actual = match compile_and_interpret(input) {
            Ok(mir) => format!("{:#?}", mir),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_interpret_mir("12*3+8/4-2", expect!["36"]);
    }

    // TODO: negatives and saturation/wrapping
    #[test]
    fn division_is_floor() {
        check_interpret_mir("7/4", expect!["1"]);
    }
}
