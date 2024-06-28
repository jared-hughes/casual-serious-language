use crate::build_mir::ast_to_mir;
use crate::errors::Diag;
use crate::mir::*;
use crate::parser::parse;
use crate::runtime_err::RuntimeErrorInner as RE;
use crate::runtime_value::*;
use crate::span::DUMMY_SPAN;
use index_vec::{index_vec, IndexVec};

pub fn compile_and_interpret(input: &str) -> Result<RuntimeValue, Diag> {
    let ast_program = parse(input)?;
    let mir_program = ast_to_mir(&ast_program)?;
    interpret_mir(&mir_program)
}

pub fn interpret_mir(program: &Program) -> RuntimeResult {
    Interpreter::interpret(program)
}

struct Interpreter<'prog> {
    program: &'prog Program,
}

type Memory = IndexVec<IP, RuntimeValue>;

impl<'prog> Interpreter<'prog> {
    pub fn interpret(program: &'prog Program) -> RuntimeResult {
        let interpreter = Interpreter { program };
        interpreter.run_fn(&"main", vec![])
    }

    fn run_fn(&self, fn_name: &str, args: Vec<RuntimeValue>) -> RuntimeResult {
        let block = self
            .program
            .fns
            .get(fn_name)
            .ok_or_else(|| RE::MissingFunction(fn_name.to_string()).span(DUMMY_SPAN))?;
        if block.params.len() != args.len() {
            Err(RE::IncorrectArgs(fn_name.to_string()).span(DUMMY_SPAN))?;
        }
        let mut mem = index_vec![I64(0); block.len()];
        for (i, inst) in block.iter().enumerate() {
            if i < args.len() {
                if args[i].to_type() != block.params[i] {
                    Err(RE::IncorrectArgs(fn_name.to_string()).span(DUMMY_SPAN))?;
                }
                mem[i] = args[i];
                continue;
            }
            mem[i] = self.eval(&mem, inst)?;
        }
        Ok(mem[block.get_return()])
    }

    fn eval(&self, mem: &Memory, inst: &Inst) -> RuntimeResult {
        Ok(match inst.kind {
            Literal(Integer(x)) => I64(x),
            Literal(Float(x)) => F64(x),
            Unary(op, a_ip) => {
                let a = mem[a_ip];
                let info = op.get_intrinsic();
                (info.compute)(a).map_err(|e| e.span(inst.span))?
            }
            Binary(op, a_ip, b_ip) => {
                let a = mem[a_ip];
                let b = mem[b_ip];
                let info = op.get_intrinsic();
                (info.compute)(a, b).map_err(|e| e.span(inst.span))?
            }
            FnCall(ref fun, ref arg_ips, _) => {
                let mut args: Vec<RuntimeValue> = vec![];
                for a in arg_ips {
                    args.push(mem[*a]);
                }
                self.run_fn(&fun, args)?
            }
            LoadArg(_) => panic!("LoadArg after parameter instructions"),
        })
    }
}

#[cfg(test)]
mod interpret_expr_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_interpret_mir(input: &str, expect: Expect) {
        let program = if !input.contains("{") {
            let ty = if input.contains(".") { "f64" } else { "i64" };
            format!("fn main() -> {ty} {{ ret {input}; }}")
        } else {
            input.to_owned()
        };
        let actual = match compile_and_interpret(&program) {
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
        check_interpret_mir("1.0 + 2.0", expect!["F64(3.0)"]);
        check_interpret_mir("1 + 2", expect!["I64(3)"]);
        check_interpret_mir("1.0 - 2.0", expect!["F64(-1.0)"]);
        check_interpret_mir("1 - 2", expect!["I64(-1)"]);
        check_interpret_mir("1.0 * 2.0", expect!["F64(2.0)"]);
        check_interpret_mir("1 * 2", expect!["I64(2)"]);
        check_interpret_mir("1.0 / 2.0", expect!["F64(0.5)"]);
        check_interpret_mir("1 / 2", expect!["I64(0)"]);
    }

    // TODO: negatives and saturation/wrapping
    #[test]
    fn division_is_floor() {
        check_interpret_mir("7/4", expect!["I64(1)"]);
    }
}

#[cfg(test)]
mod interpret_stmt_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_interpret_mir(input: &str, expect: Expect) {
        let actual = match compile_and_interpret(&input) {
            Ok(mir) => format!("{:?}", mir),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_interpret_mir("fn main() -> i64 { ret 1+2; }", expect!["I64(3)"]);
    }

    #[test]
    fn call_fn() {
        check_interpret_mir(
            "fn add(x: i64, y: i64) -> i64 { ret x+y; }\
            fn main() -> i64 { ret add(3,5); }",
            expect!["I64(8)"],
        );
        check_interpret_mir(
            "fn add(x: f64, y: f64) -> f64 { ret x+y; }\
            fn main() -> f64 { ret add(3.0, 5.0); }",
            expect!["F64(8.0)"],
        );
        check_interpret_mir(
            "fn f1(x: i64) -> i64 { ret x; }
            fn f2(x: i64, y: i64) -> i64 { ret f1(x+y); }\
            fn f3(x: i64, y: i64, z: i64) -> i64 { ret f2(x,y+z); }\
            fn g2(x: i64, y: i64) -> i64 { ret f3(x,y,y+1); }\
            fn g1(x: i64) -> i64 { ret g2(x,x+1); }\
            fn main() -> i64 { ret g1(1); }",
            expect!["I64(6)"],
        );
    }

    #[test]
    fn incorrect_main() {
        // TODO: We have no distinction between compiling and linking.
        check_interpret_mir(
            "fn main(x: i64) -> i64 { ret 1+2; }",
            expect!["At (!1,1!): Incorrect args to function 'main"],
        );
    }
}
