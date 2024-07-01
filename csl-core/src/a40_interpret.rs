use crate::build_mir::ast_to_mir;
use crate::errors::Diag;
use crate::mir::*;
use crate::parser::parse;
use crate::runtime_err::RuntimeErrorInner as RE;
use crate::runtime_value::*;
use crate::span::Span;
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
        interpreter.run_fn("main", vec![])
    }

    fn run_fn(&self, fn_name: &str, args: Vec<RuntimeValue>) -> RuntimeResult {
        let body = self
            .program
            .fns
            .get(fn_name)
            .ok_or_else(|| RE::MissingFunction(fn_name.to_string()).span(DUMMY_SPAN))?;
        if body.params.len() != args.len() {
            Err(RE::IncorrectArgs(fn_name.to_string()).span(DUMMY_SPAN))?;
        }
        let mut mem: IndexVec<IP, RuntimeValue> = index_vec![I64(0); body.num_locals()];
        for (i, param_type) in body.params.iter().enumerate() {
            if args[i].to_type() != *param_type {
                Err(RE::IncorrectArgs(fn_name.to_string()).span(DUMMY_SPAN))?;
            }
            mem[i] = args[i];
        }
        let mut block = BP::from(0);
        loop {
            for inst in body.iter_stmts(block) {
                match inst {
                    Assign(ip, rval, sp) => {
                        mem[*ip] = self.eval(&mem, rval, sp)?;
                    }
                }
            }
            let term = body.get_terminator(block);
            match *term {
                Unterminated => panic!("Unterminated block {block:?}."),
                Return(ip) => return Ok(mem[ip]),
                Goto { target } => block = target,
                If {
                    cond,
                    true_branch,
                    false_branch,
                } => {
                    block = match mem[cond] {
                        Bool(true) => true_branch,
                        Bool(false) => false_branch,
                        _ => panic!("Invalid type in 'if' branch: '{}'.", mem[cond].to_type()),
                    }
                }
            }
        }
    }

    fn eval(&self, mem: &Memory, rval: &RValue, sp: &Span) -> RuntimeResult {
        Ok(match *rval {
            Literal(x) => x,
            Unary(op, a_ip) => {
                let a = mem[a_ip];
                let info = op.get_intrinsic();
                (info.compute)(a).map_err(|e| e.span(*sp))?
            }
            Binary(op, a_ip, b_ip) => {
                let a = mem[a_ip];
                let b = mem[b_ip];
                let info = op.get_intrinsic();
                (info.compute)(a, b).map_err(|e| e.span(*sp))?
            }
            FnCall(ref fun, ref arg_ips) => {
                let mut args: Vec<RuntimeValue> = vec![];
                for a in arg_ips {
                    args.push(mem[*a]);
                }
                self.run_fn(fun, args)?
            }
            Use(ip) => mem[ip],
        })
    }
}

#[cfg(test)]
mod interpret_expr_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_interpret_mir(input: &str, expect: Expect) {
        let program = if !input.contains('{') {
            let ty = match () {
                () if input.contains('<') => "bool",
                () if input.contains('>') => "bool",
                () if input.contains('=') => "bool",
                () if input.contains('.') => "f64",
                () => "i64",
            };
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
    fn unary() {
        check_interpret_mir("-(1 + 2)", expect!["I64(-3)"]);
        check_interpret_mir("-(1.0 + 2.0)", expect!["F64(-3.0)"]);
        check_interpret_mir("!(1 < 0)", expect!["Bool(true)"]);
    }

    #[test]
    fn binary_num_num() {
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

    #[test]
    fn binary_num_bool() {
        check_interpret_mir("1.0 < 2.0", expect!["Bool(true)"]);
        check_interpret_mir("1.0 <= 2.0", expect!["Bool(true)"]);
        check_interpret_mir("1.0 > 2.0", expect!["Bool(false)"]);
        check_interpret_mir("1.0 >= 2.0", expect!["Bool(false)"]);
        check_interpret_mir("1.0 == 2.0", expect!["Bool(false)"]);
        check_interpret_mir("1.0 != 2.0", expect!["Bool(true)"]);
        check_interpret_mir("1 < 2", expect!["Bool(true)"]);
        check_interpret_mir("1 <= 2", expect!["Bool(true)"]);
        check_interpret_mir("1 > 2", expect!["Bool(false)"]);
        check_interpret_mir("1 >= 2", expect!["Bool(false)"]);
        check_interpret_mir("1 == 2", expect!["Bool(false)"]);
        check_interpret_mir("1 != 2", expect!["Bool(true)"]);
    }

    #[test]
    fn binary_bool_bool() {
        check_interpret_mir("(0 > 0) < (1 > 0)", expect!["Bool(true)"]);
        check_interpret_mir("(0 > 0) <= (1 > 0)", expect!["Bool(true)"]);
        check_interpret_mir("(0 > 0) > (1 > 0)", expect!["Bool(false)"]);
        check_interpret_mir("(0 > 0) >= (1 > 0)", expect!["Bool(false)"]);
        check_interpret_mir("(0 > 0) == (1 > 0)", expect!["Bool(false)"]);
        check_interpret_mir("(0 > 0) != (1 > 0)", expect!["Bool(true)"]);
    }

    #[test]
    fn if_then() {
        check_interpret_mir("if (1 > 0) 2 else 3", expect!["At 25-43: Expected function 'main' to return type 'bool', but it returned type 'i64'"]);
        check_interpret_mir("if (0 > 0) 2 else 3", expect!["At 25-43: Expected function 'main' to return type 'bool', but it returned type 'i64'"]);
    }

    #[test]
    fn logical_connectives() {
        check_interpret_mir("(1>0) || (1>0)", expect!["Bool(true)"]);
        check_interpret_mir("(1>0) || (0>0)", expect!["Bool(true)"]);
        check_interpret_mir("(0>0) || (1>0)", expect!["Bool(true)"]);
        check_interpret_mir("(0>0) || (0>0)", expect!["Bool(false)"]);
        check_interpret_mir("(1>0) && (1>0)", expect!["Bool(true)"]);
        check_interpret_mir("(1>0) && (0>0)", expect!["Bool(false)"]);
        check_interpret_mir("(0>0) && (1>0)", expect!["Bool(false)"]);
        check_interpret_mir("(0>0) && (0>0)", expect!["Bool(false)"]);
    }
}

#[cfg(test)]
mod interpret_expr_tests_hard {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_interpret_mir(ty: &str, input: &str, expect: Expect) {
        let program = format!("fn main() -> {ty} {{ ret {input}; }}");
        let actual = match compile_and_interpret(&program) {
            Ok(mir) => format!("{:?}", mir),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn if_then() {
        check_interpret_mir("i64", "if (1 > 0) 2 else 3", expect!["I64(2)"]);
        check_interpret_mir("i64", "if (0 > 0) 2 else 3", expect!["I64(3)"]);
    }
}

#[cfg(test)]
mod interpret_stmt_tests {
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

    #[test]
    fn fibonacci_recursive() {
        // Exponentially-recursive (since there is no cache)
        check_interpret_mir(
            "fn f(n: i64) -> i64 {
                ret if (n == 0) 0
                    else if (n == 1) 1
                    else f(n-1) + f(n-2);
            }
            fn main() -> i64 { ret f(12); }",
            expect!["I64(144)"],
        );
        // Tail-recursive form
        check_interpret_mir(
            "fn f(x: i64, y: i64, n: i64) -> i64 {
                ret if (n == 0) y
                    else f(y+x, x, n-1);
            }
            fn fib(n: i64) -> i64 {
                ret f(1,0,n);
            }
            fn main() -> i64 { ret fib(12); }",
            expect!["I64(144)"],
        );
    }

    #[test]
    fn short_circuit() {
        // If these didn't short circuit, they would stack overflow.
        check_interpret_mir(
            "fn inf_loop(n: i64) -> bool {
                ret inf_loop(n);
            }
            fn main() -> bool {
                ret (1 > 0) || inf_loop(0);
            }",
            expect!["Bool(true)"],
        );
        check_interpret_mir(
            "fn inf_loop(n: i64) -> bool {
                ret inf_loop(n);
            }
            fn main() -> bool {
                ret (0 > 0) && inf_loop(0);
            }",
            expect!["Bool(false)"],
        );
    }
}
