use std::collections::HashMap;
use std::iter::zip;

use crate::ast::*;
use crate::build_mir_err::{ArgCountWrong, BuildMIRErr as ME};
use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::{OP1, OP2};
use crate::mir::{self, BasicBlock, IP};
use crate::types::*;

pub fn ast_to_mir(program: &Program) -> Result<mir::Program, Diag> {
    Ctx::build_mir(program)
}

pub struct FunctionParam {
    pub name: Ident,
    pub param_type: Type,
}

struct FnSignature {
    param_types: Vec<FunctionParam>,
    return_type: Type,
}

struct Ctx {
    mir_program: mir::Program,
    fn_table: HashMap<String, FnSignature>,
}

/* Symbol-table stuff */
impl Ctx {
    fn lookup_type(&self, ty: &Ident) -> Result<Type, Diag> {
        Ok(match ty.name.as_str() {
            "i64" => Type::I64,
            "f64" => Type::F64,
            _ => return Err(ME::UnrecognizedTypeName(ty.span, ty.name.clone()).into_diag()),
        })
    }

    fn cook_fn_signature(&self, fn_def: &FunctionDefinition) -> Result<FnSignature, Diag> {
        let mut param_types = vec![];
        for param in &fn_def.params {
            param_types.push(FunctionParam {
                name: param.name.clone(),
                param_type: self.lookup_type(&param.param_type)?,
            });
        }
        return Ok(FnSignature {
            param_types,
            return_type: self.lookup_type(&fn_def.return_type)?,
        });
    }
}

/* Statement-level */
impl Ctx {
    pub fn build_mir(program: &Program) -> Result<mir::Program, Diag> {
        let mut frontend = Ctx {
            mir_program: mir::Program::new(),
            fn_table: HashMap::new(),
        };
        frontend.build(program)?;
        Ok(frontend.mir_program)
    }

    fn build(&mut self, program: &Program) -> Result<(), Diag> {
        for stmt in &program.body {
            let FnDefinition(fn_def) = &stmt.body else {
                return Err(ME::TopLevelExpr(stmt.span).into_diag());
            };
            self.fn_table.insert(
                fn_def.fn_name.name.clone(),
                self.cook_fn_signature(&fn_def)?,
            );
        }
        for stmt in &program.body {
            let FnDefinition(fn_def) = &stmt.body else {
                return Err(ME::TopLevelExpr(stmt.span).into_diag());
            };
            let block = BasicBlock::build_from_fn(&self, &fn_def)?;
            let name = fn_def.fn_name.name.clone();
            if let Some(..) = self.mir_program.fns.get(&name) {
                Err(ME::DuplicateFnName(fn_def.fn_name.span, name.clone()).into_diag())?;
            }
            self.mir_program.fns.insert(name, block);
        }
        Ok(())
    }
}

/* Expr-level */
impl BasicBlock {
    fn build_from_fn(ctx: &Ctx, fn_def: &FunctionDefinition) -> Result<mir::BasicBlock, Diag> {
        let sig = ctx.cook_fn_signature(fn_def)?;
        let mut block = mir::BasicBlock::new(sig.param_types)?;

        if fn_def.body.len() == 0 {
            Err(ME::MissingRet(fn_def.fn_name.span).into_diag())?;
        }

        for (i, stmt) in (&fn_def.body).into_iter().enumerate() {
            let (is_ret, inner_expr) = match &stmt.body {
                Ret(_, expr) => (true, expr.as_ref()),
                _ => (false, stmt),
            };
            let ip = block.add_expr(ctx, inner_expr)?;
            match (i == fn_def.body.len() - 1, is_ret) {
                (true, true) => {
                    block.set_return(ip);
                    let return_type = block.get_type(ip);
                    if return_type != sig.return_type {
                        Err(ME::WrongReturnType(
                            inner_expr.span,
                            fn_def.fn_name.name.to_string(),
                            return_type,
                            sig.return_type,
                        )
                        .into_diag())?
                    }
                }
                (true, false) => Err(ME::MissingRet(stmt.span).into_diag())?,
                (false, true) => Err(ME::MisplacedRet(stmt.span).into_diag())?,
                (false, false) => {}
            };
        }

        Ok(block)
    }

    fn add_expr(&mut self, ctx: &Ctx, ex: &Expr) -> Result<IP, Diag> {
        Ok(match &ex.body {
            Paren(arg) => self.add_expr(ctx, arg)?,
            Literal(lit) => self.push(mir::Literal(*lit), ex.span),
            Ident(x) => {
                let Some(ip) = self.get_symbol(x) else {
                    Err(ME::IdentifierNotFound(ex.span, x.to_string()).into_diag())?
                };
                ip
            }
            Unary(op, arg_node) => {
                let arg = self.add_expr(ctx, &arg_node)?;
                self.add_unary(*op, arg)?
            }
            Binary(op, left_node, right_node) => {
                let left = self.add_expr(ctx, &left_node)?;
                let right = self.add_expr(ctx, &right_node)?;
                self.add_binary(*op, left, right)?
            }
            FnDefinition(fn_def) => return Err(ME::FnInExpr(fn_def.fn_name.span).into_diag()),
            Ret(span, _) => return Err(ME::MisplacedRet(*span).into_diag()),
            FnCall(fun, arg_nodes) => {
                let Ident(fn_name) = &fun.body else {
                    Err(ME::CallNotIdent(ex.span).into_diag())?
                };
                let Some(sig) = ctx.fn_table.get(fn_name) else {
                    Err(ME::FunctionNotFound(ex.span, fn_name.to_string()).into_diag())?
                };
                if arg_nodes.len() != sig.param_types.len() {
                    Err(ME::from_arg_count(
                        ex.span,
                        ArgCountWrong {
                            fn_name: fn_name.to_string(),
                            count_expected: sig.param_types.len(),
                            count_actual: arg_nodes.len(),
                        },
                    )
                    .into_diag())?
                }
                let mut args = vec![];
                for (arg_node, param) in zip(arg_nodes, &sig.param_types) {
                    let ip = self.add_expr(ctx, arg_node)?;
                    let arg_type = self.get_type(ip);
                    if arg_type != param.param_type {
                        Err(ME::WrongArgType(
                            ex.span,
                            fn_name.to_string(),
                            arg_type,
                            param.param_type,
                        )
                        .into_diag())?;
                    }
                    args.push(ip);
                }
                self.push(
                    mir::FnCall(fn_name.to_string(), args, sig.return_type),
                    ex.span,
                )
            }
        })
    }

    fn add_unary(&mut self, op: UnaryOp, arg: IP) -> Result<IP, Diag> {
        let arg_type = self.get_type(arg);
        let op1 = match (op.node, arg_type) {
            (Neg, I64) => OP1::NegI64,
            (Neg, F64) => OP1::NegF64,
            // Since the only types are i64 and f64, we can't represent an
            // incorrectly-typed unary operation :P
            #[allow(unreachable_patterns)]
            (_, _) => Err(ME::InvalidTypeUnary(op, arg_type).into_diag())?,
        };
        return Ok(self.push(mir::Unary(op1, arg), op.span));
    }

    fn add_binary(&mut self, op: BinOp, left: IP, right: IP) -> Result<IP, Diag> {
        let left_type = self.get_type(left);
        let right_type = self.get_type(right);
        let op2 = match (op.node, left_type, right_type) {
            (Add, I64, I64) => OP2::AddI64,
            (Add, F64, F64) => OP2::AddF64,
            (Sub, I64, I64) => OP2::SubI64,
            (Sub, F64, F64) => OP2::SubF64,
            (Mul, I64, I64) => OP2::MulI64,
            (Mul, F64, F64) => OP2::MulF64,
            (Div, I64, I64) => OP2::DivI64,
            (Div, F64, F64) => OP2::DivF64,
            (_, _, _) => Err(ME::InvalidTypeBinary(op, left_type, right_type).into_diag())?,
        };
        return Ok(self.push(mir::Binary(op2, left, right), op.span));
    }
}

#[cfg(test)]
mod build_mir_expr_block {
    use super::*;
    use crate::parser::parse;
    use expect_test::{expect, Expect};

    fn mir_from_string(input: &str) -> Result<BasicBlock, Diag> {
        let ast_program = parse(input)?;
        let mut mir_program = ast_to_mir(&ast_program)?;
        let f_block = mir_program
            .fns
            .remove("f")
            .ok_or_else(|| panic!("No function f defined."))?;
        Ok(f_block)
    }

    fn check_build_mir(input: &str, expect: Expect) {
        let program = if !input.contains("{") {
            let ty = if input.contains(".") { "f64" } else { "i64" };
            format!("fn f() -> {ty} {{ ret {input}; }}")
        } else {
            input.to_owned()
        };
        let actual = match mir_from_string(&program) {
            Ok(mir) => format!("{:#?}", mir),
            Err(err) => format!("{:#?}", err),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_build_mir(
            "fn f() -> i64 { ret 12*3+8/4; }",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(12))
                   1: i64 Literal(Integer(3))
                   2: i64 Binary(MulI64, IP(0), IP(1))
                   3: i64 Literal(Integer(8))
                   4: i64 Literal(Integer(4))
                   5: i64 Binary(DivI64, IP(3), IP(4))
                   6: i64 Binary(AddI64, IP(2), IP(5))"#]],
        );
    }

    #[test]
    fn param() {
        check_build_mir(
            "fn f(x: i64) -> i64 { ret x + 1; } ",
            expect![[r#"
                BasicBlock
                   0: i64 LoadArg(I64)
                   1: i64 Literal(Integer(1))
                   2: i64 Binary(AddI64, IP(0), IP(1))"#]],
        );
    }

    #[test]
    fn incorrect_return_type() {
        check_build_mir(
            "fn f() -> i64 { ret 1.0 + 2.0; }",
            expect![
                "At 21-29: Expected function 'f' to return type 'f64', but it returned type 'i64'"
            ],
        );
    }

    #[test]
    fn binary_overloading() {
        check_build_mir(
            "1.0 + 2.0",
            expect![[r#"
                BasicBlock
                   0: f64 Literal(Float(1.0))
                   1: f64 Literal(Float(2.0))
                   2: f64 Binary(AddF64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1 + 2",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(1))
                   1: i64 Literal(Integer(2))
                   2: i64 Binary(AddI64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1.0 - 2.0",
            expect![[r#"
                BasicBlock
                   0: f64 Literal(Float(1.0))
                   1: f64 Literal(Float(2.0))
                   2: f64 Binary(SubF64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1 - 2",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(1))
                   1: i64 Literal(Integer(2))
                   2: i64 Binary(SubI64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1.0 * 2.0",
            expect![[r#"
                BasicBlock
                   0: f64 Literal(Float(1.0))
                   1: f64 Literal(Float(2.0))
                   2: f64 Binary(MulF64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1 * 2",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(1))
                   1: i64 Literal(Integer(2))
                   2: i64 Binary(MulI64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1.0 / 2.0",
            expect![[r#"
                BasicBlock
                   0: f64 Literal(Float(1.0))
                   1: f64 Literal(Float(2.0))
                   2: f64 Binary(DivF64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1 / 2",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(1))
                   1: i64 Literal(Integer(2))
                   2: i64 Binary(DivI64, IP(0), IP(1))"#]],
        );
    }

    #[test]
    fn type_error_binary() {
        check_build_mir(
            "1.0 + 2",
            expect!["At 25: Type error: Cannot perform f64 + i64"],
        );
        check_build_mir(
            "1 + 2.0",
            expect!["At 23: Type error: Cannot perform i64 + f64"],
        );
        check_build_mir(
            "1.0 - 2",
            expect!["At 25: Type error: Cannot perform f64 - i64"],
        );
        check_build_mir(
            "1 - 2.0",
            expect!["At 23: Type error: Cannot perform i64 - f64"],
        );
        check_build_mir(
            "1.0 * 2",
            expect!["At 25: Type error: Cannot perform f64 * i64"],
        );
        check_build_mir(
            "1 * 2.0",
            expect!["At 23: Type error: Cannot perform i64 * f64"],
        );
        check_build_mir(
            "1.0 / 2",
            expect!["At 25: Type error: Cannot perform f64 / i64"],
        );
        check_build_mir(
            "1 / 2.0",
            expect!["At 23: Type error: Cannot perform i64 / f64"],
        );
    }
}

#[cfg(test)]
mod build_mir_functions {
    use super::*;
    use crate::parser::parse;
    use expect_test::{expect, Expect};

    fn mir_from_string(input: &str) -> Result<mir::Program, Diag> {
        let ast_program = parse(input)?;
        Ok(ast_to_mir(&ast_program)?)
    }

    fn check_build_mir(input: &str, expect: Expect) {
        let actual = match mir_from_string(input) {
            Ok(mir) => format!("{:#?}", mir),
            Err(err) => format!("{:#?}", err),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_build_mir(
            "fn f() -> i64 { ret 12*3+8/4; }
            fn main() -> i64 { ret f(); }",
            expect![[r#"
                fn f BasicBlock
                   0: i64 Literal(Integer(12))
                   1: i64 Literal(Integer(3))
                   2: i64 Binary(MulI64, IP(0), IP(1))
                   3: i64 Literal(Integer(8))
                   4: i64 Literal(Integer(4))
                   5: i64 Binary(DivI64, IP(3), IP(4))
                   6: i64 Binary(AddI64, IP(2), IP(5))
                fn main BasicBlock
                   0: i64 FnCall("f", [], I64)"#]],
        );
    }

    #[test]
    fn params() {
        check_build_mir(
            "fn f(x: i64) -> i64 { ret x*3; }",
            expect![[r#"
                fn f BasicBlock
                   0: i64 LoadArg(I64)
                   1: i64 Literal(Integer(3))
                   2: i64 Binary(MulI64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "fn f(y: i64, x: i64) -> i64 { ret (x/y)+(y/x); }",
            expect![[r#"
                fn f BasicBlock
                   0: i64 LoadArg(I64)
                   1: i64 LoadArg(I64)
                   2: i64 Binary(DivI64, IP(1), IP(0))
                   3: i64 Binary(DivI64, IP(0), IP(1))
                   4: i64 Binary(AddI64, IP(2), IP(3))"#]],
        );
        check_build_mir(
            "fn f(f: i64) -> i64 { ret f; };",
            expect![[r#"
                fn f BasicBlock
                   0: i64 LoadArg(I64)"#]],
        );
        check_build_mir(
            "fn add(x: f64, y: f64) -> f64 { ret x+y; }\
            fn main() -> f64 { ret add(3.0, 5.0); }",
            expect![[r#"
                fn add BasicBlock
                   0: f64 LoadArg(F64)
                   1: f64 LoadArg(F64)
                   2: f64 Binary(AddF64, IP(0), IP(1))
                fn main BasicBlock
                   0: f64 Literal(Float(3.0))
                   1: f64 Literal(Float(5.0))
                   2: f64 FnCall("add", [IP(0), IP(1)], F64)"#]],
        )
    }

    #[test]
    fn param_errors() {
        check_build_mir(
            "fn f(x: i64, x: i64) -> i64 { ret f(1,1); };",
            expect!["At (!1,1!): Duplicate parameter 'x' already defined."],
        );
        check_build_mir(
            "fn f(x: i64) -> i64 { ret y*3; }",
            expect!["At 27: Identifier 'y' not found in the local scope of the function."],
        );
    }

    #[test]
    fn fn_not_found() {
        check_build_mir(
            "fn f() -> i64 { ret g(); }",
            expect!["At 21-23: Function 'g' not found. Try defining it with `fn g(x: u64) -> u64 { ret x * x; }`"],
        );
        check_build_mir(
            "fn f() -> i64 { ret 1; }\
            fn f() -> i64 { ret 2; }",
            expect!["At 28: Duplicate function 'f' already defined."],
        );
    }

    #[test]
    fn invalid_constructions() {
        check_build_mir(
            "fn f() -> i64 { ret (f)(); }",
            expect!["At 21-25: Function name must be an identifier."],
        );
        check_build_mir(
            "fn f() -> i64 { ret (f+1)(); }",
            expect!["At 21-27: Function name must be an identifier."],
        );
        check_build_mir(
            "1+2;",
            expect!["At 1-3: Top-level exprs are not yet supported. Try a main function instead: `fn main() -> i64 { ret 1 + 2; }`"],
        );
        check_build_mir(
            "fn f() -> i64 { 1+2; }",
            expect!["At 17-19: Unit type '()' has not yet been implemented, so all functions must have a return. Try adding 'ret'."],
        );
        check_build_mir(
            "fn f() -> i64 { ret 3; 1+2; }",
            expect!["At 17-21: Misplaced 'ret'. The keyword 'ret' can only be applied to the final statement in a block."],
        );
        check_build_mir(
            "fn f() -> i64 { }",
            expect!["At 4: Unit type '()' has not yet been implemented, so all functions must have a return. Try adding 'ret'."],
        );
        check_build_mir(
            "fn f() -> i64 {\
                ret 1 + (fn g() -> i64 {});\
            };",
            expect!["At 28: Function expressions are not yet supported. Try moving this to be a global function."],
        );
    }

    #[test]
    fn type_errors() {
        check_build_mir(
            "fn f(x: i64, y: i64) -> i64 { ret f(x); };",
            expect!["At 35-38: Too few args for function 'f': expected 2 but got only 1."],
        );
        check_build_mir(
            "fn f(x: i64, y: i64) -> i64 { ret f(x,x,x); };",
            expect!["At 35-42: Too many args for function 'f': expected 2 but got 3."],
        );
        check_build_mir(
            "fn f(x: i64, y: i64) -> i64 { ret f(x,1.0); };",
            expect!["At 35-42: Function 'f' expected an argument of type 'i64' here, but you passed in 'f64'"],
        );
        check_build_mir(
            "fn f(x: i64) -> f64 { ret x; };",
            expect![
                "At 27: Expected function 'f' to return type 'f64', but it returned type 'i64'"
            ],
        );
        check_build_mir(
            "fn f(x: i64, y: i64) -> I64 { ret 1; };",
            expect!["At 25-27: Type 'I64' is not recognized. Try 'i64' or 'f64' instead."],
        );
        check_build_mir(
            "fn f() -> i64 { ret f() + 1.0; };",
            expect!["At 25: Type error: Cannot perform i64 + f64"],
        );
    }
}
