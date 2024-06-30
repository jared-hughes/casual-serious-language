use std::collections::HashMap;
use std::iter::zip;

use crate::ast::*;
use crate::build_mir_err::{self as ME, ArgCountWrong};
use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::{OP1, OP2};
use crate::mir::{self, BasicBlock, IP};
use crate::symbol_table::SymbolTable;
use crate::types::*;

pub fn ast_to_mir(program: &Program) -> Result<mir::Program, Diag> {
    TopCtx::build_mir(program)
}

pub struct FunctionParam {
    pub name: Ident,
    pub param_type: Type,
}

struct FnSignature {
    param_types: Vec<FunctionParam>,
    return_type: Type,
}

fn err<T>(x: impl Diagnostic) -> Result<T, Diag> {
    Err(x.into_diag())
}

/// Top-level context
struct TopCtx {
    mir_program: mir::Program,
    fn_table: HashMap<String, FnSignature>,
}

/* Symbol-table stuff */
impl TopCtx {
    fn lookup_type(&self, ty: &Ident) -> Result<Type, Diag> {
        Ok(match ty.name.as_str() {
            "i64" => Type::I64,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            _ => {
                return err(ME::UnrecognizedTypeName {
                    span: ty.span,
                    name: ty.name.clone(),
                })
            }
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
impl TopCtx {
    pub fn build_mir(program: &Program) -> Result<mir::Program, Diag> {
        let mut top_level_ctx = Self {
            mir_program: mir::Program::new(),
            fn_table: HashMap::new(),
        };
        top_level_ctx.build(program)?;
        Ok(top_level_ctx.mir_program)
    }

    fn build(&mut self, program: &Program) -> Result<(), Diag> {
        for stmt in &program.body {
            let FnDefinition(fn_def) = &stmt.body else {
                return err(ME::TopLevelExpr { span: stmt.span });
            };
            self.fn_table.insert(
                fn_def.fn_name.name.clone(),
                self.cook_fn_signature(&fn_def)?,
            );
        }
        for stmt in &program.body {
            let FnDefinition(fn_def) = &stmt.body else {
                return err(ME::TopLevelExpr { span: stmt.span });
            };
            let name = fn_def.fn_name.name.clone();
            let ctx = Ctx::from_top(self);
            let block = Ctx::build_bb_from_fn(&ctx, &fn_def)?;
            if let Some(..) = self.mir_program.fns.get(&name) {
                err(ME::DuplicateFnName {
                    span: fn_def.fn_name.span,
                    name: name.clone(),
                })?;
            }
            self.mir_program.fns.insert(name, block);
        }
        Ok(())
    }
}

/// Expression-level context
struct Ctx<'ctx> {
    top_ctx: &'ctx TopCtx,
    pub symbol_table: SymbolTable<'ctx>,
}

impl<'ctx> Ctx<'ctx> {
    fn from_top(top_ctx: &'ctx TopCtx) -> Ctx<'ctx> {
        Ctx {
            top_ctx,
            symbol_table: SymbolTable::new(),
        }
    }

    fn child(&'ctx self) -> Ctx<'ctx> {
        Ctx {
            top_ctx: self.top_ctx,
            symbol_table: self.symbol_table.child(),
        }
    }
}

/* Expr-level */
impl<'ctx> Ctx<'ctx> {
    fn build_bb_from_fn(
        pctx: &'ctx Ctx<'ctx>,
        fn_def: &FunctionDefinition,
    ) -> Result<mir::BasicBlock, Diag> {
        let mut ctx = pctx.child();
        let sig = ctx.top_ctx.cook_fn_signature(fn_def)?;
        let param_types = sig.param_types.iter().map(|x| x.param_type).collect();
        let mut block = mir::BasicBlock::new(param_types);

        for p in sig.param_types {
            let name = p.name.name.to_string();
            let ip = block.push(mir::LoadArg(p.param_type), p.name.span);
            if let Some(..) = ctx.symbol_table.get_symbol(&name) {
                Err(ME::DuplicateParameter {
                    span: p.name.span,
                    name: name.to_owned(),
                }
                .into_diag())?;
            }
            ctx.symbol_table.set_symbol(name, ip);
        }

        if fn_def.body.len() == 0 {
            err(ME::MissingRet {
                span: fn_def.fn_name.span,
            })?;
        }

        for (i, stmt) in (&fn_def.body).into_iter().enumerate() {
            if i == fn_def.body.len() - 1 {
                if let Ret(..) = stmt.body {
                } else {
                    return err(ME::MissingRet { span: stmt.span });
                }
            }
            match &stmt.body {
                Ret(_, expr) => {
                    if i != fn_def.body.len() - 1 {
                        return err(ME::MisplacedRet { span: stmt.span });
                    }
                    let ip = ctx.add_expr(&mut block, &expr)?;
                    block.set_return(ip);
                    let return_type = block.get_type(ip);
                    if return_type != sig.return_type {
                        err(ME::WrongReturnType {
                            span: expr.span,
                            fn_name: fn_def.fn_name.name.to_string(),
                            actual: return_type,
                            expected: sig.return_type,
                        })?
                    }
                }
                Let(_, ident, expr) => {
                    if let Some(..) = ctx.symbol_table.get_symbol(&ident.name) {
                        return Err(ME::DuplicateDefinition {
                            span: ident.span,
                            name: ident.name.to_string(),
                        }
                        .into_diag());
                    }
                    let ip = ctx.add_expr(&mut block, &expr)?;
                    ctx.symbol_table.set_symbol(ident.name.to_string(), ip)
                }
                _ => {
                    // Dead code
                }
            }
        }

        Ok(block)
    }

    fn add_expr(&self, block: &mut BasicBlock, ex: &Expr) -> Result<IP, Diag> {
        Ok(match &ex.body {
            Paren(arg) => self.add_expr(block, arg)?,
            Literal(lit) => block.push(mir::Literal(*lit), ex.span),
            IdentExpr(x) => {
                let Some(ip) = self.symbol_table.get_symbol(x) else {
                    err(ME::IdentifierNotFound {
                        span: ex.span,
                        name: x.to_string(),
                    })?
                };
                ip
            }
            Unary(op, arg_node) => {
                let arg = self.add_expr(block, &arg_node)?;
                block.add_unary(*op, arg)?
            }
            Binary(op, left_node, right_node) => {
                let left = self.add_expr(block, &left_node)?;
                let right = self.add_expr(block, &right_node)?;
                block.add_binary(*op, left, right)?
            }
            FnDefinition(fn_def) => err(ME::FnInExpr {
                span: fn_def.fn_name.span,
            })?,
            Ret(span, _) => return err(ME::MisplacedRet { span: *span }),
            Let(span, ..) => return err(ME::MisplacedLet { span: *span }),
            FnCall(fun, arg_nodes) => {
                let IdentExpr(fn_name) = &fun.body else {
                    err(ME::CallNotIdent { span: ex.span })?
                };
                let Some(sig) = self.top_ctx.fn_table.get(fn_name) else {
                    err(ME::FunctionNotFound {
                        span: ex.span,
                        name: fn_name.to_string(),
                    })?
                };
                if arg_nodes.len() != sig.param_types.len() {
                    Err(ME::from_arg_count(
                        ex.span,
                        ArgCountWrong {
                            fn_name: fn_name.to_string(),
                            count_expected: sig.param_types.len(),
                            count_actual: arg_nodes.len(),
                        },
                    ))?
                }
                let mut args = vec![];
                for (arg_node, param) in zip(arg_nodes, &sig.param_types) {
                    let ip = self.add_expr(block, arg_node)?;
                    let arg_type = block.get_type(ip);
                    if arg_type != param.param_type {
                        err(ME::WrongArgType {
                            span: ex.span,
                            fn_name: fn_name.to_string(),
                            actual: arg_type,
                            expected: param.param_type,
                        })?
                    }
                    args.push(ip);
                }
                block.push(
                    mir::FnCall(fn_name.to_string(), args, sig.return_type),
                    ex.span,
                )
            }
        })
    }
}

impl BasicBlock {
    fn add_unary(&mut self, op: UnaryOp, arg: IP) -> Result<IP, Diag> {
        let arg_type = self.get_type(arg);
        let op1 = match (op.node, arg_type) {
            (Neg, I64) => OP1::NegI64,
            (Neg, F64) => OP1::NegF64,
            (Not, Bool) => OP1::NotBool,
            (_, _) => err(ME::InvalidTypeUnary {
                span: op.span,
                op,
                arg_type,
            })?,
        };
        return Ok(self.push(mir::Unary(op1, arg), op.span));
    }

    fn add_binary(&mut self, op: BinOp, left: IP, right: IP) -> Result<IP, Diag> {
        let left_type = self.get_type(left);
        let right_type = self.get_type(right);
        let op2 = match (op.node, left_type, right_type) {
            /* i64 */
            (Add, I64, I64) => OP2::AddI64,
            (Sub, I64, I64) => OP2::SubI64,
            (Mul, I64, I64) => OP2::MulI64,
            (Div, I64, I64) => OP2::DivI64,
            (Compare(Lt), I64, I64) => OP2::LtI64,
            (Compare(LtEq), I64, I64) => OP2::LtEqI64,
            (Compare(Gt), I64, I64) => OP2::GtI64,
            (Compare(GtEq), I64, I64) => OP2::GtEqI64,
            (Compare(Neq), I64, I64) => OP2::NeqI64,
            (Compare(Eq), I64, I64) => OP2::EqI64,
            /* f64 */
            (Add, F64, F64) => OP2::AddF64,
            (Sub, F64, F64) => OP2::SubF64,
            (Mul, F64, F64) => OP2::MulF64,
            (Div, F64, F64) => OP2::DivF64,
            (Compare(Lt), F64, F64) => OP2::LtF64,
            (Compare(LtEq), F64, F64) => OP2::LtEqF64,
            (Compare(Gt), F64, F64) => OP2::GtF64,
            (Compare(GtEq), F64, F64) => OP2::GtEqF64,
            (Compare(Neq), F64, F64) => OP2::NeqF64,
            (Compare(Eq), F64, F64) => OP2::EqF64,
            /* bool */
            (Compare(Lt), Bool, Bool) => OP2::LtBool,
            (Compare(LtEq), Bool, Bool) => OP2::LtEqBool,
            (Compare(Gt), Bool, Bool) => OP2::GtBool,
            (Compare(GtEq), Bool, Bool) => OP2::GtEqBool,
            (Compare(Neq), Bool, Bool) => OP2::NeqBool,
            (Compare(Eq), Bool, Bool) => OP2::EqBool,
            /* bad */
            (_, _, _) => err(ME::InvalidTypeBinary {
                span: op.span,
                op,
                left_type,
                right_type,
            })?,
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
            let ty = match () {
                () if input.contains("<") => "bool",
                () if input.contains(">") => "bool",
                () if input.contains("=") => "bool",
                () if input.contains(".") => "f64",
                () => "i64",
            };
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
                "At 21-29: Expected function 'f' to return type 'i64', but it returned type 'f64'"
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
        check_build_mir(
            "1 < 2",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(1))
                   1: i64 Literal(Integer(2))
                   2: bool Binary(LtI64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "1.0 < 2.0",
            expect![[r#"
                BasicBlock
                   0: f64 Literal(Float(1.0))
                   1: f64 Literal(Float(2.0))
                   2: bool Binary(LtF64, IP(0), IP(1))"#]],
        );
        check_build_mir(
            "(1>0) <= (2.0>1.0)",
            expect![[r#"
                BasicBlock
                   0: i64 Literal(Integer(1))
                   1: i64 Literal(Integer(0))
                   2: bool Binary(GtI64, IP(0), IP(1))
                   3: f64 Literal(Float(2.0))
                   4: f64 Literal(Float(1.0))
                   5: bool Binary(GtF64, IP(3), IP(4))
                   6: bool Binary(LtEqBool, IP(2), IP(5))"#]],
        );
    }

    #[test]
    fn type_error_unary() {
        check_build_mir("!1", expect!["At 21: Type error: Cannot perform !i64"]);
        check_build_mir("!1.0", expect!["At 21: Type error: Cannot perform !f64"]);
        check_build_mir("-(0<1)", expect!["At 22: Type error: Cannot perform -bool"]);
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
        check_build_mir(
            "1 < 2.0",
            expect!["At 24: Type error: Cannot perform i64 < f64"],
        );
        check_build_mir(
            "(1 < 2) < 3",
            expect!["At 30: Type error: Cannot perform bool < i64"],
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
            expect!["At 14: Duplicate parameter 'x' already defined."],
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

    #[test]
    fn fn_let() {
        check_build_mir(
            "fn sumSq(x: f64, y: f64) -> f64 {\
                let xx = x * x;\
                let yy = y * y;\
                ret xx + yy;\
            }",
            expect![[r#"
                fn sumSq BasicBlock
                   0: f64 LoadArg(F64)
                   1: f64 LoadArg(F64)
                   2: f64 Binary(MulF64, IP(0), IP(0))
                   3: f64 Binary(MulF64, IP(1), IP(1))
                   4: f64 Binary(AddF64, IP(2), IP(3))"#]],
        );
        check_build_mir(
            "fn pow16(x: f64) -> f64 {\
                let x2 = x * x;\
                let x4 = x2 * x2;\
                ret x4 * x4;\
            }",
            expect![[r#"
                fn pow16 BasicBlock
                   0: f64 LoadArg(F64)
                   1: f64 Binary(MulF64, IP(0), IP(0))
                   2: f64 Binary(MulF64, IP(1), IP(1))
                   3: f64 Binary(MulF64, IP(2), IP(2))"#]],
        );
    }

    #[test]
    fn fn_let_collisions() {
        check_build_mir(
            "fn f(x: f64, y: f64) -> f64 {\
                let x = y;
                ret x;
            }",
            expect!["At 34: Cannot redefine 'x' since it is already defined."],
        );
        check_build_mir(
            "fn f(x: f64) -> f64 {\
                let z = x;
                let z = x;
                ret z;
            }",
            expect!["At 53: Cannot redefine 'z' since it is already defined."],
        );
    }
}
