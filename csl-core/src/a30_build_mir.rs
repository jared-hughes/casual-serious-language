use std::collections::HashMap;
use std::iter::zip;

use crate::ast::*;
use crate::build_mir_err::{self as ME, ArgCountWrong};
use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::{OP1, OP2};
use crate::mir::{self, FnBody, BP, IP};
use crate::span::Span;
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
            let block = Ctx::build_body_from_fn(&ctx, &fn_def)?;
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
    fn build_body_from_fn(
        pctx: &'ctx Ctx<'ctx>,
        fn_def: &FunctionDefinition,
    ) -> Result<mir::FnBody, Diag> {
        let mut ctx = pctx.child();
        let sig = ctx.top_ctx.cook_fn_signature(fn_def)?;
        let param_types = sig.param_types.iter().map(|x| x.param_type).collect();
        let mut body = mir::FnBody::new(param_types);
        let block = body.new_basic_block();

        for (i, p) in sig.param_types.into_iter().enumerate() {
            let name = p.name.name.to_string();
            let ip = IP::from(i);
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
            err(ME::FnMissingRet {
                span: fn_def.fn_name.span,
            })?;
        }

        let ((block, ip), ret_span) = ctx.add_stmts_ret(&mut body, block, &fn_def.body)?;
        let return_type = body.get_type(ip);
        if return_type != sig.return_type {
            err(ME::WrongReturnType {
                span: ret_span,
                fn_name: fn_def.fn_name.name.to_string(),
                actual: return_type,
                expected: sig.return_type,
            })?
        }
        body.set_terminator(block, mir::Return(ip));

        Ok(body)
    }

    /// Precondition: stmts is nonempty.
    /// Caller should give an appropriate error message when `stmts.len() == 0`.
    /// Returns a tuple (ip, span of returning expr).
    fn add_stmts_ret(
        &mut self,
        body: &mut FnBody,
        block: BP,
        stmts: &Vec<Expr>,
    ) -> Result<((BP, IP), Span), Diag> {
        assert!(stmts.len() > 0);
        let mut block = block;
        for (i, stmt) in (&stmts).into_iter().enumerate() {
            if i == stmts.len() - 1 {
                if let Ret(_, expr) = &stmt.body {
                    let (block, ip) = self.add_expr(body, block, &expr)?;
                    return Ok(((block, ip), expr.span));
                } else {
                    return err(ME::FnMissingRet { span: stmt.span });
                }
            }
            match &stmt.body {
                // TODO: Move Let and Ret special cases out of here, and make it return "never";
                Ret(..) => return err(ME::MisplacedRet { span: stmt.span }),
                Let(_, ident, expr) => {
                    if let Some(..) = self.symbol_table.get_symbol(&ident.name) {
                        return Err(ME::DuplicateDefinition {
                            span: ident.span,
                            name: ident.name.to_string(),
                        }
                        .into_diag());
                    }
                    let (block1, ip) = self.add_expr(body, block, &expr)?;
                    block = block1;
                    self.symbol_table.set_symbol(ident.name.to_string(), ip);
                }
                _ => {
                    todo!("Add dead code to MIR to get type checking.")
                }
            }
        }
        unreachable!();
    }

    /// Returns (BP, IP).
    /// The BP tells you where control flow is at, after executing this expr.
    /// The IP tells you where is the return value of this expr.
    fn add_expr(&self, body: &mut FnBody, block: BP, ex: &Expr) -> Result<(BP, IP), Diag> {
        Ok(match &ex.body {
            Paren(arg) => return self.add_expr(body, block, arg),
            Literal(lit) => (
                block,
                body.push_assign_new_ip(block, mir::Literal(*lit), ex.span),
            ),
            IdentExpr(x) => {
                let Some(ip) = self.symbol_table.get_symbol(x) else {
                    err(ME::IdentifierNotFound {
                        span: ex.span,
                        name: x.to_string(),
                    })?
                };
                (block, ip)
            }
            Unary(op, arg_node) => {
                let (block, arg) = self.add_expr(body, block, &arg_node)?;
                (block, body.add_unary(block, *op, arg)?)
            }
            Binary(op, left_node, right_node) => {
                let (block, left) = self.add_expr(body, block, &left_node)?;
                let (block, right) = self.add_expr(body, block, &right_node)?;
                (block, body.add_binary(block, *op, left, right)?)
            }
            FnDefinition(fn_def) => err(ME::FnInExpr {
                span: fn_def.fn_name.span,
            })?,
            Ret(span, _) => return err(ME::MisplacedRet { span: *span }),
            Let(span, ..) => return err(ME::MisplacedLet { span: *span }),
            Block(stmts) => {
                let mut child_ctx = self.child();
                if stmts.len() == 0 {
                    err(ME::BlockMissingRet { span: ex.span })?;
                }
                let ((block, ip), _sp) = child_ctx.add_stmts_ret(body, block, stmts)?;
                (block, ip)
            }
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
                let mut block = block;
                for (arg_node, param) in zip(arg_nodes, &sig.param_types) {
                    let (block1, ip) = self.add_expr(body, block, arg_node)?;
                    block = block1;
                    let arg_type = body.get_type(ip);
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
                let ip = body.push_assign_new_ip(
                    block,
                    mir::FnCall(fn_name.to_string(), args, sig.return_type),
                    ex.span,
                );
                (block, ip)
            }
            If(cond_node, if_node, else_node) => {
                let (block, cond_ip) = self.add_expr(body, block, &cond_node)?;
                {
                    let cond_type = body.get_type(cond_ip);
                    if cond_type != Bool {
                        err(ME::WrongIfConditionType {
                            span: cond_node.span,
                            cond_type,
                        })?;
                    }
                }
                // Setup diamond shape
                let if_block = body.new_basic_block();
                let else_block = body.new_basic_block();
                body.set_terminator(
                    block,
                    mir::If {
                        cond: cond_ip,
                        true_branch: if_block,
                        false_branch: else_block,
                    },
                );
                // Add branches
                let (if_block, if_ip) = self.add_expr(body, if_block, &if_node)?;
                let Some(else_node) = else_node else {
                    todo!();
                };
                let (else_block, else_ip) = self.add_expr(body, else_block, &else_node)?;
                let out_block = body.new_basic_block();
                let value_type = {
                    let if_type = body.get_type(if_ip);
                    let else_type = body.get_type(else_ip);
                    if if_type != else_type {
                        err(ME::WrongElseType {
                            span: else_node.span,
                            if_type,
                            else_type,
                        })?;
                    }
                    if_type
                };
                // Reconstruct value back to output branch
                let out_val = body.push_local(value_type);
                body.push(
                    if_block,
                    mir::Assign(out_val, mir::Use(if_ip, value_type), if_node.span),
                );
                body.push(
                    else_block,
                    mir::Assign(out_val, mir::Use(else_ip, value_type), if_node.span),
                );
                body.set_terminator(if_block, mir::Goto { target: out_block });
                body.set_terminator(else_block, mir::Goto { target: out_block });
                (out_block, out_val)
            }
        })
    }
}

impl FnBody {
    fn add_unary(&mut self, block: BP, op: UnaryOp, arg: IP) -> Result<IP, Diag> {
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
        return Ok(self.push_assign_new_ip(block, mir::Unary(op1, arg), op.span));
    }

    fn add_binary(&mut self, block: BP, op: BinOp, left: IP, right: IP) -> Result<IP, Diag> {
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
        return Ok(self.push_assign_new_ip(block, mir::Binary(op2, left, right), op.span));
    }
}

#[cfg(test)]
mod build_mir_expr_block {
    use super::*;
    use crate::parser::parse;
    use expect_test::{expect, Expect};

    fn mir_from_string(input: &str) -> Result<FnBody, Diag> {
        let ast_program = parse(input)?;
        let mut mir_program = ast_to_mir(&ast_program)?;
        let f_block = mir_program
            .fns
            .remove("f")
            .ok_or_else(|| panic!("No function f defined."))?;
        Ok(f_block)
    }

    fn check_build_mir(input: &str, expect: Expect) {
        let ty = match () {
            () if input.contains("<") => "bool",
            () if input.contains(">") => "bool",
            () if input.contains("==") => "bool",
            () if input.contains("!=") => "bool",
            () if input.contains(".") => "f64",
            () => "i64",
        };
        let program = format!("fn f() -> {ty} {{ ret {input}; }}");
        let actual = match mir_from_string(&program) {
            Ok(mir) => format!("{:#?}", mir),
            Err(err) => format!("{:#?}", err),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn binary_overloading() {
        check_build_mir(
            "1.0 + 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Float(1.0))
                  IP(1) = Literal(Float(2.0))
                  IP(2) = Binary(AddF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 + 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Binary(AddI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 - 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Float(1.0))
                  IP(1) = Literal(Float(2.0))
                  IP(2) = Binary(SubF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 - 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Binary(SubI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 * 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Float(1.0))
                  IP(1) = Literal(Float(2.0))
                  IP(2) = Binary(MulF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 * 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 / 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Float(1.0))
                  IP(1) = Literal(Float(2.0))
                  IP(2) = Binary(DivF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 / 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Binary(DivI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 < 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Binary(LtI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 < 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Float(1.0))
                  IP(1) = Literal(Float(2.0))
                  IP(2) = Binary(LtF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "(1>0) <= (2.0>1.0)",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(0))
                  IP(2) = Binary(GtI64, IP(0), IP(1))
                  IP(3) = Literal(Float(2.0))
                  IP(4) = Literal(Float(1.0))
                  IP(5) = Binary(GtF64, IP(3), IP(4))
                  IP(6) = Binary(LtEqBool, IP(2), IP(5))
                  Return(IP(6))
            "#]],
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

    #[test]
    fn block() {
        check_build_mir(
            "1 + { ret 2+3; } + 4",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Literal(Integer(3))
                  IP(3) = Binary(AddI64, IP(1), IP(2))
                  IP(4) = Binary(AddI64, IP(0), IP(3))
                  IP(5) = Literal(Integer(4))
                  IP(6) = Binary(AddI64, IP(4), IP(5))
                  Return(IP(6))
            "#]],
        );

        check_build_mir(
            "{ let x=1; ret x*2; } + {let x=3; ret x*4;}",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(1))
                  IP(1) = Literal(Integer(2))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  IP(3) = Literal(Integer(3))
                  IP(4) = Literal(Integer(4))
                  IP(5) = Binary(MulI64, IP(3), IP(4))
                  IP(6) = Binary(AddI64, IP(2), IP(5))
                  Return(IP(6))
            "#]],
        );
    }

    #[test]
    fn empty_block() {
        check_build_mir(
            "1 + { }",
            expect!["At 25-27: Unit type '()' has not yet been implemented, so all blocks must have a return. Try adding 'ret'."],
        );
    }

    #[test]
    fn shadowing_errors() {
        check_build_mir(
            "{ let x=1; ret {let x=3; ret x*4;};}",
            expect!["At 41: Cannot redefine 'x' since it is already defined."],
        );
    }
}

#[cfg(test)]
mod build_mir_expr_hard {
    use super::*;
    use crate::parser::parse;
    use expect_test::{expect, Expect};

    fn mir_from_string(input: &str) -> Result<FnBody, Diag> {
        let ast_program = parse(input)?;
        let mut mir_program = ast_to_mir(&ast_program)?;
        let f_block = mir_program
            .fns
            .remove("f")
            .ok_or_else(|| panic!("No function f defined."))?;
        Ok(f_block)
    }

    fn check_build_mir(ty: &str, input: &str, expect: Expect) {
        let program = format!("fn f() -> {ty} {{ ret {input}; }}");
        let actual = match mir_from_string(&program) {
            Ok(mir) => format!("{:#?}", mir),
            Err(err) => format!("{:#?}", err),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn if_else() {
        check_build_mir(
            "i64",
            "if(2>1) 3 else 4",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(2))
                  IP(1) = Literal(Integer(1))
                  IP(2) = Binary(GtI64, IP(0), IP(1))
                  If { cond: IP(2), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(3) = Literal(Integer(3))
                  IP(5) = Use(IP(3), I64)
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(4) = Literal(Integer(4))
                  IP(5) = Use(IP(4), I64)
                  Goto { target: BP(3) }

                BP(3): BasicBlock
                  Return(IP(5))
            "#]],
        );
    }

    #[test]
    fn if_else_errors() {
        check_build_mir(
            "i64",
            "if(2>1) 3 else 4.0",
            expect!["At 36-38: Expected else branch to return type 'i64' (the same type as the 'if' branch), but the else branch returned type 'f64'."],
        );
        check_build_mir(
            "i64",
            "if(2) 3 else 4.0",
            expect!["At 24: Expected 'if' to branch on a boolean ('Bool') but got type 'i64'."],
        );
        // TODO-unit-type: Handle no else branch
        // check_build_mir(
        //     "i64",
        //     "if(2>1) 3",
        //     expect![[r#"
        //         TODO
        //     "#]],
        // );
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
    fn param() {
        check_build_mir(
            "fn f(x: i64) -> i64 { ret x + 1; } ",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(1) = Literal(Integer(1))
                  IP(2) = Binary(AddI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
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
    fn smoke_test() {
        check_build_mir(
            "fn f() -> i64 { ret 12*3+8/4; }
            fn main() -> i64 { ret f(); }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(0) = Literal(Integer(12))
                  IP(1) = Literal(Integer(3))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  IP(3) = Literal(Integer(8))
                  IP(4) = Literal(Integer(4))
                  IP(5) = Binary(DivI64, IP(3), IP(4))
                  IP(6) = Binary(AddI64, IP(2), IP(5))
                  Return(IP(6))

                fn main:
                BP(0): BasicBlock
                  IP(0) = FnCall("f", [], I64)
                  Return(IP(0))
            "#]],
        );
    }

    #[test]
    fn params() {
        check_build_mir(
            "fn f(x: i64) -> i64 { ret x*3; }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(1) = Literal(Integer(3))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "fn f(y: i64, x: i64) -> i64 { ret (x/y)+(y/x); }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(2) = Binary(DivI64, IP(1), IP(0))
                  IP(3) = Binary(DivI64, IP(0), IP(1))
                  IP(4) = Binary(AddI64, IP(2), IP(3))
                  Return(IP(4))
            "#]],
        );
        check_build_mir(
            "fn f(f: i64) -> i64 { ret f; };",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  Return(IP(0))
            "#]],
        );
        check_build_mir(
            "fn add(x: f64, y: f64) -> f64 { ret x+y; }\
            fn main() -> f64 { ret add(3.0, 5.0); }",
            expect![[r#"
                fn add:
                BP(0): BasicBlock
                  IP(2) = Binary(AddF64, IP(0), IP(1))
                  Return(IP(2))

                fn main:
                BP(0): BasicBlock
                  IP(0) = Literal(Float(3.0))
                  IP(1) = Literal(Float(5.0))
                  IP(2) = FnCall("add", [IP(0), IP(1)], F64)
                  Return(IP(2))
            "#]],
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
                fn sumSq:
                BP(0): BasicBlock
                  IP(2) = Binary(MulF64, IP(0), IP(0))
                  IP(3) = Binary(MulF64, IP(1), IP(1))
                  IP(4) = Binary(AddF64, IP(2), IP(3))
                  Return(IP(4))
            "#]],
        );
        check_build_mir(
            "fn pow16(x: f64) -> f64 {\
                let x2 = x * x;\
                let x4 = x2 * x2;\
                ret x4 * x4;\
            }",
            expect![[r#"
                fn pow16:
                BP(0): BasicBlock
                  IP(1) = Binary(MulF64, IP(0), IP(0))
                  IP(2) = Binary(MulF64, IP(1), IP(1))
                  IP(3) = Binary(MulF64, IP(2), IP(2))
                  Return(IP(3))
            "#]],
        );
    }

    #[test]
    fn fibonacci_recursive() {
        check_build_mir(
            "fn f(x: i64) -> i64 {
                ret if (x == 0) 0
                    else if (x == 1) 1
                    else f(x-1) + f(x-2);
            }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(1) = Literal(Integer(0))
                  IP(2) = Binary(EqI64, IP(0), IP(1))
                  If { cond: IP(2), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(3) = Literal(Integer(0))
                  IP(15) = Use(IP(3), I64)
                  Goto { target: BP(6) }

                BP(2): BasicBlock
                  IP(4) = Literal(Integer(1))
                  IP(5) = Binary(EqI64, IP(0), IP(4))
                  If { cond: IP(5), true_branch: BP(3), false_branch: BP(4) }

                BP(3): BasicBlock
                  IP(6) = Literal(Integer(1))
                  IP(14) = Use(IP(6), I64)
                  Goto { target: BP(5) }

                BP(4): BasicBlock
                  IP(7) = Literal(Integer(1))
                  IP(8) = Binary(SubI64, IP(0), IP(7))
                  IP(9) = FnCall("f", [IP(8)], I64)
                  IP(10) = Literal(Integer(2))
                  IP(11) = Binary(SubI64, IP(0), IP(10))
                  IP(12) = FnCall("f", [IP(11)], I64)
                  IP(13) = Binary(AddI64, IP(9), IP(12))
                  IP(14) = Use(IP(13), I64)
                  Goto { target: BP(5) }

                BP(5): BasicBlock
                  IP(15) = Use(IP(14), I64)
                  Goto { target: BP(6) }

                BP(6): BasicBlock
                  Return(IP(15))
            "#]],
        );
    }

    #[test]
    fn fn_let_collisions() {
        check_build_mir(
            "fn f(x: f64, y: f64) -> f64 {\
                let x = y;\
                ret x;\
            }",
            expect!["At 34: Cannot redefine 'x' since it is already defined."],
        );
        check_build_mir(
            "fn f(x: f64) -> f64 {\
                let z = x;\
                let z = x;\
                ret z;\
            }",
            expect!["At 36: Cannot redefine 'z' since it is already defined."],
        );
        check_build_mir(
            "fn f(x: f64) -> f64 {\
                let z = {let x = 5; ret x+1;};\
                ret z;
            }",
            expect!["At 35: Cannot redefine 'x' since it is already defined."],
        );
    }
}
