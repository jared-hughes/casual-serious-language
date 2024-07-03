use std::collections::HashMap;
use std::iter::zip;

use crate::ast::*;
use crate::build_mir_err::{self as ME, ArgCountWrong};
use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::{OP1, OP2};
use crate::mir::{self, FnBody, BP, IP};
use crate::runtime_value::{cook_lit, RuntimeValue};
use crate::span::{Span, DUMMY_SPAN};
use crate::symbol_table::{Local, SymbolTable};
use crate::types::*;

pub(crate) fn ast_to_mir(program: &Program) -> Result<mir::Program, Diag> {
    TopCtx::build_mir(program)
}

pub(crate) struct FunctionParam {
    pub(crate) name: Ident,
    pub(crate) param_type: Type,
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
    fn lookup_type(&self, ty: &Expr) -> Result<Type, Diag> {
        match &ty.body {
            Literal(Lit::Unit) => Ok(Type::Unit),
            IdentExpr(ident) => self.lookup_type_ident(&Ident {
                // Silly clone here.
                name: ident.clone(),
                span: ty.span,
            }),
            Paren(inner) => self.lookup_type(inner),
            _ => err(ME::NotAType { span: ty.span }),
        }
    }

    fn lookup_type_ident(&self, ty: &Ident) -> Result<Type, Diag> {
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
        Ok(FnSignature {
            param_types,
            return_type: self.lookup_type(&fn_def.return_type)?,
        })
    }
}

/* Statement-level */
impl TopCtx {
    pub(crate) fn build_mir(program: &Program) -> Result<mir::Program, Diag> {
        let mut top_level_ctx = Self {
            mir_program: mir::Program::new(),
            fn_table: HashMap::new(),
        };
        top_level_ctx.build(program)?;
        Ok(top_level_ctx.mir_program)
    }

    fn build(&mut self, program: &Program) -> Result<(), Diag> {
        for stmt in &program.body {
            let FnDefinition(fn_def) = &stmt.expr.body else {
                return err(ME::TopLevelExpr { span: stmt.span });
            };
            if let Bare = stmt.kind {
            } else {
                return err(ME::TopLevelExpr { span: stmt.span });
            }
            self.fn_table
                .insert(fn_def.fn_name.name.clone(), self.cook_fn_signature(fn_def)?);
        }
        for stmt in &program.body {
            let FnDefinition(fn_def) = &stmt.expr.body else {
                return err(ME::TopLevelExpr { span: stmt.span });
            };
            let name = fn_def.fn_name.name.clone();
            let block = Ctx::build_body_from_fn(self, fn_def)?;
            if self.mir_program.fns.contains_key(&name) {
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
    pub(crate) body: &'ctx mut mir::FnBody,
    pub(crate) symbol_table: SymbolTable<'ctx>,
}

impl Ctx<'_> {
    fn child(&mut self) -> Ctx<'_> {
        Ctx {
            top_ctx: self.top_ctx,
            body: self.body,
            symbol_table: self.symbol_table.child(),
        }
    }
}

/* Expr-level */
impl Ctx<'_> {
    fn build_body_from_fn(
        top_ctx: &TopCtx,
        fn_def: &FunctionDefinition,
    ) -> Result<mir::FnBody, Diag> {
        let sig = top_ctx.cook_fn_signature(fn_def)?;
        let param_types = sig.param_types.iter().map(|x| x.param_type).collect();
        let mut body = mir::FnBody::new(param_types);
        let mut ctx = Ctx {
            top_ctx,
            body: &mut body,
            symbol_table: SymbolTable::new(),
        };
        ctx.add_fn_def(fn_def, sig)?;
        Ok(body)
    }

    fn add_fn_def(&mut self, fn_def: &FunctionDefinition, sig: FnSignature) -> Result<(), Diag> {
        let block = self.body.new_basic_block();

        for (i, p) in sig.param_types.iter().enumerate() {
            let name = p.name.name.to_string();
            let ip = IP::from(i);
            if self.symbol_table.get_symbol(&name).is_some() {
                Err(ME::DuplicateParameter {
                    span: p.name.span,
                    name: name.to_owned(),
                }
                .into_diag())?;
            }
            self.symbol_table.set_symbol(
                name,
                Local {
                    mutability: Mutability::No,
                    ip,
                },
            );
        }

        let ((block, ip), ret_span) = self.add_stmts_ret(block, &fn_def.body)?;
        let return_type = self.body.get_type(ip);
        if return_type != sig.return_type {
            err(ME::WrongReturnType {
                span: ret_span,
                fn_name: fn_def.fn_name.name.to_string(),
                actual: return_type,
                expected: sig.return_type,
            })?
        }
        self.body.set_terminator(block, mir::Return(ip));
        Ok(())
    }

    /// Returns a tuple (ip, span of returning expr).
    fn add_stmts_ret(&mut self, block: BP, stmts: &[Statement]) -> Result<((BP, IP), Span), Diag> {
        let mut block = block;
        for (i, stmt) in stmts.iter().enumerate() {
            let expr = &stmt.expr;
            match &stmt.kind {
                Ret => {
                    if i != stmts.len() - 1 {
                        return err(ME::MisplacedRet { span: stmt.span });
                    }
                    let (block, ip) = self.add_expr(block, expr)?;
                    return Ok(((block, ip), expr.span));
                }
                Let(LetLHS { ident, mutability }) => {
                    if self.symbol_table.has_symbol(&ident.name) {
                        return Err(ME::DuplicateDefinition {
                            span: ident.span,
                            name: ident.name.to_string(),
                        }
                        .into_diag());
                    }
                    let (block1, ip) = self.add_expr(block, expr)?;
                    block = block1;
                    self.symbol_table.set_symbol(
                        ident.name.to_string(),
                        Local {
                            ip,
                            mutability: *mutability,
                        },
                    );
                }
                _ => {
                    let (block1, _ip) = self.add_expr(block, expr)?;
                    block = block1;
                }
            }
        }
        let bip = self.body.push_unit_new_ip(block, DUMMY_SPAN);
        Ok((bip, DUMMY_SPAN))
    }

    /// Returns (BP, IP).
    /// The BP tells you where control flow is at, after executing this expr.
    /// The IP tells you where is the return value of this expr.
    fn add_expr(&mut self, block: BP, ex: &Expr) -> Result<(BP, IP), Diag> {
        Ok(match &ex.body {
            Paren(arg) => return self.add_expr(block, arg),
            Literal(lit) => {
                let cooked = cook_lit(*lit);
                self.body.push_constant(block, cooked, ex.span)
            }
            IdentExpr(x) => {
                let Some(sv) = self.symbol_table.get_symbol(x) else {
                    err(ME::IdentifierNotFound {
                        span: ex.span,
                        name: x.to_string(),
                    })?
                };
                let Local { ip, mutability: _ } = *sv;
                (block, ip)
            }
            Assign(ident, expr) => {
                let Some(sv) = self.symbol_table.get_symbol(&ident.name) else {
                    err(ME::IdentifierNotFound {
                        span: ident.span,
                        name: ident.name.to_string(),
                    })?
                };
                let Local { ip, mutability } = *sv;
                let Mutability::Yes = mutability else {
                    err(ME::LocalImmutable {
                        span: ident.span,
                        name: ident.name.to_string(),
                    })?
                };

                let (block, rhs_ip) = self.add_expr(block, expr)?;

                self.body
                    .push(block, mir::Assign(ip, mir::Use(rhs_ip), DUMMY_SPAN));
                self.body.push_unit_new_ip(block, DUMMY_SPAN)
            }
            Unary(op, arg_node) => {
                let (block, arg) = self.add_expr(block, arg_node)?;
                self.body.add_unary(block, *op, arg)?
            }
            Binary(op, left_node, right_node) => match op.node {
                Or | And => {
                    let (block, cond_ip) = self.add_expr(block, left_node)?;
                    {
                        let cond_type = self.body.get_type(cond_ip);
                        if cond_type != Bool {
                            err(ME::WrongAndOrConditionType {
                                span: left_node.span,
                                cond_type,
                                kind: op.node,
                            })?;
                        }
                    }
                    // Fork
                    let (if_block, else_block) = self.fork_if_else(block, cond_ip);
                    // Calculate on each fork
                    let (if_bi, else_bi) = if let And = op.node {
                        // a && b => a ? b : a
                        let if_bi = self.add_expr(if_block, right_node)?;
                        {
                            let right_type = self.body.get_type(if_bi.1);
                            if right_type != Bool {
                                err(ME::WrongAndOrSecondType {
                                    span: left_node.span,
                                    right_type,
                                    kind: op.node,
                                })?;
                            }
                        }
                        let else_bi = self.body.push_constant_bi(
                            else_block,
                            RuntimeValue::Bool(false),
                            DUMMY_SPAN,
                        );
                        (if_bi, else_bi)
                    } else {
                        // a || b => a ? a : b
                        let if_bi = self.body.push_constant_bi(
                            if_block,
                            RuntimeValue::Bool(true),
                            DUMMY_SPAN,
                        );
                        let else_bi = self.add_expr(else_block, right_node)?;
                        {
                            let right_type = self.body.get_type(else_bi.1);
                            if right_type != Bool {
                                err(ME::WrongAndOrSecondType {
                                    span: left_node.span,
                                    right_type,
                                    kind: op.node,
                                })?;
                            }
                        }
                        (if_bi, else_bi)
                    };
                    // Join
                    self.join_if_else_returns(Bool, if_bi, else_bi)
                }
                _ => {
                    let (block, left) = self.add_expr(block, left_node)?;
                    let (block, right) = self.add_expr(block, right_node)?;
                    self.body.add_binary(block, *op, left, right)?
                }
            },
            FnDefinition(fn_def) => err(ME::FnInExpr {
                span: fn_def.fn_name.span,
            })?,
            Block(stmts) => {
                let mut child_ctx = self.child();
                let (bip, _sp) = child_ctx.add_stmts_ret(block, stmts)?;
                bip
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
                    let (block1, ip) = self.add_expr(block, arg_node)?;
                    block = block1;
                    let arg_type = self.body.get_type(ip);
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
                self.body.push_assign_new_ip(
                    block,
                    mir::FnCall(fn_name.to_string(), args),
                    sig.return_type,
                    ex.span,
                )
            }
            If(cond_node, if_node, else_node) => {
                let (block, cond_ip) = self.add_expr(block, cond_node)?;
                {
                    let cond_type = self.body.get_type(cond_ip);
                    if cond_type != Bool {
                        err(ME::WrongIfConditionType {
                            span: cond_node.span,
                            cond_type,
                        })?;
                    }
                }
                // Setup diamond shape
                let (if_block, else_block) = self.fork_if_else(block, cond_ip);
                // Add branches
                let if_bi = self.add_expr(if_block, if_node)?;
                let else_bi = match else_node {
                    Some(else_node) => self.add_expr(else_block, else_node)?,
                    None => self.body.push_unit_new_ip(else_block, DUMMY_SPAN),
                };
                let value_type = {
                    let if_type = self.body.get_type(if_bi.1);
                    let else_type = self.body.get_type(else_bi.1);
                    if if_type != else_type {
                        match else_node {
                            Some(else_node) => err(ME::WrongElseType {
                                span: else_node.span,
                                if_type,
                                else_type,
                            })?,
                            None => err(ME::WrongTypeMissingElse {
                                span: if_node.span,
                                if_type,
                            })?,
                        }
                    }
                    if_type
                };
                // Reconstruct value back to output branch
                self.join_if_else_returns(value_type, if_bi, else_bi)
            }
        })
    }

    /// Precondition: caller checks `cond_ip` points to a Bool.
    fn fork_if_else(&mut self, block: BP, cond_ip: IP) -> (BP, BP) {
        assert!(self.body.get_type(cond_ip) == Bool);
        let if_block = self.body.new_basic_block();
        let else_block = self.body.new_basic_block();
        self.body.set_terminator(
            block,
            mir::If {
                cond: cond_ip,
                true_branch: if_block,
                false_branch: else_block,
            },
        );
        (if_block, else_block)
    }

    fn join_if_else(&mut self, if_block: BP, else_block: BP) -> BP {
        let out_block = self.body.new_basic_block();
        self.body
            .set_terminator(if_block, mir::Goto { target: out_block });
        self.body
            .set_terminator(else_block, mir::Goto { target: out_block });
        out_block
    }

    /// Precondition: `if_ip` and `else_ip` have the same type (`value_type`)
    fn join_if_else_returns(
        &mut self,
        value_type: Type,
        if_bi: (BP, IP),
        else_bi: (BP, IP),
    ) -> (BP, IP) {
        let (if_block, if_ip) = if_bi;
        let (else_block, else_ip) = else_bi;
        assert!(self.body.get_type(if_ip) == value_type);
        assert!(self.body.get_type(else_ip) == value_type);
        let out_val = self.body.push_local(value_type);
        self.body
            .push(if_block, mir::Assign(out_val, mir::Use(if_ip), DUMMY_SPAN));
        self.body.push(
            else_block,
            mir::Assign(out_val, mir::Use(else_ip), DUMMY_SPAN),
        );
        let out_block = self.join_if_else(if_block, else_block);
        (out_block, out_val)
    }
}

impl FnBody {
    fn add_unary(&mut self, block: BP, op: UnaryOp, arg: IP) -> Result<(BP, IP), Diag> {
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
        let ty = op1.get_intrinsic().return_type;
        Ok(self.push_assign_new_ip(block, mir::Unary(op1, arg), ty, op.span))
    }

    fn add_binary(&mut self, block: BP, op: BinOp, left: IP, right: IP) -> Result<(BP, IP), Diag> {
        let left_type = self.get_type(left);
        let right_type = self.get_type(right);
        let op2 = match (op.node, left_type, right_type) {
            /* i64 */
            (Add, I64, I64) => OP2::AddI64,
            (Sub, I64, I64) => OP2::SubI64,
            (Mul, I64, I64) => OP2::MulI64,
            (FloorDiv, I64, I64) => OP2::FloorDivI64,
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
            (TrueDiv, F64, F64) => OP2::TrueDivF64,
            (FloorDiv, F64, F64) => OP2::FloorDivF64,
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
        let ty = op2.get_intrinsic().return_type;
        Ok(self.push_assign_new_ip(block, mir::Binary(op2, left, right), ty, op.span))
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
            () if input.contains('<') => "bool",
            () if input.contains('>') => "bool",
            () if input.contains("==") => "bool",
            () if input.contains("!=") => "bool",
            () if input.contains('.') => "f64",
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
                  IP(0) = Literal(F64(1.0))
                  IP(1) = Literal(F64(2.0))
                  IP(2) = Binary(AddF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 + 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(AddI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 - 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(F64(1.0))
                  IP(1) = Literal(F64(2.0))
                  IP(2) = Binary(SubF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 - 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(SubI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 * 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(F64(1.0))
                  IP(1) = Literal(F64(2.0))
                  IP(2) = Binary(MulF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 * 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 / 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(F64(1.0))
                  IP(1) = Literal(F64(2.0))
                  IP(2) = Binary(TrueDivF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 // 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(F64(1.0))
                  IP(1) = Literal(F64(2.0))
                  IP(2) = Binary(FloorDivF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 // 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(FloorDivI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1 < 2",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(LtI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "1.0 < 2.0",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(F64(1.0))
                  IP(1) = Literal(F64(2.0))
                  IP(2) = Binary(LtF64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "(1>0) <= (2.0>1.0)",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(0))
                  IP(2) = Binary(GtI64, IP(0), IP(1))
                  IP(3) = Literal(F64(2.0))
                  IP(4) = Literal(F64(1.0))
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
            "1 / 2",
            expect!["At 23: Type error: Cannot perform i64 / i64"],
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
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Literal(I64(3))
                  IP(3) = Binary(AddI64, IP(1), IP(2))
                  IP(4) = Binary(AddI64, IP(0), IP(3))
                  IP(5) = Literal(I64(4))
                  IP(6) = Binary(AddI64, IP(4), IP(5))
                  Return(IP(6))
            "#]],
        );

        check_build_mir(
            "{ let x=1; ret x*2; } + {let x=3; ret x*4;}",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  IP(3) = Literal(I64(3))
                  IP(4) = Literal(I64(4))
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
            expect!["At 23: Type error: Cannot perform i64 + ()"],
        );
    }

    #[test]
    fn shadowing_errors() {
        check_build_mir(
            "{ let x=1; ret {let x=3; ret x*4;};}",
            expect!["At 41: Cannot redefine 'x' since it is already defined."],
        );
    }

    #[test]
    fn scoping() {
        check_build_mir(
            "{ let x = 1; ret x; } + x",
            expect!["At 45: Identifier 'x' not found in the local scope of the function."],
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
                  IP(0) = Literal(I64(2))
                  IP(1) = Literal(I64(1))
                  IP(2) = Binary(GtI64, IP(0), IP(1))
                  If { cond: IP(2), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(3) = Literal(I64(3))
                  IP(5) = Use(IP(3))
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(4) = Literal(I64(4))
                  IP(5) = Use(IP(4))
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
        check_build_mir(
            "i64",
            "if(2>1) 3",
            expect!["At 29: Expected else branch to return type 'i64' (the same type as this 'if' branch), but there is no else branch. Try removing 'ret' from the 'if' branch, or adding an 'else' branch."],
        );
    }

    #[test]
    fn and_or() {
        check_build_mir(
            "bool",
            "(1>2) || (3==4)",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(GtI64, IP(0), IP(1))
                  If { cond: IP(2), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(3) = Literal(Bool(true))
                  IP(7) = Use(IP(3))
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(4) = Literal(I64(3))
                  IP(5) = Literal(I64(4))
                  IP(6) = Binary(EqI64, IP(4), IP(5))
                  IP(7) = Use(IP(6))
                  Goto { target: BP(3) }

                BP(3): BasicBlock
                  Return(IP(7))
            "#]],
        );
        check_build_mir(
            "bool",
            "(1>2) && (3==4)",
            expect![[r#"
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(GtI64, IP(0), IP(1))
                  If { cond: IP(2), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(3) = Literal(I64(3))
                  IP(4) = Literal(I64(4))
                  IP(5) = Binary(EqI64, IP(3), IP(4))
                  IP(7) = Use(IP(5))
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(6) = Literal(Bool(false))
                  IP(7) = Use(IP(6))
                  Goto { target: BP(3) }

                BP(3): BasicBlock
                  Return(IP(7))
            "#]],
        );
    }

    #[test]
    fn and_or_errors() {
        check_build_mir(
            "bool",
            "1 || (1 > 0)",
            expect!["At 22: Expected first argument of '||' to be 'Bool' but got type 'i64'."],
        );
        check_build_mir(
            "i64",
            "(1>0) || 1",
            expect!["At 21-25: Expected second argument of '||' to be 'Bool' but got type 'i64'."],
        );
        check_build_mir(
            "bool",
            "1 && (1 > 0)",
            expect!["At 22: Expected first argument of '&&' to be 'Bool' but got type 'i64'."],
        );
        check_build_mir(
            "i64",
            "(1>0) && 1",
            expect!["At 21-25: Expected second argument of '&&' to be 'Bool' but got type 'i64'."],
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
        ast_to_mir(&ast_program)
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
                  IP(1) = Literal(I64(1))
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
            expect!["At 27: Type error: Cannot perform i64 / i64"],
        );
    }

    #[test]
    fn params() {
        check_build_mir(
            "fn f(x: i64) -> i64 { ret x*3; }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(1) = Literal(I64(3))
                  IP(2) = Binary(MulI64, IP(0), IP(1))
                  Return(IP(2))
            "#]],
        );
        check_build_mir(
            "fn f(y: i64, x: i64) -> i64 { ret (x/y)+(y/x); }",
            expect!["At 37: Type error: Cannot perform i64 / i64"],
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
                  IP(0) = Literal(F64(3.0))
                  IP(1) = Literal(F64(5.0))
                  IP(2) = FnCall("add", [IP(0), IP(1)])
                  Return(IP(2))
            "#]],
        );
    }

    #[test]
    fn paren_params() {
        check_build_mir(
            "fn f(x: (i64)) -> (i64) { ret x; };",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  Return(IP(0))
            "#]],
        );
        check_build_mir(
            "fn f(x: ()) -> () { ret x; };",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  Return(IP(0))
            "#]],
        );
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
            expect![
                "At (!1,1!): Expected function 'f' to return type 'i64', but it returned type '()'"
            ],
        );
        check_build_mir(
            "fn f() -> i64 { ret 3; 1+2; }",
            expect!["At 17-21: Misplaced 'ret'. The keyword 'ret' can only be applied to the final statement in a block."],
        );
        check_build_mir(
            "fn f() -> i64 { }",
            expect![
                "At (!1,1!): Expected function 'f' to return type 'i64', but it returned type '()'"
            ],
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
    }

    #[test]
    fn type_repr_errors() {
        check_build_mir(
            "fn f(x: i64, y: i64) -> I64 { ret 1; };",
            expect!["At 25-27: Type 'I64' is not recognized. Try 'i64' or 'f64' instead."],
        );
        check_build_mir(
            "fn f() -> (1,2) { ret f() + 1.0; };",
            expect!["At 13: Expected to see a ')' here."],
        );
        check_build_mir(
            "fn f() -> -i64 { ret f() + 1.0; };",
            expect!["At 11-14: This is not a valid representation of a type. Try something like 'i64' or '()' instead."],
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
    fn dead_code() {
        check_build_mir(
            "fn f() -> i64 { 1+2; ret 3+4; }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(0) = Literal(I64(1))
                  IP(1) = Literal(I64(2))
                  IP(2) = Binary(AddI64, IP(0), IP(1))
                  IP(3) = Literal(I64(3))
                  IP(4) = Literal(I64(4))
                  IP(5) = Binary(AddI64, IP(3), IP(4))
                  Return(IP(5))
            "#]],
        );
        check_build_mir(
            "fn f() -> i64 { 1+2.0; ret 3+4; }",
            expect!["At 18: Type error: Cannot perform i64 + f64"],
        );
        check_build_mir(
            "fn f() -> i64 { ret 3+4; 1+2; }",
            expect!["At 17-23: Misplaced 'ret'. The keyword 'ret' can only be applied to the final statement in a block."],
        );
        check_build_mir(
            "fn f(x: bool) -> i64 { if(x) (); ret 3+4; }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  If { cond: IP(0), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(1) = Literal(UnitValue)
                  IP(3) = Use(IP(1))
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(2) = Literal(UnitValue)
                  IP(3) = Use(IP(2))
                  Goto { target: BP(3) }

                BP(3): BasicBlock
                  IP(4) = Literal(I64(3))
                  IP(5) = Literal(I64(4))
                  IP(6) = Binary(AddI64, IP(4), IP(5))
                  Return(IP(6))
            "#]],
        );
        check_build_mir(
            "fn branch(x: bool) -> () { if (x) {} else (); }",
            expect![[r#"
                fn branch:
                BP(0): BasicBlock
                  If { cond: IP(0), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(1) = Literal(UnitValue)
                  IP(3) = Use(IP(1))
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(2) = Literal(UnitValue)
                  IP(3) = Use(IP(2))
                  Goto { target: BP(3) }

                BP(3): BasicBlock
                  IP(4) = Literal(UnitValue)
                  Return(IP(4))
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
                  IP(1) = Literal(I64(0))
                  IP(2) = Binary(EqI64, IP(0), IP(1))
                  If { cond: IP(2), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(3) = Literal(I64(0))
                  IP(15) = Use(IP(3))
                  Goto { target: BP(6) }

                BP(2): BasicBlock
                  IP(4) = Literal(I64(1))
                  IP(5) = Binary(EqI64, IP(0), IP(4))
                  If { cond: IP(5), true_branch: BP(3), false_branch: BP(4) }

                BP(3): BasicBlock
                  IP(6) = Literal(I64(1))
                  IP(14) = Use(IP(6))
                  Goto { target: BP(5) }

                BP(4): BasicBlock
                  IP(7) = Literal(I64(1))
                  IP(8) = Binary(SubI64, IP(0), IP(7))
                  IP(9) = FnCall("f", [IP(8)])
                  IP(10) = Literal(I64(2))
                  IP(11) = Binary(SubI64, IP(0), IP(10))
                  IP(12) = FnCall("f", [IP(11)])
                  IP(13) = Binary(AddI64, IP(9), IP(12))
                  IP(14) = Use(IP(13))
                  Goto { target: BP(5) }

                BP(5): BasicBlock
                  IP(15) = Use(IP(14))
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

    #[test]
    fn fn_mut_errors() {
        check_build_mir(
            "fn f(x: i64) -> i64 {let y=0; y=5; ret y;}",
            expect![
                "At 31: Identifier 'y' is immutable. Try declaring it with 'let mut y' instead."
            ],
        );
        check_build_mir(
            "fn f(x: i64) -> i64 {x=5; ret x;}",
            // TODO-errormsg: wasn't declared with 'let', so inappropriate error message.
            expect![
                "At 22: Identifier 'x' is immutable. Try declaring it with 'let mut x' instead."
            ],
        );
        check_build_mir(
            "fn f(x: i64) -> i64 {f=5; ret f;}",
            // TODO-errormsg: include 'f' in scope.
            expect!["At 22: Identifier 'f' not found in the local scope of the function."],
        );
    }

    #[test]
    fn fn_mut() {
        check_build_mir(
            "fn f() -> i64 {
                let mut y = 0;
                y = 5;
                ret y;
            }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(0) = Literal(I64(0))
                  IP(1) = Literal(I64(5))
                  IP(0) = Use(IP(1))
                  IP(2) = Literal(UnitValue)
                  Return(IP(0))
            "#]],
        );
        check_build_mir(
            "fn f(x: i64) -> i64 {
                let mut y = 0;
                if (x > 10) y = x;
                ret y;
            }",
            expect![[r#"
                fn f:
                BP(0): BasicBlock
                  IP(1) = Literal(I64(0))
                  IP(2) = Literal(I64(10))
                  IP(3) = Binary(GtI64, IP(0), IP(2))
                  If { cond: IP(3), true_branch: BP(1), false_branch: BP(2) }

                BP(1): BasicBlock
                  IP(1) = Use(IP(0))
                  IP(4) = Literal(UnitValue)
                  IP(6) = Use(IP(4))
                  Goto { target: BP(3) }

                BP(2): BasicBlock
                  IP(5) = Literal(UnitValue)
                  IP(6) = Use(IP(5))
                  Goto { target: BP(3) }

                BP(3): BasicBlock
                  Return(IP(1))
            "#]],
        );
    }
}
