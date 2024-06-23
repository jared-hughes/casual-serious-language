use crate::ast::*;
use crate::build_mir_err::BuildMIRErr;
use crate::errors::{Diag, Diagnostic};
use crate::intrinsics::OP2;
use crate::mir::{self, BasicBlock, IP};
use crate::types::*;

pub fn ast_to_mir(program: Expr) -> Result<BasicBlock, Diag> {
    Frontend::build_mir(program)
}

struct Frontend {
    block: BasicBlock,
}

impl Frontend {
    pub fn build_mir(program: Expr) -> Result<BasicBlock, Diag> {
        let mut frontend = Frontend {
            block: BasicBlock::new(),
        };
        let ret = frontend.add_to_mir(program)?;
        frontend.block.set_return(ret);
        Ok(frontend.block)
    }

    fn add_to_mir(&mut self, ex: Expr) -> Result<IP, Diag> {
        Ok(match ex.body {
            Binary(op, left_node, right_node) => {
                let left = self.add_to_mir(*left_node)?;
                let right = self.add_to_mir(*right_node)?;
                self.add_binary_to_mir(op, left, right)?
            }
            Literal(lit) => self.block.push(mir::Literal(lit), ex.span),
            Ident(_) => Err(BuildMIRErr::IdentifiersUnsupported(ex.span).into_diag())?,
        })
    }

    fn add_binary_to_mir(&mut self, op: BinOp, left: IP, right: IP) -> Result<IP, Diag> {
        let left_type = self.block.get_type(left);
        let right_type = self.block.get_type(right);
        let op2 = match (op.node, left_type, right_type) {
            (Add, I64, I64) => OP2::AddI64,
            (Add, F64, F64) => OP2::AddF64,
            (Sub, I64, I64) => OP2::SubI64,
            (Sub, F64, F64) => OP2::SubF64,
            (Mul, I64, I64) => OP2::MulI64,
            (Mul, F64, F64) => OP2::MulF64,
            (Div, I64, I64) => OP2::DivI64,
            (Div, F64, F64) => OP2::DivF64,
            (_, _, _) => {
                Err(BuildMIRErr::InvalidTypeBinary(op, left_type, right_type).into_diag())?
            }
        };
        return Ok(self.block.push(mir::Binary(op2, left, right), op.span));
    }
}

#[cfg(test)]
mod frontend_tests {
    use super::*;
    use crate::parser::parse;
    use expect_test::{expect, Expect};

    fn mir_from_string(input: &str) -> Result<BasicBlock, Diag> {
        Ok(ast_to_mir(*parse(input)?)?)
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
            "12*3+8/4",
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
    fn error_unsupported() {
        check_build_mir("x+1", expect!["At 1: Identifier? I hardly know her."]);
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
            expect!["At 5: Type error: Cannot perform f64 + i64"],
        );
        check_build_mir(
            "1 + 2.0",
            expect!["At 3: Type error: Cannot perform i64 + f64"],
        );
        check_build_mir(
            "1.0 - 2",
            expect!["At 5: Type error: Cannot perform f64 - i64"],
        );
        check_build_mir(
            "1 - 2.0",
            expect!["At 3: Type error: Cannot perform i64 - f64"],
        );
        check_build_mir(
            "1.0 * 2",
            expect!["At 5: Type error: Cannot perform f64 * i64"],
        );
        check_build_mir(
            "1 * 2.0",
            expect!["At 3: Type error: Cannot perform i64 * f64"],
        );
        check_build_mir(
            "1.0 / 2",
            expect!["At 5: Type error: Cannot perform f64 / i64"],
        );
        check_build_mir(
            "1 / 2.0",
            expect!["At 3: Type error: Cannot perform i64 / f64"],
        );
    }
}
