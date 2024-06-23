use crate::ast::*;
use crate::build_mir_err::BuildMIRErr;
use crate::errors::{Diag, Diagnostic};
use crate::mir::{self, BasicBlock, IP};

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
        Ok(match ex {
            Binary(op, left, right) => {
                let left_ip = self.add_to_mir(*left)?;
                let right_ip = self.add_to_mir(*right)?;
                self.block.push(mir::Binary(op, left_ip, right_ip))
            }
            Literal(lit) => self.block.push(mir::Literal(lit)),
            Ident(_) => Err(BuildMIRErr::IdentifiersUnsupported.into_diag())?,
        })
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
               0: Literal(Integer(12))
               1: Literal(Integer(3))
               2: Binary(Mul, IP(0), IP(1))
               3: Literal(Integer(8))
               4: Literal(Integer(4))
               5: Binary(Div, IP(3), IP(4))
               6: Binary(Add, IP(2), IP(5))"#]],
        );
    }
}
