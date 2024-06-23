pub use crate::ast::{
    BinOpKind,
    Lit::{self, *},
};
use crate::intrinsics::OP2;
use crate::span::Span;
use crate::types::Type;
use index_vec::IndexVec;
use std::fmt;

pub use InstInner::*;
#[derive(Clone, Copy, Debug)]
pub enum InstInner {
    Literal(Lit),
    Binary(OP2, IP, IP),
}

#[derive(Clone, Copy)]
pub struct Inst {
    pub kind: InstInner,
    pub value_type: Type,
    pub span: Span,
}

impl fmt::Debug for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?}", self.value_type, self.kind)
    }
}

/* Indexing */

index_vec::define_index_type! {
    pub struct IP = u32;
    MAX_INDEX = 0xFFFF_FF00;
    DEBUG_FORMAT = "IP({})";
}

#[derive(Clone)]
pub struct BasicBlock {
    vec: IndexVec<IP, Inst>,
    return_index: IP,
}

// impl Index<IP> for BasicBlock {
//     type Output = Inst;

//     #[inline(always)]
//     fn index(&self, i: IP) -> &Self::Output {
//         &self.vec[i]
//     }
// }

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            vec: IndexVec::new(),
            return_index: IP::from_usize(0),
        }
    }

    pub fn set_return(&mut self, ip: IP) {
        assert!(ip < self.len());
        self.return_index = ip;
    }

    pub fn get_return(&self) -> IP {
        self.return_index
    }

    pub fn push(&mut self, inst: InstInner, span: Span) -> IP {
        let ind = self.vec.len();
        let ip = IP::from_usize(ind);
        let value_type = self.compute_type(inst, ip);
        self.vec.push(Inst {
            kind: inst,
            value_type,
            span,
        });
        return ip;
    }

    pub fn get_type(&self, ip: IP) -> Type {
        return self.vec[ip].value_type;
    }

    /// Compute the return type of the instruction, and do basic sanity checks.
    fn compute_type(&self, inst: InstInner, ip: IP) -> Type {
        match inst {
            Literal(Integer(_)) => Type::I64,
            Literal(Float(_)) => Type::F64,
            Binary(op, a, b) => {
                assert!(a < ip);
                assert!(b < ip);
                let info = op.get_intrinsic();
                assert!(info.param_types.0 == self.get_type(a));
                assert!(info.param_types.1 == self.get_type(b));
                info.return_type
            }
        }
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn iter(&self) -> core::slice::Iter<'_, Inst> {
        self.vec.iter()
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BasicBlock")?;
        for (i, inst) in self.vec.iter().enumerate() {
            write!(f, "\n{i:>4}: {inst:?}")?;
        }
        Ok(())
    }
}
