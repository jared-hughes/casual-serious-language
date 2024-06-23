pub use crate::ast::{
    BinOpKind,
    Lit::{self, *},
};
use index_vec::IndexVec;
use std::fmt;
use std::ops::Index;

pub use Inst::*;
#[derive(Clone, Copy, Debug)]
pub enum Inst {
    Literal(Lit),
    Binary(BinOpKind, IP, IP),
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

impl Index<IP> for BasicBlock {
    type Output = Inst;

    #[inline(always)]
    fn index(&self, i: IP) -> &Self::Output {
        &self.vec[i]
    }
}

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

    pub fn push(&mut self, inst: Inst) -> IP {
        let ind = self.vec.len();
        self.vec.push(inst);
        let ip = IP::from_usize(ind);
        self.validate_inst(inst, ip);
        ip
    }

    fn validate_inst(&self, inst: Inst, ip: IP) {
        // TODO: Typechecking, once we have floats and such.
        match inst {
            Literal(_) => (),
            Binary(_, a, b) => {
                assert!(a < ip);
                assert!(b < ip);
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
