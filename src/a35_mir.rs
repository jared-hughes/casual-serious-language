pub use crate::ast::{
    BinOp,
    Lit::{self, *},
};
use std::fmt;
use std::ops::Index;

pub use Inst::*;
#[derive(Clone, Copy, Debug)]
pub enum Inst {
    Literal(Lit),
    Binary(BinOp, IP, IP),
}

/* Indexing */

// TODO: look into [dependencies] index_vec = "0.1.3"
// index_vec::define_index_type! { pub struct IP = u32; }
// OR: rustc_index seems a bit nicer.

/// An instruction pointer into a basic block
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct IP(pub u32);

impl IP {
    #[inline]
    pub fn inc(&self) -> Self {
        IP(self.0 + 1)
    }

    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Extracts the value of this index as a `usize`.
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone)]
pub struct BasicBlock {
    vec: Vec<Inst>,
    return_index: IP,
}

const IP_MAX: usize = 0xFFFF_FF00;

impl Index<IP> for BasicBlock {
    type Output = Inst;

    #[inline(always)]
    fn index(&self, i: IP) -> &Self::Output {
        &self.vec[i.0 as usize]
    }
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            vec: Vec::new(),
            return_index: IP(0),
        }
    }

    pub fn set_return(&mut self, ip: IP) {
        assert!(ip.as_usize() < self.len());
        self.return_index = ip;
    }

    pub fn get_return(&self) -> IP {
        self.return_index
    }

    pub fn push(&mut self, inst: Inst) -> IP {
        let ind = self.vec.len();
        assert!(ind <= IP_MAX);
        self.vec.push(inst);
        let ip = IP(ind as u32);
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
