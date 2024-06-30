pub use crate::ast::{
    BinOpKind,
    Lit::{self, *},
};
use crate::intrinsics::{OP1, OP2};
use crate::span::Span;
use crate::types::Type;
use index_vec::IndexVec;
use std::collections::HashMap;
use std::fmt;

pub use InstInner::*;
#[derive(Clone, Debug)]
pub enum InstInner {
    /// Basic block argument: if this is the `i`th instruction of the basic
    /// block, then load argument number `i`.
    LoadArg(Type),
    Literal(Lit),
    Unary(OP1, IP),
    Binary(OP2, IP, IP),
    FnCall(String, Vec<IP>, Type),
}

#[derive(Clone)]
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

pub struct Program {
    pub fns: HashMap<String, BasicBlock>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            fns: HashMap::new(),
        }
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fns = self.fns.iter().collect::<Vec<_>>();
        fns.sort_by_key(|x| x.0);
        for (i, (fn_name, block)) in fns.into_iter().enumerate() {
            write!(f, "fn {fn_name} {block:#?}")?;
            if i != self.fns.len() - 1 {
                writeln!(f, "")?;
            }
        }
        Ok(())
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
    pub params: Vec<Type>,
    vec: IndexVec<IP, Inst>,
    return_index: IP,
}

impl BasicBlock {
    pub fn new(params: Vec<Type>) -> BasicBlock {
        BasicBlock {
            params,
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
        let value_type = self.compute_type(&inst, ip);
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
    fn compute_type(&self, inst: &InstInner, ip: IP) -> Type {
        match *inst {
            LoadArg(ty) => ty,
            Literal(Integer(_)) => Type::I64,
            Literal(Float(_)) => Type::F64,
            Unary(op, a) => {
                assert!(a < ip);
                let info = op.get_intrinsic();
                assert!(info.param_types == self.get_type(a));
                info.return_type
            }
            Binary(op, a, b) => {
                assert!(a < ip);
                assert!(b < ip);
                let info = op.get_intrinsic();
                assert!(info.param_types.0 == self.get_type(a));
                assert!(info.param_types.1 == self.get_type(b));
                info.return_type
            }
            FnCall(_, ref args, value_type) => {
                for a in args {
                    assert!(a < &ip);
                }
                value_type
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
