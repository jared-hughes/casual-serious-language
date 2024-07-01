pub use crate::ast::{
    BinOpKind,
    Lit::{self, *},
};
use crate::types::Type;
use crate::{
    intrinsics::{OP1, OP2},
    span::Span,
};
use index_vec::{index_vec, IndexVec};
use std::collections::HashMap;
use std::fmt;

pub use RValue::*;
#[derive(Clone, Debug)]
pub enum RValue {
    /// e.g. `1.0`;
    Literal(Lit),
    /// Pass through the argument unchanged.
    /// TODO: Don't want to include type here, and on FnCall.
    Use(IP, Type),
    /// e.g. `-x`
    Unary(OP1, IP),
    /// e.g. `x + y`
    Binary(OP2, IP, IP),
    /// e.g. `f(x)`
    FnCall(String, Vec<IP>, Type),
}

pub use Statement::*;
#[derive(Clone)]
pub enum Statement {
    Assign(IP, RValue, Span),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Assign(ip, rvalue, _span) => write!(f, "{:?} = {:?}", ip, rvalue),
        }
    }
}

pub struct Program {
    pub fns: HashMap<String, FnBody>,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
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
        for (i, (fn_name, fn_block)) in fns.iter().enumerate() {
            writeln!(f, "fn {fn_name}:")?;
            write!(f, "{fn_block:#?}")?;
            if i != self.fns.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

index_vec::define_index_type! {
    /// Block pointer into a vec of basic blocks
    pub struct BP = u32;
    MAX_INDEX = 0xFFFF_FF00;
    DEBUG_FORMAT = "BP({})";
}

pub struct FnBody {
    pub params: Vec<Type>,
    blocks: IndexVec<BP, BasicBlockData>,
    types: IndexVec<IP, Type>,
}

impl FnBody {
    pub fn new(params: Vec<Type>) -> FnBody {
        let types = IndexVec::from_vec(params.clone());
        FnBody {
            params,
            blocks: index_vec![],
            types,
        }
    }

    pub fn new_basic_block(&mut self) -> BP {
        let ind = self.blocks.len();
        let block = BP::from_usize(ind);
        self.blocks.push(BasicBlockData::new());
        block
    }

    pub fn get_type(&self, ip: IP) -> Type {
        self.types[ip]
    }

    /// Create a new IP (local var) and assign to it.
    pub fn push_assign_new_ip(&mut self, block: BP, inst: RValue, span: Span) -> IP {
        assert!(block < self.blocks.len());
        let value_type = self.compute_type(&inst);
        let ip = self.push_local(value_type);
        self.blocks[block].stmts.push(Assign(ip, inst, span));
        ip
    }

    pub fn assign_new_ip(&mut self, block: BP, inst: RValue, span: Span) -> (BP, IP) {
        assert!(block < self.blocks.len());
        let value_type = self.compute_type(&inst);
        let ip = self.push_local(value_type);
        self.blocks[block].stmts.push(Assign(ip, inst, span));
        (block, ip)
    }

    pub fn push_unit_new_ip(&mut self, block: BP, span: Span) -> IP {
        self.push_assign_new_ip(block, Literal(crate::ast::Lit::Unit), span)
    }

    /// Create a new IP (local var)
    pub fn push_local(&mut self, value_type: Type) -> IP {
        let ind = self.types.len();
        let ip = IP::from_usize(ind);
        self.types.push(value_type);
        ip
    }

    pub fn num_locals(&self) -> usize {
        self.types.len()
    }

    /// Push a statement, checked.
    pub fn push(&mut self, block: BP, stmt: Statement) {
        assert!(block < self.blocks.len());
        let Assign(ip, ref rval, _) = stmt;
        self.sanity_check_assign(ip, rval);
        self.blocks[block].stmts.push(stmt);
    }

    pub fn set_terminator(&mut self, block: BP, term: Terminator) {
        assert!(block < self.blocks.len());
        self.blocks[block].terminator = term;
    }

    pub fn get_terminator(&self, block: BP) -> &Terminator {
        &self.blocks[block].terminator
    }

    /// Compute the return type of the instruction, and do basic sanity checks.
    fn compute_type(&self, rval: &RValue) -> Type {
        match *rval {
            Use(_, value_type) => value_type,
            Literal(Integer(_)) => Type::I64,
            Literal(Float(_)) => Type::F64,
            Literal(Unit) => Type::Unit,
            Unary(op, ..) => op.get_intrinsic().return_type,
            Binary(op, ..) => op.get_intrinsic().return_type,
            FnCall(_, _, value_type) => value_type,
        }
    }

    fn sanity_check_assign(&self, ip: IP, rval: &RValue) {
        match *rval {
            Use(a, ..) => {
                assert!(a < ip);
            }
            Literal(..) => (),
            Unary(op, a) => {
                assert!(a < ip);
                let info = op.get_intrinsic();
                assert!(info.param_types == self.get_type(a));
            }
            Binary(op, a, b) => {
                assert!(a < ip);
                assert!(b < ip);
                let info = op.get_intrinsic();
                assert!(info.param_types.0 == self.get_type(a));
                assert!(info.param_types.1 == self.get_type(b));
            }
            FnCall(_, ref args, _) => {
                for a in args {
                    assert!(a < &ip);
                }
            }
        }
    }

    pub fn iter_stmts(&self, block: BP) -> std::slice::Iter<Statement> {
        self.blocks[block].stmts.iter()
    }
}

impl fmt::Debug for FnBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, block) in self.blocks.iter().enumerate() {
            write!(f, "{:?}: ", BP::from(i))?;
            write!(f, "{:?}", block)?;
            if i != self.blocks.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

index_vec::define_index_type! {
    /// IP stands for "instruction pointer" but this actually now means "local var"
    pub struct IP = u32;
    MAX_INDEX = 0xFFFF_FF00;
    DEBUG_FORMAT = "IP({})";
}

pub use Terminator::*;
#[derive(Clone, Debug)]
pub enum Terminator {
    /// Basic blocks should only be unterminated during construction.
    Unterminated,
    /// Exits the function, returning the value given in IP.
    Return(IP),
    /// Go to the start of that basic block.
    Goto { target: BP },
    /// Depending on the boolean in `cond`, branch to one of these two branches.
    If {
        cond: IP,
        true_branch: BP,
        false_branch: BP,
    },
}

#[derive(Clone)]
struct BasicBlockData {
    pub stmts: IndexVec<IP, Statement>,
    pub terminator: Terminator,
}

impl BasicBlockData {
    pub fn new() -> BasicBlockData {
        BasicBlockData {
            stmts: IndexVec::new(),
            terminator: Unterminated,
        }
    }
}

impl fmt::Debug for BasicBlockData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "BasicBlock")?;
        for inst in self.stmts.iter() {
            writeln!(f, "  {inst:?}")?;
        }
        writeln!(f, "  {:?}", self.terminator)?;
        Ok(())
    }
}
