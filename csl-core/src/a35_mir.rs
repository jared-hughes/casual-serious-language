pub(crate) use crate::ast::Lit;
use crate::{
    intrinsics::{OP1, OP2},
    span::Span,
};
use crate::{runtime_value::RuntimeValue, types::Type};
use index_vec::{index_vec, IndexVec};
use std::collections::HashMap;
use std::fmt;

pub(crate) use RValue::*;
#[derive(Clone, Debug)]
pub(crate) enum RValue {
    /// e.g. `1.0`;
    Literal(RuntimeValue),
    /// Pass through the argument unchanged.
    Use(IP),
    /// e.g. `-x`
    Unary(OP1, IP),
    /// e.g. `x + y`
    Binary(OP2, IP, IP),
    /// e.g. `f(x)`
    FnCall(String, Vec<IP>),
}

pub(crate) use Statement::*;
#[derive(Clone)]
pub(crate) enum Statement {
    Assign(IP, RValue, Span),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Assign(ip, rvalue, _span) => write!(f, "{:?} = {:?}", ip, rvalue),
        }
    }
}

pub(crate) struct Program {
    pub(crate) fns: HashMap<String, FnBody>,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub(crate) fn new() -> Program {
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
    pub(crate) struct BP = u32;
    MAX_INDEX = 0xFFFF_FF00;
    DEBUG_FORMAT = "BP({})";
}

pub(crate) struct FnBody {
    pub(crate) params: Vec<Type>,
    blocks: IndexVec<BP, BasicBlockData>,
    types: IndexVec<IP, Type>,
}

impl FnBody {
    pub(crate) fn new(params: Vec<Type>) -> FnBody {
        let types = IndexVec::from_vec(params.clone());
        FnBody {
            params,
            blocks: index_vec![],
            types,
        }
    }

    pub(crate) fn new_basic_block(&mut self) -> BP {
        let ind = self.blocks.len();
        let block = BP::from_usize(ind);
        self.blocks.push(BasicBlockData::new());
        block
    }

    pub(crate) fn get_type(&self, ip: IP) -> Type {
        self.types[ip]
    }

    /// Create a new IP (local var) and assign to it.
    pub(crate) fn push_assign_new_ip(
        &mut self,
        block: BP,
        inst: RValue,
        ty: Type,
        span: Span,
    ) -> IP {
        assert!(block < self.blocks.len());
        let ip = self.push_local(ty);
        self.blocks[block].stmts.push(Assign(ip, inst, span));
        ip
    }

    pub(crate) fn push_constant(&mut self, block: BP, val: RuntimeValue, span: Span) -> IP {
        self.push_assign_new_ip(block, Literal(val), val.get_type(), span)
    }

    pub(crate) fn push_constant_bi(
        &mut self,
        block: BP,
        val: RuntimeValue,
        span: Span,
    ) -> (BP, IP) {
        (block, self.push_constant(block, val, span))
    }

    pub(crate) fn push_unit_new_ip(&mut self, block: BP, span: Span) -> IP {
        self.push_constant(block, RuntimeValue::UnitValue, span)
    }

    /// Create a new IP (local var)
    pub(crate) fn push_local(&mut self, value_type: Type) -> IP {
        let ind = self.types.len();
        let ip = IP::from_usize(ind);
        self.types.push(value_type);
        ip
    }

    pub(crate) fn num_locals(&self) -> usize {
        self.types.len()
    }

    /// Push a statement, checked.
    pub(crate) fn push(&mut self, block: BP, stmt: Statement) {
        assert!(block < self.blocks.len());
        let Assign(ip, ref rval, _) = stmt;
        self.sanity_check_assign(ip, rval);
        self.blocks[block].stmts.push(stmt);
    }

    pub(crate) fn set_terminator(&mut self, block: BP, term: Terminator) {
        assert!(block < self.blocks.len());
        self.blocks[block].terminator = term;
    }

    pub(crate) fn get_terminator(&self, block: BP) -> &Terminator {
        &self.blocks[block].terminator
    }

    #[mutants::skip] // This is a no-op when codegen is correct.
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
            FnCall(_, ref args) => {
                for a in args {
                    assert!(a < &ip);
                }
            }
        }
    }

    pub(crate) fn iter_stmts(&self, block: BP) -> std::slice::Iter<Statement> {
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
    pub(crate) struct IP = u32;
    MAX_INDEX = 0xFFFF_FF00;
    DEBUG_FORMAT = "IP({})";
}

pub(crate) use Terminator::*;
#[derive(Clone, Debug)]
pub(crate) enum Terminator {
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
    pub(crate) stmts: IndexVec<IP, Statement>,
    pub(crate) terminator: Terminator,
}

impl BasicBlockData {
    pub(crate) fn new() -> BasicBlockData {
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
