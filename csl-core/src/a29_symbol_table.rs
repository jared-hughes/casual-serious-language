use std::collections::HashMap;

use crate::ast::Mutability;
use crate::mir::IP;

pub(crate) use SymbolValue::*;
#[derive(Clone, Copy)]
pub(crate) enum SymbolValue {
    Local { ip: IP, mutability: Mutability },
}

pub(crate) struct SymbolTable<'ctx> {
    map: HashMap<String, SymbolValue>,
    parent: Option<&'ctx SymbolTable<'ctx>>,
}

impl<'ctx> Default for SymbolTable<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> SymbolTable<'ctx> {
    pub(crate) fn set_symbol(&mut self, name: String, sv: SymbolValue) {
        self.map.insert(name, sv);
    }

    // TODO-perf: consider caching to avoid walking up a long table tree.
    pub(crate) fn get_symbol(&self, symb: &str) -> Option<&SymbolValue> {
        if let Some(sv) = self.map.get(symb) {
            return Some(sv);
        }
        if let Some(p) = &self.parent {
            return p.get_symbol(symb);
        }
        None
    }

    pub(crate) fn has_symbol(&self, symb: &str) -> bool {
        if self.map.contains_key(symb) {
            return true;
        }
        if let Some(p) = &self.parent {
            return p.has_symbol(symb);
        }
        false
    }

    fn new_with_parent(parent: Option<&'ctx SymbolTable<'ctx>>) -> Self {
        Self {
            map: HashMap::new(),
            parent,
        }
    }

    pub(crate) fn new() -> Self {
        Self::new_with_parent(None)
    }

    pub(crate) fn child(&'ctx self) -> Self {
        Self::new_with_parent(Some(self))
    }
}
