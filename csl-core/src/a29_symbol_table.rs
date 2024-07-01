use std::collections::HashMap;

use crate::mir::IP;

pub(crate) struct SymbolTable<'ctx> {
    map: HashMap<String, IP>,
    parent: Option<&'ctx SymbolTable<'ctx>>,
}

impl<'ctx> Default for SymbolTable<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> SymbolTable<'ctx> {
    pub(crate) fn set_symbol(&mut self, name: String, ip: IP) {
        self.map.insert(name, ip);
    }

    // TODO-perf: consider caching to avoid walking up a long table tree.
    pub(crate) fn get_symbol(&self, symb: &str) -> Option<IP> {
        if let Some(ip) = self.map.get(symb).cloned() {
            // TODO: IP should derive Copy.
            return Some(ip);
        }
        if let Some(p) = &self.parent {
            return p.get_symbol(symb);
        }
        None
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
