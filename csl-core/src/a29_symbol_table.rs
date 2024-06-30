use std::collections::HashMap;

use crate::mir::IP;

pub struct SymbolTable<'ctx> {
    map: HashMap<String, IP>,
    parent: Option<Box<&'ctx SymbolTable<'ctx>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn set_symbol(&mut self, name: String, ip: IP) {
        self.map.insert(name, ip);
    }

    // TODO-perf: consider caching to avoid walking up a long table tree.
    pub fn get_symbol(&self, symb: &str) -> Option<IP> {
        if let Some(ip) = self.map.get(symb).cloned() {
            // TODO: IP should derive Copy.
            return Some(ip.clone());
        }
        if let Some(p) = &self.parent {
            return p.get_symbol(symb);
        }
        None
    }

    fn new_with_parent(parent: Option<Box<&'ctx SymbolTable<'ctx>>>) -> Self {
        Self {
            map: HashMap::new(),
            parent,
        }
    }

    pub fn new() -> Self {
        Self::new_with_parent(None)
    }

    pub fn child(&'ctx self) -> Self {
        Self::new_with_parent(Some(Box::new(self)))
    }
}
