use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Symbol {
    name: String,
}

pub fn symbol(name: &str) -> Symbol {
    Symbol::from(name)
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl Symbol {
    fn name(&self) -> &str {
        &self.name
    }
}

pub struct SymbolTable<T> {
    symbols: HashMap<String, Vec<T>>,

    top_level_scope: Vec<String>,

    /// Scopes of symbol names in the order of their insertion.
    history: Vec<Vec<String>>,
}

impl<T> SymbolTable<T> {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            top_level_scope: vec![],
            history: vec![],
        }
    }

    pub fn insert(&mut self, symbol: Symbol, value: T) {
        self.symbols
            .entry(symbol.name.clone())
            .or_insert_with(Vec::new)
            .push(value);
        self.current_scope().push(symbol.name);
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&T> {
        self.symbols.get(symbol.name()).and_then(|v| v.last())
    }

    pub fn begin_scope(&mut self) {
        self.history.push(vec![]);
    }

    pub fn end_scope(&mut self) {
        let scope = self
            .history
            .pop()
            .expect("Attempted to end top-level scope");

        for symbol in scope.iter().rev() {
            self.symbols.get_mut(symbol).unwrap().pop();
        }
    }

    fn current_scope(&mut self) -> &mut Vec<String> {
        self.history.last_mut().unwrap_or(&mut self.top_level_scope)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let table = SymbolTable::<()>::new();
        assert_eq!(table.get(&symbol("a")), None);
    }

    #[test]
    fn single() {
        let mut table = SymbolTable::new();
        table.insert(symbol("a"), 1);
        assert_eq!(table.get(&symbol("a")), Some(&1));
    }

    #[test]
    fn shadowing() {
        let mut table = SymbolTable::new();
        table.insert(symbol("a"), 1);
        table.insert(symbol("a"), 2);
        assert_eq!(table.get(&symbol("a")), Some(&2));
    }

    #[test]
    fn shadowing_scopes() {
        let mut table = SymbolTable::new();
        table.insert(symbol("a"), 1);

        table.begin_scope();
        table.insert(symbol("a"), 2);
        assert_eq!(table.get(&symbol("a")), Some(&2));
        table.end_scope();

        assert_eq!(table.get(&symbol("a")), Some(&1));
    }

    #[test]
    fn nested_scopes() {
        let mut table = SymbolTable::new();
        table.insert(symbol("a"), 1);

        table.begin_scope();
        table.insert(symbol("a"), 2);
        assert_eq!(table.get(&symbol("a")), Some(&2));

        table.begin_scope();
        table.insert(symbol("a"), 3);
        assert_eq!(table.get(&symbol("a")), Some(&3));
        table.end_scope();

        assert_eq!(table.get(&symbol("a")), Some(&2));
        table.end_scope();

        assert_eq!(table.get(&symbol("a")), Some(&1));
    }

    #[test]
    #[should_panic]
    fn end_scope_panics_on_unmatched_call() {
        SymbolTable::<u8>::new().end_scope();
    }
}
