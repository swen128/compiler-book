use crate::temp::Label;

use super::{
    frame::Frame,
    symbol::{symbol, Symbol, SymbolTable},
    translate::Access,
    types::FunctionSignature,
    types::Ty,
};

pub struct Environment<F: Frame + Clone + PartialEq> {
    pub types: TypeTable,
    pub values: ValueTable<F>,
}

pub struct TypeTable {
    table: SymbolTable<Ty>,
}

pub struct ValueTable<F: Frame + Clone + PartialEq> {
    table: SymbolTable<ValueEntry<F>>,
    next_unique_id: usize,
}

pub enum ValueEntry<F: Frame + Clone + PartialEq> {
    Variable {
        ty: Ty,
        access: Access<F>,
    },
    Function {
        signature: FunctionSignature,
        label: Label,
    },
}

impl<F: Frame + Clone + PartialEq> ValueEntry<F> {
    pub fn func(params: Vec<Ty>, result: Ty) -> Self {
        Self::Function {
            signature: FunctionSignature { params, result },
            label: Label::new(),
        }
    }
}

impl<F: Frame + Clone + PartialEq> Environment<F> {
    /// Returns an environment with the built-in types and functions.
    pub fn base() -> Self {
        Self {
            types: TypeTable::base(),
            values: ValueTable::base(),
        }
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.values.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.values.end_scope();
    }
}

impl<F: Frame + Clone + PartialEq> ValueTable<F> {
    /// Returns a table with the built-in functions.
    pub fn base() -> Self {
        let mut table = SymbolTable::new();
        table.insert(
            symbol("print"),
            ValueEntry::func(vec![Ty::String], Ty::Unit),
        );
        Self {
            table,
            next_unique_id: 0,
        }
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&ValueEntry<F>> {
        self.table.get(symbol)
    }

    pub fn insert(&mut self, symbol: Symbol, value: ValueEntry<F>) {
        self.table.insert(symbol, value);
    }

    pub fn begin_scope(&mut self) {
        self.table.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.table.end_scope();
    }

    /// Returns a unique symbol that is not already defined in the table.
    /// Note: This function is NOT thread-safe.
    pub fn unique_symbol(&mut self) -> Symbol {
        let mut symbol;
        loop {
            symbol = Symbol::from(format!("_unique_{}", self.next_unique_id));

            if !self.table.has(&symbol) {
                break symbol;
            } else {
                self.next_unique_id += 1;
            }
        }
    }
}

impl TypeTable {
    /// Returns a table with the built-in types.
    pub fn base() -> Self {
        let mut table = SymbolTable::new();
        table.insert(symbol("int"), Ty::Int);
        table.insert(symbol("string"), Ty::String);
        Self { table }
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&Ty> {
        self.table.get(symbol)
    }

    pub fn insert(&mut self, symbol: Symbol, ty: Ty) {
        self.table.insert(symbol, ty);
    }

    pub fn begin_scope(&mut self) {
        self.table.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.table.end_scope();
    }
}

pub struct Scope<'a, F: Frame + Clone + PartialEq>(&'a mut Environment<F>);

impl<'a, F: Frame + Clone + PartialEq> Scope<'a, F> {
    pub fn new(env: &'a mut Environment<F>) -> Self {
        env.begin_scope();
        Self(env)
    }
}

impl<'a, F: Frame + Clone + PartialEq> Drop for Scope<'a, F> {
    fn drop(&mut self) {
        self.0.end_scope();
    }
}

impl<'a, F: Frame + Clone + PartialEq> std::ops::Deref for Scope<'a, F> {
    type Target = Environment<F>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, F: Frame + Clone + PartialEq> std::ops::DerefMut for Scope<'a, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
