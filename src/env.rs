use crate::{
    ast::{Id, TypeId},
    temp::Label,
};

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
        Self { table }
    }

    pub fn get(&self, symbol: &Id) -> Option<&ValueEntry<F>> {
        self.table.get(&Symbol::from(symbol))
    }

    pub fn insert(&mut self, id: Id, value: ValueEntry<F>) {
        self.table.insert(Symbol::from(id), value);
    }

    pub fn begin_scope(&mut self) {
        self.table.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.table.end_scope();
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

    pub fn get(&self, id: &TypeId) -> Option<&Ty> {
        self.table.get(&Symbol::from(id))
    }

    pub fn insert(&mut self, id: &TypeId, ty: Ty) {
        self.table.insert(Symbol::from(id), ty);
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

impl From<&TypeId> for Symbol {
    fn from(id: &TypeId) -> Self {
        Self::from(id.0.as_str())
    }
}

impl From<&Id> for Symbol {
    fn from(id: &Id) -> Self {
        Self::from(id.0.as_str())
    }
}
