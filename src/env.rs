use super::{
    symbol::{symbol, Symbol, SymbolTable},
    types::Ty,
};

pub struct Environment {
    pub types: TypeTable,
    pub values: ValueTable,
}

pub struct TypeTable {
    table: SymbolTable<Ty>,
}

pub struct ValueTable {
    table: SymbolTable<ValueEntry>,
}

pub enum ValueEntry {
    Variable(Ty),
    Function { params: Vec<Ty>, result: Ty },
}

impl Environment {
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

impl ValueTable {
    /// Returns a table with the built-in functions.
    pub fn base() -> Self {
        let mut table = SymbolTable::new();
        table.insert(
            symbol("print"),
            ValueEntry::Function {
                params: vec![Ty::String],
                result: Ty::Unit,
            },
        );
        Self { table }
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&ValueEntry> {
        self.table.get(symbol)
    }

    pub fn insert(&mut self, symbol: Symbol, value: ValueEntry) {
        self.table.insert(symbol, value);
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

pub struct Scope<'a>(&'a mut Environment);

impl<'a> Scope<'a> {
    pub fn new(env: &'a mut Environment) -> Self {
        env.begin_scope();
        Self(env)
    }
}

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        self.0.end_scope();
    }
}

impl<'a> std::ops::Deref for Scope<'a> {
    type Target = Environment;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> std::ops::DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
