use std::collections::HashMap;

use super::symbol::Symbol;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ty {
    Int,
    String,
    Record(UniqueId, RecordFields),
    Array(UniqueId, Box<Ty>),
    Nil,
    Unit,
    Name(Symbol, Box<Ty>),

    Unknown,
}

impl Ty {
    pub fn array(element_ty: Ty) -> Self {
        Self::Array(UniqueId::new(), Box::new(element_ty))
    }

    pub fn record(fields: impl Iterator<Item = (Symbol, Ty)>) -> Self {
        Self::Record(UniqueId::new(), RecordFields::new(fields))
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Ty::Nil, Ty::Record(_, _)) => true,
            (Ty::Unknown, _) => true,

            (Ty::Record(id, _), Ty::Record(other_id, _)) => id == other_id,
            (Ty::Array(id, _), Ty::Array(other_id, _)) => id == other_id,

            _ => self == other,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RecordFields(HashMap<Symbol, (usize, Ty)>);

impl RecordFields {
    pub fn new(fields: impl Iterator<Item = (Symbol, Ty)>) -> Self {
        let mut map = HashMap::new();
        // The index of unique fields in the record.
        let mut i = 0;

        for (key, value) in fields {
            match map.get(&key) {
                Some(_) => {
                    // Duplicate keys are overwritten by the last one.
                    map.insert(key, (i, value));
                }
                None => {
                    map.insert(key, (i, value));
                    i += 1;
                }
            }
        }

        Self(map)
    }

    pub fn get(&self, key: &Symbol) -> Option<&(usize, Ty)> {
        self.0.get(key)
    }
    
    pub fn keys(&self) -> impl Iterator<Item = &Symbol> {
        self.0.keys()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub params: Vec<Ty>,
    pub result: Ty,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UniqueId(pub usize);

impl UniqueId {
    fn new() -> Self {
        static mut COUNTER: usize = 0;
        unsafe {
            let id = COUNTER;
            COUNTER += 1;
            Self(id)
        }
    }
}

pub struct IdGenerator {
    counter: usize,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn next(&mut self) -> UniqueId {
        let id = self.counter;
        self.counter += 1;
        UniqueId(id)
    }
}
