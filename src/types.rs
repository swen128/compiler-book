use super::symbol::Symbol;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ty {
    Int,
    String,
    Record(UniqueId, Vec<RecordField>),
    Array(UniqueId, Box<Ty>),
    Nil,
    Unit,
    Name(Symbol, Box<Ty>),
    
    Unknown,
}

impl Ty {
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Ty::Nil, Ty::Record(_, _)) => true,
            _ => self == other,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RecordField {
    pub key: Symbol,
    pub ty: Ty,
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
