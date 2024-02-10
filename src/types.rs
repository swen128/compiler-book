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

    pub fn common_type(a: &Self, b: &Self) -> Option<Self> {
        if a.is_subtype_of(b) {
            Some(b.clone())
        } else if b.is_subtype_of(a) {
            Some(a.clone())
        } else {
            None
        }
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

    pub fn is_comparable_to(&self, other: &Self) -> bool {
        self != &Ty::Unknown && other != &Ty::Unknown && Self::common_type(self, other).is_some()
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

#[cfg(test)]
mod tests {
    use crate::symbol::symbol;

    use super::*;

    #[test]
    fn test_common_ancestor() {
        let int = Ty::Int;
        let string = Ty::String;
        let record_a = Ty::record([(symbol("foo"), Ty::Int)].into_iter());
        let record_b = Ty::record([(symbol("bar"), Ty::Int)].into_iter());
        let array_a = Ty::array(Ty::Int);
        let array_b = Ty::array(Ty::String);
        let nil = Ty::Nil;
        let unknown = Ty::Unknown;

        same_type(&int);
        same_type(&string);
        same_type(&record_a);
        same_type(&array_a);
        same_type(&nil);
        same_type(&unknown);

        case(&int, &string, None);
        case(&int, &record_a, None);
        case(&int, &array_a, None);
        case(&int, &nil, None);

        case(&string, &record_a, None);
        case(&string, &array_a, None);
        case(&string, &nil, None);

        case(&record_a, &record_b, None);
        case(&record_a, &array_a, None);

        case(&array_a, &array_b, None);
        case(&array_a, &nil, None);

        case(&nil, &record_a, Some(&record_a));

        case(&int, &unknown, Some(&int));
        case(&string, &unknown, Some(&string));
        case(&record_a, &unknown, Some(&record_a));
        case(&array_a, &unknown, Some(&array_a));
        case(&nil, &unknown, Some(&nil));

        fn case(a: &Ty, b: &Ty, expected: Option<&Ty>) {
            assert_eq!(
                Ty::common_type(a, b),
                expected.cloned(),
                "The common type of ({:?}, {:?}) is {:?}",
                a,
                b,
                expected
            );
            assert_eq!(
                Ty::common_type(b, a),
                expected.cloned(),
                "The common type of ({:?}, {:?}) is {:?}",
                b,
                a,
                expected
            );
        }

        fn same_type(ty: &Ty) {
            assert_eq!(Ty::common_type(ty, ty), Some(ty.clone()));
        }
    }
}
