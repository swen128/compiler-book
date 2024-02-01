use std::sync::atomic::{AtomicU32, Ordering};

use super::symbol::Symbol;

/// Represents a value temporarily held in a register.
#[derive(Debug, Clone, PartialEq)]
pub struct Temp {
    id: u32,
}

impl Temp {
    /// Creates a unique temporary.
    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        let id = COUNTER.fetch_add(1, Ordering::SeqCst);
        Self { id }
    }
}

pub struct TempTable {}

/// Represents some machine-language location whose exact address is yet to be determined.
#[derive(Debug, Clone, PartialEq)]
pub struct Label(Symbol);

impl Label {
    /// Creates a unique label.
    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        let id = COUNTER.fetch_add(1, Ordering::SeqCst);
        let symbol = Symbol::from(format!(".L{}", id));
        Self(symbol)
    }

    /// Creates a new label whose assembly-language name is the given string.
    pub fn named(name: &str) -> Self {
        todo!()
    }
}
