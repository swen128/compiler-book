use super::symbol::Symbol;

/// Represents a value temporarily held in a register.
#[derive(Debug, Clone, PartialEq)]
pub struct Temp();

impl Temp {
    /// Creates a unique temporary.
    pub fn new() -> Self {
        todo!()
    }
}

pub struct TempTable {}

/// Represents some machine-language location whose exact address is yet to be determined.
#[derive(Debug, Clone, PartialEq)]
pub struct Label(Symbol);

impl Label {
    /// Creates a unique label.
    pub fn new() -> Self {
        todo!()
    }

    /// Creates a new label whose assembly-language name is the given string.
    pub fn named(name: &str) -> Self {
        todo!()
    }
}
