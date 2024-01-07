#[derive(Debug, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start: Position::new(start as usize), end: Position::new(end as usize) }
    }
}

impl From<logos::Span> for Span {
    fn from(span: logos::Span) -> Span {
        Span::new(span.start, span.end)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub offset: usize,
}

impl Position {
    pub fn zero() -> Position {
        Position { offset: 0 }
    }

    pub fn new(offset: usize) -> Position {
        Position { offset }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.offset)
    }
}
