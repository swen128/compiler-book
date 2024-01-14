use std::{fmt::Debug, ops::Range};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start: Position::new(start as usize), end: Position::new(end as usize) }
    }
    
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.clone(),
            end: other.end.clone(),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?} @ {}..{}", self.value, self.span.start, self.span.end))
    }
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned {
            value,
            span,
        }
    }
}

impl chumsky::Span for Span {
    type Context = ();
    type Offset = usize;

    fn new(_: (), range: Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }

    fn start(&self) -> usize {
        self.start.offset
    }

    fn end(&self) -> usize {
        self.end.offset
    }

    fn context(&self) {
        ()
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
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
