pub use document::{Position, Span, Spanned};
pub use parser::ParseError;

pub mod ast;
mod document;
mod lexer;
mod parser;

pub fn parse<'src>(source: &'src str) -> Result<ast::Program, Vec<ParseError<'src>>> {
    let tokens = lexer::tokenize(source);
    parser::parse(tokens)
}
