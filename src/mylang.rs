pub use document::{Position, Span, Spanned};
pub use parser::ParseError;

pub mod ast;
mod document;
mod lexer;
mod parser;
mod checker;
mod intermediate_repr;
mod types;
mod env;
mod symbol;
mod temp;
mod escape;

pub fn parse<'src>(source: &'src str) -> Result<ast::Program, Vec<ParseError<'src>>> {
    let tokens = lexer::tokenize(source);
    parser::parse(tokens)
}
