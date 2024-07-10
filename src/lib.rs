pub use document::{Position, Span, Spanned};
pub use parser::ParseError;

pub mod ast;
mod checker;
mod document;
mod env;
mod escape;
mod ir;
mod lexer;
mod parser;
mod symbol;
mod temp;
mod types;
mod translate;
mod frame;
mod canonical;
mod canonical_tree;
mod basic_block;
mod trace;
mod codegen;
mod assembly;
mod flow;

pub fn parse<'src>(source: &'src str) -> Result<ast::Program, Vec<ParseError<'src>>> {
    let tokens = lexer::tokenize(source);
    parser::parse(tokens)
}

pub fn compile(_source: &str) -> Result<String, ()> {
    todo!()
}
