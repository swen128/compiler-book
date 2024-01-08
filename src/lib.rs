mod mylang;

pub use mylang::parse;
pub use mylang::document::Position;

pub fn compile(source: &str) -> Result<String, ()> {
    todo!()
}
