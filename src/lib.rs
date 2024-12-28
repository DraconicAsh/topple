mod error;
mod lexer;
mod parser;

pub enum ToppleType {
    ByteTable,
    BitTable,
    InstrTable,
    VariantTable(Vec<ToppleType>),
}
