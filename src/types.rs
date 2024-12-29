pub enum ToppleType {
    ByteTable,
    BitTable,
    InstrTable,
    VariantTable(Vec<ToppleType>),
}
