use std::convert::From;

#[derive(Debug, PartialEq)]
pub enum ToppleType {
    ByteTable(ByteTable),
    BitTable(BitTable),
    InstrTable,
    VariantTable(Vec<ToppleType>),
}

#[derive(Debug, PartialEq)]
pub struct ByteTable {
    table: Vec<u8>,
}

impl From<u64> for ByteTable {
    fn from(value: u64) -> Self {
        Self {
            table: value.to_le_bytes().to_vec(),
        }
    }
}

#[derive(Debug)]
pub struct BitTable {
    table: Vec<u8>,
    len: usize,
}

impl BitTable {
    pub fn from_str(s: &str) -> Self {
        let len = s.len();
        let mut table = Vec::with_capacity((len / 8) + 1);
        for (i, c) in s.chars().enumerate() {
            let byte_idx = i / 8;
            let bit_idx = i % 8;
            if bit_idx == 0 {
                table.push(0);
            }
            if c == '0' {
                continue;
            }
            table[byte_idx] |= 0b1000_0000 >> bit_idx;
        }
        Self { table, len }
    }
}

impl std::cmp::PartialEq for BitTable {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            false
        } else {
            // When capacitry is shrunk, bits are zeroed
            self.table == other.table
        }
    }
}

#[cfg(test)]
mod type_tests {
    use super::*;

    #[test]
    fn bit_from_str() {
        let one_byte = BitTable::from_str("00000110");
        let a = BitTable {
            table: vec![0b0000_0110],
            len: 8,
        };
        assert_eq!(a, one_byte);
        let two_byte = BitTable::from_str("000000001");
        let b = BitTable {
            table: vec![0, 128],
            len: 9,
        };
        assert_eq!(b, two_byte);
    }
}
