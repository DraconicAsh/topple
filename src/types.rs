use std::convert::From;

#[derive(Debug, PartialEq)]
pub enum ToppleType {
    ByteTable(ByteTable),
    BitTable(BitTable),
    InstrTable,
    VariantTable(Vec<ToppleType>),
}

impl std::fmt::Display for ToppleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ByteTable(t) => write!(f, "{t}"),
            Self::BitTable(t) => write!(f, "{t}"),
            Self::InstrTable => write!(f, ""),
            Self::VariantTable(t) => write!(f, "{t:?}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ByteTable {
    table: Vec<u8>,
}

impl ByteTable {
    pub fn as_unsigned(&self) -> Option<u64> {
        let len = self.table.len();
        if len > 8 {
            None
        } else {
            let mut arr = [0; 8];
            for i in 0..len {
                arr[i] = self.table[i];
            }
            Some(u64::from_le_bytes(arr))
        }
    }

    pub fn as_signed(&self) -> Option<i64> {
        let len = self.table.len();
        if len > 8 {
            None
        } else {
            let mut arr = [0; 8];
            for i in 0..len {
                arr[i] = self.table[i];
            }
            Some(i64::from_le_bytes(arr))
        }
    }
}

impl From<u64> for ByteTable {
    fn from(value: u64) -> Self {
        Self {
            table: value.to_le_bytes().to_vec(),
        }
    }
}

impl From<BitTable> for ByteTable {
    fn from(value: BitTable) -> Self {
        Self { table: value.table }
    }
}

impl std::fmt::Display for ByteTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let len = self.table.len();
        if len == 0 {
            return write!(f, "[]");
        }
        let mut s = String::with_capacity(len * 3);
        s.push('[');
        let mut i = 0;
        s += &format!("{:08b}", self.table[i]);
        i += 1;
        while i < len {
            s.push_str(", ");
            s += &format!("{:08b}", self.table[i]);
            i += 1;
        }
        s.push(']');
        write!(f, "{s}")
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

    pub fn get(&self, idx: usize) -> bool {
        let byte_idx = idx / 8;
        let bit_idx = idx % 8;
        let bit = self.table[byte_idx] & (0b1000_0000 >> bit_idx);
        bit != 0
    }
}

impl std::cmp::PartialEq for BitTable {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            false
        } else {
            // When capacity is shrunk, bits are zeroed
            self.table == other.table
        }
    }
}

impl From<ByteTable> for BitTable {
    fn from(value: ByteTable) -> Self {
        let table = value.table;
        let len = table.len() * 8;
        Self { table, len }
    }
}

impl std::fmt::Display for BitTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.len == 0 {
            return write!(f, "[]");
        }
        let mut s = String::with_capacity(self.len * 3);
        s.push('[');
        let mut i = 0;
        if self.get(i) {
            s.push('1');
        } else {
            s.push('0');
        }
        i += 1;
        while i < self.len {
            s.push_str(", ");
            if self.get(i) {
                s.push('1');
            } else {
                s.push('0');
            }
            i += 1;
        }
        s.push(']');
        write!(f, "{s}")
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

    #[test]
    fn print_bytes() {
        let single = ByteTable {
            table: vec![0b0010_0000],
        };
        assert_eq!("[00100000]", format!("{single}"));

        let short = ByteTable { table: vec![1] };
        assert_eq!("[00000001]", format!("{short}"));

        let num = ByteTable::from(64);
        let s = format!(
            "[01000000, {0:08b}, {0:08b}, {0:08b}, {0:08b}, {0:08b}, {0:08b}, {0:08b}]",
            0
        );
        assert_eq!(s, format!("{num}"));
    }

    #[test]
    fn print_bits() {
        let byte = BitTable {
            table: vec![0b0110_0110],
            len: 8,
        };
        assert_eq!("[0, 1, 1, 0, 0, 1, 1, 0]", format!("{byte}"));

        let two_bytes = BitTable {
            table: vec![0b0000_1000, 0b1000_1000],
            len: 16,
        };
        assert_eq!(
            "[0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]",
            format!("{two_bytes}")
        );

        let bit = BitTable {
            table: vec![0b1000_0000],
            len: 1,
        };
        assert_eq!("[1]", format!("{bit}"));
    }
}
