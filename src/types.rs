use std::convert::From;

#[derive(Debug, PartialEq, Clone)]
pub enum ToppleType {
    ByteTable(ByteTable),
    InstrTable,
    VariantTable(Vec<ToppleType>),
}

impl std::fmt::Display for ToppleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ByteTable(t) => write!(f, "{t}"),
            Self::InstrTable => write!(f, ""),
            Self::VariantTable(t) => write!(f, "{t:?}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

    pub fn from_str(s: &str) -> Self {
        Self {
            table: s.as_bytes().to_vec(),
        }
    }

    pub fn from_bit_str(s: &str) -> Self {
        let mut table = Vec::new();
        for (i, b) in s.chars().rev().filter(|&x| x != '_').enumerate() {
            let byte_idx = i / 8;
            let bit_idx = i % 8;
            if bit_idx == 0 {
                match b {
                    '1' => table.push(1),
                    '0' => table.push(0),
                    _ => unreachable!(),
                }
            } else if b == '1' {
                table[byte_idx] |= 1 << bit_idx;
            }
        }
        Self { table }
    }

    pub fn len(&self) -> usize {
        self.table.len()
    }

    pub fn try_from_type_vec(t: Vec<ToppleType>) -> Option<Self> {
        if t.is_empty() {
            return None;
        }
        let mut table = Vec::new();
        for e in t.iter() {
            if let ToppleType::ByteTable(b) = e {
                if b.table.len() == 1 {
                    table.push(b.table[0]);
                    continue;
                }
            }
            return None;
        }
        let ret = Self { table };
        Some(ret)
    }
}

impl From<u64> for ByteTable {
    fn from(value: u64) -> Self {
        Self {
            table: value.to_le_bytes().to_vec(),
        }
    }
}

impl From<u128> for ByteTable {
    fn from(value: u128) -> Self {
        Self {
            table: value.to_le_bytes().to_vec(),
        }
    }
}

impl std::fmt::Display for ByteTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let len = self.table.len();
        if len == 0 {
            return write!(f, "[]");
        }
        let mut s = String::with_capacity(len * 10);
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

#[cfg(test)]
mod type_tests {
    use super::*;

    #[test]
    fn binary_str() {
        let single = ByteTable::from_bit_str("00100011");
        let single_test = ByteTable {
            table: vec![0b0010_0011],
        };
        assert_eq!(single_test, single);

        let nine = ByteTable::from_bit_str("100101100");
        let nine_test = ByteTable {
            table: vec![0b0010_1100, 0b0000_0001],
        };
        assert_eq!(nine_test, nine);
    }

    #[test]
    fn binary_num() {
        let single = ByteTable::from_bit_str("00010010");
        assert_eq!(Some(0b0001_0010), single.as_unsigned());

        let double = ByteTable::from_bit_str("0001_0010_1100_1010");
        assert_eq!(Some(0b0001_0010_1100_1010), double.as_unsigned());
    }

    #[test]
    fn print_bytes() {
        let single = ByteTable {
            table: vec![0b0010_0000],
        };
        assert_eq!("[00100000]", format!("{single}"));

        let short = ByteTable { table: vec![1] };
        assert_eq!("[00000001]", format!("{short}"));

        let num = ByteTable::from(64_u64);
        let s = format!(
            "[01000000, {0:08b}, {0:08b}, {0:08b}, {0:08b}, {0:08b}, {0:08b}, {0:08b}]",
            0
        );
        assert_eq!(s, format!("{num}"));
    }
}
