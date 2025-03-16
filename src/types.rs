use crate::parser::block_print;
use crate::parser::AST;
use std::convert::From;
use std::ops::{Add, Div, Mul, Sub};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

#[derive(Debug, PartialEq, Clone)]
pub enum ToppleType {
    ByteTable(ByteTable),
    InstrTable(AST),
    VariantTable(Vec<ToppleType>),
}

impl std::fmt::Display for ToppleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ByteTable(t) => write!(f, "{t}"),
            Self::InstrTable(t) => write!(f, "{}", block_print(t, 0)),
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

    pub fn trim(&mut self) {
        let mut i = self.table.len();
        while i != 0 {
            i -= 1;
            if self.table[i] == 0 {
                self.table.pop();
            } else {
                break;
            }
        }
    }
}

impl Add for ByteTable {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Sub for ByteTable {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Mul for ByteTable {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Div for ByteTable {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Not for ByteTable {
    type Output = Self;

    fn not(self) -> Self::Output {
        todo!()
    }
}

impl BitAnd for ByteTable {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl BitOr for ByteTable {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl BitXor for ByteTable {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Shl for ByteTable {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Shl<u64> for ByteTable {
    type Output = Self;

    fn shl(self, rhs: u64) -> Self::Output {
        todo!()
    }
}

impl Shr for ByteTable {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Shr<u64> for ByteTable {
    type Output = Self;

    fn shr(self, rhs: u64) -> Self::Output {
        todo!()
    }
}

impl From<u64> for ByteTable {
    fn from(value: u64) -> Self {
        let mut t = Self {
            table: value.to_le_bytes().to_vec(),
        };
        t.trim();
        t
    }
}

impl From<u128> for ByteTable {
    fn from(value: u128) -> Self {
        let mut t = Self {
            table: value.to_le_bytes().to_vec(),
        };
        t.trim();
        t
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
        assert_eq!("[01000000]", format!("{num}"));
    }

    #[test]
    fn short_int_table() {
        let byte: ByteTable = 27_u64.into();
        let table = ByteTable { table: vec![27] };
        assert_eq!(table, byte);
        assert_eq!(1, byte.len());

        let bytes: ByteTable = 0b00010000_00000000_00000000_00000000_u64.into();
        let table = ByteTable {
            table: vec![0, 0, 0, 0b00010000],
        };
        assert_eq!(table, bytes);
        assert_eq!(4, table.len());
    }
}

#[cfg(test)]
mod math_ops_tests {
    use super::*;

    #[test]
    fn addition() {
        let a: ByteTable = num(27);
        let b: ByteTable = num(3);
        let ans = ByteTable { table: vec![30] };
        assert_eq!(ans, a + b);
    }

    #[test]
    fn addition_extend() {
        let a: ByteTable = num(0b10000000);
        let ans = ByteTable {
            table: vec![0, 0b00000001],
        };
        assert_eq!(ans, a.clone() + a);
    }

    #[test]
    fn subtraction() {
        let a: ByteTable = num(27);
        let b: ByteTable = num(7);
        let ans = ByteTable { table: vec![20] };
        assert_eq!(ans, a - b);
    }

    #[test]
    fn subtraction_16_bit() {
        let a: ByteTable = num(0b00000001_00000000);
        let b: ByteTable = num(0b10000000);
        let ans = ByteTable {
            table: vec![0, 0b10000000],
        };
        assert_eq!(ans, a - b);
    }

    #[test]
    fn multiplication() {
        let a: ByteTable = num(20);
        let b: ByteTable = num(3);
        let ans = ByteTable { table: vec![60] };
        assert_eq!(ans, a * b);
    }

    #[test]
    fn multiplication_extend() {
        let a: ByteTable = num(20);
        let b: ByteTable = num(30);
        let ans = ByteTable {
            table: vec![0b0101_1000, 0b0000_0010],
        };
        assert_eq!(ans, a * b);
    }

    #[test]
    fn division() {
        let a: ByteTable = num(18);
        let b: ByteTable = num(3);
        let ans = ByteTable { table: vec![6] };
        assert_eq!(ans, a / b);
    }

    #[test]
    fn division_cutoff() {
        let a: ByteTable = num(20);
        let b: ByteTable = num(3);
        let ans = ByteTable { table: vec![6] };
        assert_eq!(ans, a / b);
    }

    fn num(n: u64) -> ByteTable {
        n.into()
    }
}

#[cfg(test)]
mod bitwise_ops_tests {
    use super::*;

    #[test]
    fn bit_not() {
        let a = num(1);
        let ans = ByteTable {
            table: vec![0b11111110],
        };
        assert_eq!(ans, !a);
    }

    #[test]
    fn bit_and() {
        let a = num(27);
        let b = num(10);
        let ans = ByteTable { table: vec![10] };
        assert_eq!(ans, a & b);
    }

    #[test]
    fn bit_or() {
        let a = num(256);
        let b = num(1);
        let ans = ByteTable {
            table: vec![0b0000_0001, 0b0000_0001],
        };
        assert_eq!(ans, a | b);
    }

    #[test]
    fn bit_xor() {
        let a = num(27);
        let b = num(10);
        let ans = ByteTable { table: vec![17] };
        assert_eq!(ans, a ^ b);
    }

    #[test]
    fn shift_left() {
        let a = num(1);
        let b = num(4);
        let ans = ByteTable { table: vec![16] };
        assert_eq!(ans, a << b);
    }

    #[test]
    fn shift_left_extend() {
        let a = num(128);
        let b = num(1);
        let ans = ByteTable {
            table: vec![0, 0b0000_0001],
        };
        assert_eq!(ans, a << b);
    }

    #[test]
    fn shift_right() {
        let a = num(0b00010001_00000000);
        let b = num(5);
        let ans = ByteTable {
            table: vec![0, 0b10001000],
        };
        assert_eq!(ans, a >> b);
    }

    #[test]
    fn shift_right_cutoff() {
        let a = num(11);
        let b = num(1);
        let ans = ByteTable { table: vec![5] };
        assert_eq!(ans, a >> b);
    }

    fn num(n: u64) -> ByteTable {
        n.into()
    }
}
