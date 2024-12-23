use crate::error::{LexerError, ToppleResult};
use std::io::{BufRead, Lines};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Symbol {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    Comma,
    Dot,
    SemiColon,
    Operator(Operator),
    Str(String),
    Num(Num),
    Keyword(Keyword),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Let,
    Print,
    Read,
}

#[derive(Debug, PartialEq)]
pub enum Num {
    Imm(u64),
    Bits(String),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Assign,     // =
    Plus,       // +
    Sub,        // -
    Mult,       // *
    Div,        // /
    BitAnd,     // &
    BitOr,      // |
    BitNot,     // !
    BitXor,     // ^
    ShiftLeft,  // <<
    ShiftRight, // >>
    Eq,         // ==
    InvEq,      // !=
    Greater,    // >
    Less,       // <
    GreaterEq,  // >=
    LessEq,     // <=
    Append,     // ++
}

pub fn lex<T: BufRead>(buf: T) -> ToppleResult<Vec<Symbol>> {
    let mut res = Vec::new();
    let mut buf_iter = buf.lines().enumerate();
    while let Some((i, line)) = buf_iter.next() {
        let line = match line {
            Ok(l) => l,
            Err(e) => {
                return Err(LexerError::new(
                    "Lower-Level error while attempting to read line",
                    i as u64,
                )
                .source(Box::new(e))
                .wrap())
            }
        };
        let mut iter = line.chars().peekable();
        while let Some(c) = iter.next() {
            if c.is_whitespace() {
                continue;
            }
            match c {
                '(' => res.push(Symbol::LeftParen),
                ')' => res.push(Symbol::RightParen),
                '[' => res.push(Symbol::LeftBracket),
                ']' => res.push(Symbol::RightBracket),
                '{' => res.push(Symbol::LeftCurly),
                '}' => res.push(Symbol::RightCurly),
                ',' => res.push(Symbol::Comma),
                '.' => res.push(Symbol::Dot),
                ';' => res.push(Symbol::SemiColon),
                '0' => match iter.peek() {
                    Some(p) => {
                        if *p == 'b' {
                            iter.next();
                            lex_bits(&mut iter, &mut res, i as u64)?;
                        } else {
                            lex_num(&mut iter, &c, &mut res, i as u64, false)?;
                        }
                    }
                    None => {
                        res.push(Symbol::Num(Num::Imm(0)));
                        return Ok(res);
                    }
                },
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    lex_num(&mut iter, &c, &mut res, i as u64, false)?
                }
                '-' => match iter.peek() {
                    Some(p) => {
                        if p.is_ascii_digit() {
                            lex_num(&mut iter, &c, &mut res, i as u64, true)?;
                        } else {
                            lex_op(&mut iter, &c, &mut res, i as u64)?;
                        }
                    }
                    None => res.push(Symbol::Operator(Operator::Sub)),
                },
                '=' | '+' | '*' | '/' | '&' | '|' | '!' | '^' | '<' | '>' => {
                    lex_op(&mut iter, &c, &mut res, i as u64)?
                }
                '"' | '\'' => {
                    if let Some(p) = iter.peek() {
                        if c == *p {
                            iter.next();
                            res.push(Symbol::Str(String::new()));
                            continue;
                        }
                    }
                    lex_string(&mut iter, &mut buf_iter, &c, &mut res, i as u64)?;
                }
                _ => lex_ident(&mut iter, &c, &mut res, i as u64)?,
            }
        }
    }
    Ok(res)
}

fn lex_num(
    iter: &mut Peekable<Chars>,
    c: &char,
    res: &mut Vec<Symbol>,
    line: u64,
    is_neg: bool,
) -> ToppleResult<()> {
    let mut num_str = String::new();
    num_str.push(*c);
    if iter.peek().is_none() {
        match num_str.parse::<u64>() {
            Ok(n) => {
                res.push(Symbol::Num(Num::Imm(n)));
                return Ok(());
            }
            Err(e) => {
                return Err(
                    LexerError::new("Lower-Level error when parsing number", line)
                        .source(Box::new(e))
                        .wrap(),
                )
            }
        }
    };
    while let Some(p) = iter.peek() {
        if p.is_ascii_digit() {
            num_str.push(*p);
            iter.next();
        } else {
            break;
        }
    }
    if is_neg {
        match num_str.parse::<i64>() {
            Ok(n) => {
                res.push(Symbol::Num(Num::Imm(n as u64)));
                Ok(())
            }
            Err(e) => Err(
                LexerError::new("Lower-Level error when parsing number", line)
                    .source(Box::new(e))
                    .wrap(),
            ),
        }
    } else {
        match num_str.parse::<u64>() {
            Ok(n) => {
                res.push(Symbol::Num(Num::Imm(n)));
                Ok(())
            }
            Err(e) => Err(
                LexerError::new("Lower-Level error when parsing number", line)
                    .source(Box::new(e))
                    .wrap(),
            ),
        }
    }
}

fn lex_bits(iter: &mut Peekable<Chars>, res: &mut Vec<Symbol>, line: u64) -> ToppleResult<()> {
    if iter.peek().is_none() {
        return Err(
            LexerError::new("Binary encoded numbers must have at least 1 digit", line).wrap(),
        );
    }
    let mut bin_str = String::new();
    while let Some(p) = iter.peek() {
        if *p == '0' || *p == '1' {
            bin_str.push(*p);
            iter.next();
        } else {
            break;
        }
    }
    if bin_str.is_empty() {
        Err(LexerError::new("Binary encoded numbers must have at least 1 digit", line).wrap())
    } else {
        res.push(Symbol::Num(Num::Bits(bin_str)));
        Ok(())
    }
}

fn lex_string<T: BufRead>(
    iter: &mut Peekable<Chars>,
    buf_iter: &mut std::iter::Enumerate<Lines<T>>,
    c: &char,
    res: &mut Vec<Symbol>,
    line: u64,
) -> ToppleResult<()> {
    todo!()
}

fn lex_ident(
    iter: &mut Peekable<Chars>,
    c: &char,
    res: &mut Vec<Symbol>,
    line: u64,
) -> ToppleResult<()> {
    todo!()
}

fn lex_op(
    iter: &mut Peekable<Chars>,
    c: &char,
    res: &mut Vec<Symbol>,
    line: u64,
) -> ToppleResult<()> {
    todo!()
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn num_lexer() {
        let buf = "123 0b10110111\n10110000 173456\n0789-394";
        let out = lex(buf.as_bytes()).unwrap();
        let n = (-394 as i64) as u64;
        assert_eq!(out.len(), 6);
        assert_eq!(out[0], Symbol::Num(Num::Imm(123)));
        assert_eq!(out[1], Symbol::Num(Num::Bits("10110111".into())));
        assert_eq!(out[2], Symbol::Num(Num::Imm(10110000)));
        assert_eq!(out[3], Symbol::Num(Num::Imm(173456)));
        assert_eq!(out[4], Symbol::Num(Num::Imm(789)));
        assert_eq!(out[5], Symbol::Num(Num::Imm(n)));
    }
}
