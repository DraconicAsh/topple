use crate::error::{LexerError, ToppleResult};
use std::io::{BufRead, Lines};
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

pub type TokenStream = Vec<(Token, usize, usize)>;

#[derive(Debug, PartialEq)]
pub enum Token {
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

pub fn lex<T: BufRead>(buf: T) -> ToppleResult<Vec<Token>> {
    let mut res = Vec::new();
    let mut buf_iter = buf.lines().enumerate();
    while let Some((i, line)) = buf_iter.next() {
        let line = match line {
            Ok(l) => l,
            Err(e) => {
                return Err(LexerError::new(
                    "Lower-Level error while attempting to read line",
                    i,
                    0,
                )
                .source(Box::new(e))
                .wrap())
            }
        };
        let mut iter = line.chars().enumerate().peekable();
        while let Some((j, c)) = iter.next() {
            if c.is_whitespace() {
                continue;
            }
            match c {
                '(' => res.push(Token::LeftParen),
                ')' => res.push(Token::RightParen),
                '[' => res.push(Token::LeftBracket),
                ']' => res.push(Token::RightBracket),
                '{' => res.push(Token::LeftCurly),
                '}' => res.push(Token::RightCurly),
                ',' => res.push(Token::Comma),
                '.' => res.push(Token::Dot),
                ';' => res.push(Token::SemiColon),
                '0' => match iter.peek() {
                    Some((_, p)) => {
                        if *p == 'b' {
                            iter.next();
                            lex_bits(&mut iter, &mut res, i, j)?;
                        } else {
                            lex_num(&mut iter, &c, &mut res, i, j, false)?;
                        }
                    }
                    None => {
                        res.push(Token::Num(Num::Imm(0)));
                        return Ok(res);
                    }
                },
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    lex_num(&mut iter, &c, &mut res, i, j, false)?
                }
                '-' => match iter.peek() {
                    Some((_, p)) => {
                        if p.is_ascii_digit() {
                            lex_num(&mut iter, &c, &mut res, i, j, true)?;
                        } else {
                            lex_op(&mut iter, &c, &mut res, i, j)?;
                        }
                    }
                    None => res.push(Token::Operator(Operator::Sub)),
                },
                '=' | '+' | '*' | '/' | '&' | '|' | '!' | '^' | '<' | '>' => {
                    lex_op(&mut iter, &c, &mut res, i, j)?
                }
                '"' | '\'' => {
                    if let Some((_, p)) = iter.peek() {
                        if c == *p {
                            iter.next();
                            res.push(Token::Str(String::new()));
                            continue;
                        }
                    }
                    lex_string(&mut iter, &mut buf_iter, &c, &mut res, i, j)?;
                }
                _ => lex_ident(&mut iter, &c, &mut res, i, j)?,
            }
        }
    }
    Ok(res)
}

fn lex_num(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut Vec<Token>,
    line: usize,
    chr: usize,
    is_neg: bool,
) -> ToppleResult<()> {
    let mut num_str = String::new();
    num_str.push(*c);
    if iter.peek().is_none() {
        match num_str.parse::<u64>() {
            Ok(n) => {
                res.push(Token::Num(Num::Imm(n)));
                return Ok(());
            }
            Err(e) => {
                return Err(
                    LexerError::new("Lower-Level error when parsing number", line, chr)
                        .source(Box::new(e))
                        .wrap(),
                )
            }
        }
    };
    while let Some((_, p)) = iter.peek() {
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
                res.push(Token::Num(Num::Imm(n as u64)));
                Ok(())
            }
            Err(e) => Err(
                LexerError::new("Lower-Level error when parsing number", line, chr)
                    .source(Box::new(e))
                    .wrap(),
            ),
        }
    } else {
        match num_str.parse::<u64>() {
            Ok(n) => {
                res.push(Token::Num(Num::Imm(n)));
                Ok(())
            }
            Err(e) => Err(
                LexerError::new("Lower-Level error when parsing number", line, chr)
                    .source(Box::new(e))
                    .wrap(),
            ),
        }
    }
}

fn lex_bits(
    iter: &mut Peekable<Enumerate<Chars>>,
    res: &mut Vec<Token>,
    line: usize,
    chr: usize,
) -> ToppleResult<()> {
    if iter.peek().is_none() {
        return Err(LexerError::new(
            "Binary encoded numbers must have at least 1 digit",
            line,
            chr,
        )
        .wrap());
    }
    let mut bin_str = String::new();
    while let Some((_, p)) = iter.peek() {
        if *p == '0' || *p == '1' {
            bin_str.push(*p);
            iter.next();
        } else {
            break;
        }
    }
    if bin_str.is_empty() {
        Err(LexerError::new(
            "Binary encoded numbers must have at least 1 digit",
            line,
            chr,
        )
        .wrap())
    } else {
        res.push(Token::Num(Num::Bits(bin_str)));
        Ok(())
    }
}

fn lex_string<T: BufRead>(
    iter: &mut Peekable<Enumerate<Chars>>,
    buf_iter: &mut std::iter::Enumerate<Lines<T>>,
    c: &char,
    res: &mut Vec<Token>,
    line: usize,
    chr: usize,
) -> ToppleResult<()> {
    todo!()
}

fn lex_ident(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut Vec<Token>,
    line: usize,
    chr: usize,
) -> ToppleResult<()> {
    todo!()
}

fn lex_op(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut Vec<Token>,
    line: usize,
    chr: usize,
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
        assert_eq!(out[0], Token::Num(Num::Imm(123)));
        assert_eq!(out[1], Token::Num(Num::Bits("10110111".into())));
        assert_eq!(out[2], Token::Num(Num::Imm(10110000)));
        assert_eq!(out[3], Token::Num(Num::Imm(173456)));
        assert_eq!(out[4], Token::Num(Num::Imm(789)));
        assert_eq!(out[5], Token::Num(Num::Imm(n)));
    }
}
