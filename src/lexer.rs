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

pub fn lex<T: BufRead>(buf: T) -> ToppleResult<TokenStream> {
    let mut res = Vec::new();
    let mut buf_iter = buf.lines().enumerate();
    let mut reading_literal = (false, 0, 0, String::new());
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
        if reading_literal.0 {
            lex_string(
                &mut iter,
                &'\'',
                &mut res,
                reading_literal.1,
                reading_literal.2,
                &mut reading_literal,
            )?;
        }
        while let Some((j, c)) = iter.next() {
            if c.is_whitespace() {
                continue;
            }
            match c {
                '(' => res.push((Token::LeftParen, i, j)),
                ')' => res.push((Token::RightParen, i, j)),
                '[' => res.push((Token::LeftBracket, i, j)),
                ']' => res.push((Token::RightBracket, i, j)),
                '{' => res.push((Token::LeftCurly, i, j)),
                '}' => res.push((Token::RightCurly, i, j)),
                ',' => res.push((Token::Comma, i, j)),
                '.' => res.push((Token::Dot, i, j)),
                ';' => res.push((Token::SemiColon, i, j)),
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
                        res.push((Token::Num(Num::Imm(0)), i, j));
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
                    None => res.push((Token::Operator(Operator::Sub), i, j)),
                },
                '=' | '+' | '*' | '/' | '&' | '|' | '!' | '^' | '<' | '>' => {
                    lex_op(&mut iter, &c, &mut res, i, j)?
                }
                '"' | '\'' => {
                    if let Some((_, p)) = iter.peek() {
                        if c == *p {
                            iter.next();
                            res.push((Token::Str(String::new()), i, j));
                            continue;
                        }
                    }
                    lex_string(&mut iter, &c, &mut res, i, j, &mut reading_literal)?;
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
    res: &mut TokenStream,
    line: usize,
    chr: usize,
    is_neg: bool,
) -> ToppleResult<()> {
    let mut num_str = String::new();
    num_str.push(*c);
    if iter.peek().is_none() {
        match num_str.parse::<u64>() {
            Ok(n) => {
                res.push((Token::Num(Num::Imm(n)), line, chr));
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
                res.push((Token::Num(Num::Imm(n as u64)), line, chr));
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
                res.push((Token::Num(Num::Imm(n)), line, chr));
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
    res: &mut TokenStream,
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
        res.push((Token::Num(Num::Bits(bin_str)), line, chr));
        Ok(())
    }
}

fn lex_string(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut TokenStream,
    line: usize,
    chr: usize,
    reading_literal: &mut (bool, usize, usize, String),
) -> ToppleResult<()> {
    let is_literal = *c == '\'' || reading_literal.0;
    let mut s = if reading_literal.0 {
        std::mem::take(&mut reading_literal.3)
    } else {
        String::new()
    };
    loop {
        let p = match iter.peek() {
            Some((_, peek)) => peek,
            None => {
                if is_literal {
                    s.push('\n');
                    if !reading_literal.0 {
                        *reading_literal = (true, line, chr, s)
                    }
                    return Ok(());
                } else {
                    return Err(LexerError::new(
                        "Strings (\" \") cannot be multiple lines, try a Raw String (' ')?",
                        line,
                        chr,
                    )
                    .wrap());
                }
            }
        };
        if *p == *c {
            res.push((Token::Str(s), line, chr));
            iter.next();
            reading_literal.0 = false;
            break;
        }
        if is_literal {
            if *p == '\\' {
                iter.next();
                if let Some((_, p2)) = iter.peek() {
                    if *p2 == '\'' {
                        s.push('\'');
                        iter.next();
                        continue;
                    }
                }
                s.push('\\');
                continue;
            }
            s.push(*p);
            iter.next();
        } else if *p == '\\' {
            iter.next();
            let p2 = match iter.peek() {
                Some((_, peek)) => peek,
                None => {
                    return Err(LexerError::new(
                        "Strings (\" \") cannot be multiple lines, tray a Raw String (' ')?",
                        line,
                        chr,
                    )
                    .wrap())
                }
            };
            match *p2 {
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                '\\' => s.push('\\'),
                '0' => s.push('\0'),
                '"' => s.push('"'),
                _ => continue,
            }
            iter.next();
        } else {
            s.push(*p);
            iter.next();
        }
    }
    Ok(())
}

fn lex_ident(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut TokenStream,
    line: usize,
    chr: usize,
) -> ToppleResult<()> {
    todo!()
}

fn lex_op(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut TokenStream,
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
        assert_eq!(out[0].0, Token::Num(Num::Imm(123)));
        assert_eq!(out[1].0, Token::Num(Num::Bits("10110111".into())));
        assert_eq!(out[2].0, Token::Num(Num::Imm(10110000)));
        assert_eq!(out[3].0, Token::Num(Num::Imm(173456)));
        assert_eq!(out[4].0, Token::Num(Num::Imm(789)));
        assert_eq!(out[5].0, Token::Num(Num::Imm(n)));
    }

    #[test]
    fn string_lexer() {
        let buf = r#"
        "first" 'second'
        'th
ird'
        ""''
        "#;
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 5);
        assert_eq!(out[0].0, Token::Str("first".into()));
        assert_eq!(out[1].0, Token::Str("second".into()));
        assert_eq!(out[2].0, Token::Str("th\nird".into()));
        assert_eq!(out[3].0, Token::Str(String::new()));
        assert_eq!(out[4].0, Token::Str(String::new()));
    }

    #[test]
    fn string_new_lines() {
        let buf = r#"
        "Escape\nNewline" 'Literal
Newline'
        "#;
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].0, Token::Str("Escape\nNewline".into()));
        assert_eq!(out[1].0, Token::Str("Literal\nNewline".into()));
    }

    #[test]
    fn string_escapes() {
        let buf = r#"
        "New\nline"
        "\r" "\t" "\\" "\0" "\a"
        '\n\r\t\0'
        "#;
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 7);
        assert_eq!(out[0].0, Token::Str("New\nline".into()));
        assert_eq!(out[1].0, Token::Str("\r".into()));
        assert_eq!(out[2].0, Token::Str("\t".into()));
        assert_eq!(out[3].0, Token::Str("\\".into()));
        assert_eq!(out[4].0, Token::Str("\0".into()));
        assert_eq!(out[5].0, Token::Str("a".into()));
        assert_eq!(out[6].0, Token::Str("\\n\\r\\t\\0".into()));
    }
}
