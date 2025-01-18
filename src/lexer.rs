use crate::error::{ToppleError, ToppleResult};
use std::io::{BufRead, Lines};
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

pub type TokenStream = Vec<(Token, usize, usize)>;
pub type TokenStreamSlice<'a> = &'a [(Token, usize, usize)];

#[derive(Debug, PartialEq, Clone)]
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
    Op(Op),
    Str(String),
    Num(Num),
    Keyword(Keyword),
    Ident(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    For,
    Args,
    SelfK,
    Read,
    Print,
    PrintNum,
    PrintSigned,
    Import,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Keyword::Let => "let",
            Keyword::For => "for",
            Keyword::Args => "args",
            Keyword::SelfK => "self",
            Keyword::Read => "read",
            Keyword::Print => "print",
            Keyword::PrintNum => "print_num",
            Keyword::PrintSigned => "print_signed",
            Keyword::Import => "import",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Num {
    Imm(u64),
    Bits(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    Assign,     // =
    Add,        // +
    Sub,        // -
    Mult,       // *
    Div,        // /
    Mod,        // %
    BitAnd,     // &
    BitOr,      // |
    BitNot,     // !
    BitXor,     // ^
    ShiftLeft,  // <<
    ShiftRight, // >>
    Eq,         // ==
    NotEq,      // !=
    Greater,    // >
    Less,       // <
    GreaterEq,  // >=
    LessEq,     // <=
    Push,       // ++
    Pop,        // --
}

pub fn lex<T: BufRead>(buf: T) -> ToppleResult<TokenStream> {
    let mut res = Vec::new();
    let mut buf_iter = buf.lines().enumerate();
    let mut reading_literal = (false, 0, 0, String::new());
    while let Some((i, line)) = buf_iter.next() {
        let line = match line {
            Ok(l) => l,
            Err(e) => return Err(ToppleError::LineReadError(i, Box::new(e))),
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
                    None => res.push((Token::Op(Op::Sub), i, j)),
                },
                '=' | '+' | '*' | '/' | '&' | '|' | '!' | '^' | '<' | '>' | '%' => {
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
            Err(e) => return Err(ToppleError::NumberParseError(line, chr, Box::new(e))),
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
            Err(e) => Err(ToppleError::NumberParseError(line, chr, Box::new(e))),
        }
    } else {
        match num_str.parse::<u64>() {
            Ok(n) => {
                res.push((Token::Num(Num::Imm(n)), line, chr));
                Ok(())
            }
            Err(e) => Err(ToppleError::NumberParseError(line, chr, Box::new(e))),
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
        return Err(ToppleError::EmptyBinaryNumError(line, chr));
    }
    let mut bin_str = String::new();
    while let Some((_, p)) = iter.peek() {
        if *p == '0' || *p == '1' {
            bin_str.push(*p);
            iter.next();
        } else if *p == '_' {
            iter.next();
            continue;
        } else {
            break;
        }
    }
    if bin_str.is_empty() {
        Err(ToppleError::EmptyBinaryNumError(line, chr))
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
                    return Err(ToppleError::OpenStringError(line, chr));
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
                None => return Err(ToppleError::OpenStringError(line, chr)),
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
    if !(c.is_ascii_alphabetic() || *c == '_') {
        return Err(ToppleError::InvalidChar(line, chr, *c));
    }
    let mut s = String::new();
    s.push(*c);
    while let Some((_, p)) = iter.peek() {
        if p.is_ascii_alphanumeric() || *p == '_' {
            s.push(*p);
            iter.next();
        } else {
            break;
        }
    }
    match s.as_str() {
        "let" => res.push((Token::Keyword(Keyword::Let), line, chr)),
        "for" => res.push((Token::Keyword(Keyword::For), line, chr)),
        "args" => res.push((Token::Keyword(Keyword::Args), line, chr)),
        "self" => res.push((Token::Keyword(Keyword::SelfK), line, chr)),
        "print" => res.push((Token::Keyword(Keyword::Print), line, chr)),
        "print_num" => res.push((Token::Keyword(Keyword::PrintNum), line, chr)),
        "print_signed" => res.push((Token::Keyword(Keyword::PrintSigned), line, chr)),
        "read" => res.push((Token::Keyword(Keyword::Read), line, chr)),
        "import" => res.push((Token::Keyword(Keyword::Import), line, chr)),
        _ => res.push((Token::Ident(s), line, chr)),
    }
    Ok(())
}

fn lex_op(
    iter: &mut Peekable<Enumerate<Chars>>,
    c: &char,
    res: &mut TokenStream,
    line: usize,
    chr: usize,
) -> ToppleResult<()> {
    match c {
        '=' => {
            if let Some((_, p)) = iter.peek() {
                if *p == '=' {
                    iter.next();
                    res.push((Token::Op(Op::Eq), line, chr));
                    return Ok(());
                }
            }
            res.push((Token::Op(Op::Assign), line, chr));
            Ok(())
        }
        '+' => {
            if let Some((_, p)) = iter.peek() {
                if *p == '+' {
                    iter.next();
                    res.push((Token::Op(Op::Push), line, chr));
                    return Ok(());
                }
            }
            res.push((Token::Op(Op::Add), line, chr));
            Ok(())
        }
        '-' => {
            if let Some((_, p)) = iter.peek() {
                if *p == '-' {
                    iter.next();
                    res.push((Token::Op(Op::Pop), line, chr));
                    return Ok(());
                }
            }
            res.push((Token::Op(Op::Sub), line, chr));
            Ok(())
        }
        '*' => {
            res.push((Token::Op(Op::Mult), line, chr));
            Ok(())
        }
        '/' => {
            res.push((Token::Op(Op::Div), line, chr));
            Ok(())
        }
        '&' => {
            res.push((Token::Op(Op::BitAnd), line, chr));
            Ok(())
        }
        '|' => {
            res.push((Token::Op(Op::BitOr), line, chr));
            Ok(())
        }
        '^' => {
            res.push((Token::Op(Op::BitXor), line, chr));
            Ok(())
        }
        '%' => {
            res.push((Token::Op(Op::Mod), line, chr));
            Ok(())
        }
        '!' => {
            if let Some((_, p)) = iter.peek() {
                if *p == '=' {
                    iter.next();
                    res.push((Token::Op(Op::NotEq), line, chr));
                    return Ok(());
                }
            }
            res.push((Token::Op(Op::BitNot), line, chr));
            Ok(())
        }
        '>' => {
            if let Some((_, p)) = iter.peek() {
                if *p == '=' {
                    iter.next();
                    res.push((Token::Op(Op::GreaterEq), line, chr));
                    return Ok(());
                } else if *p == '>' {
                    iter.next();
                    res.push((Token::Op(Op::ShiftRight), line, chr));
                    return Ok(());
                }
            }
            res.push((Token::Op(Op::Greater), line, chr));
            Ok(())
        }
        '<' => {
            if let Some((_, p)) = iter.peek() {
                if *p == '=' {
                    iter.next();
                    res.push((Token::Op(Op::LessEq), line, chr));
                    return Ok(());
                } else if *p == '<' {
                    iter.next();
                    res.push((Token::Op(Op::ShiftLeft), line, chr));
                    return Ok(());
                }
            }
            res.push((Token::Op(Op::Less), line, chr));
            Ok(())
        }
        _ => Err(ToppleError::InvalidOp(line, chr, *c)),
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn num_lexing() {
        let buf = "123 0b10110111\n10110000 173456\n0789-394 0b0010_1000_23";
        let out = lex(buf.as_bytes()).unwrap();
        let n = (-394 as i64) as u64;
        assert_eq!(out.len(), 8);
        assert_eq!(out[0].0, Token::Num(Num::Imm(123)));
        assert_eq!(out[1].0, Token::Num(Num::Bits("10110111".into())));
        assert_eq!(out[2].0, Token::Num(Num::Imm(10110000)));
        assert_eq!(out[3].0, Token::Num(Num::Imm(173456)));
        assert_eq!(out[4].0, Token::Num(Num::Imm(789)));
        assert_eq!(out[5].0, Token::Num(Num::Imm(n)));
        assert_eq!(out[6].0, Token::Num(Num::Bits("00101000".into())));
        assert_eq!(out[7].0, Token::Num(Num::Imm(23)));
    }

    #[test]
    fn string_lexing() {
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

    #[test]
    fn ops_single_char() {
        let buf = "=+-*/&|!^><";
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 11);
        assert_eq!(out[0].0, Token::Op(Op::Assign));
        assert_eq!(out[1].0, Token::Op(Op::Add));
        assert_eq!(out[2].0, Token::Op(Op::Sub));
        assert_eq!(out[3].0, Token::Op(Op::Mult));
        assert_eq!(out[4].0, Token::Op(Op::Div));
        assert_eq!(out[5].0, Token::Op(Op::BitAnd));
        assert_eq!(out[6].0, Token::Op(Op::BitOr));
        assert_eq!(out[7].0, Token::Op(Op::BitNot));
        assert_eq!(out[8].0, Token::Op(Op::BitXor));
        assert_eq!(out[9].0, Token::Op(Op::Greater));
        assert_eq!(out[10].0, Token::Op(Op::Less));
    }

    #[test]
    fn ops_multi_char() {
        let buf = "<<>>==!=>=<=++--";
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 8);
        assert_eq!(out[0].0, Token::Op(Op::ShiftLeft));
        assert_eq!(out[1].0, Token::Op(Op::ShiftRight));
        assert_eq!(out[2].0, Token::Op(Op::Eq));
        assert_eq!(out[3].0, Token::Op(Op::NotEq));
        assert_eq!(out[4].0, Token::Op(Op::GreaterEq));
        assert_eq!(out[5].0, Token::Op(Op::LessEq));
        assert_eq!(out[6].0, Token::Op(Op::Push));
        assert_eq!(out[7].0, Token::Op(Op::Pop));
    }

    #[test]
    fn identifier_lexing() {
        let buf = "var _var0 var_1 2var _3var";
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 6);
        assert_eq!(out[0].0, Token::Ident("var".into()));
        assert_eq!(out[1].0, Token::Ident("_var0".into()));
        assert_eq!(out[2].0, Token::Ident("var_1".into()));
        assert_eq!(out[3].0, Token::Num(Num::Imm(2)));
        assert_eq!(out[4].0, Token::Ident("var".into()));
        assert_eq!(out[5].0, Token::Ident("_3var".into()));
    }

    #[test]
    fn keyword_lexing() {
        let buf = "let print print_num print_signed read import\n_let readprint\nfor args self";
        let out = lex(buf.as_bytes()).unwrap();
        assert_eq!(out.len(), 11);
        assert_eq!(out[0].0, Token::Keyword(Keyword::Let));
        assert_eq!(out[1].0, Token::Keyword(Keyword::Print));
        assert_eq!(out[2].0, Token::Keyword(Keyword::PrintNum));
        assert_eq!(out[3].0, Token::Keyword(Keyword::PrintSigned));
        assert_eq!(out[4].0, Token::Keyword(Keyword::Read));
        assert_eq!(out[5].0, Token::Keyword(Keyword::Import));
        assert_eq!(out[6].0, Token::Ident("_let".into()));
        assert_eq!(out[7].0, Token::Ident("readprint".into()));
        assert_eq!(out[8].0, Token::Keyword(Keyword::For));
        assert_eq!(out[9].0, Token::Keyword(Keyword::Args));
        assert_eq!(out[10].0, Token::Keyword(Keyword::SelfK));
    }
}
