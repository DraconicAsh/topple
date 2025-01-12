use std::error::Error;
use std::fmt::Display;
use crate::lexer::{Token, Keyword};

pub type ToppleResult<T> = Result<T, ToppleError>;

#[derive(Debug)]
pub enum ToppleError {
    OpenStringError(usize, usize),
    LineReadError(usize, Box<dyn Error>),
    NumberParseError(usize, usize, Box<dyn Error>),
    EmptyBinaryNumError(usize, usize),
    InvalidOp(usize, usize, char),
    InvalidChar(usize, usize, char),
    OpenExprError(usize, usize),
    OpenBlockError(usize, usize),
    OpenParenError(usize, usize),
    OpenBracketError(usize, usize),
    HangingLetError(usize, usize),
    UnexpectedToken(Token, usize, usize),
    EmptyIndex(usize, usize),
    KeywordIsCall(Keyword, usize, usize),
    ExprPartialParse(usize, usize, usize, usize),
}

impl Display for ToppleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpenStringError(line, chr) => write!(
                f, 
                "{line}:{chr} Strings must be ended with a double quote (\") and cannot be on multiple lines. Use single quotes (\' \') for Raw Strings"
            ),
            Self::LineReadError(line, e) => write!(
                f,
                "{line}:_ Lower-Level error while attempting to read line; {e}"
            ),
            Self::NumberParseError(line, chr, e) => {
                write!(f, "{line}:{chr} Lower-Level error when parsing number; {e}")
            }
            Self::EmptyBinaryNumError(line, chr) => write!(
                f,
                "{line}:{chr} Binary encoded numbers must have at least 1 digit"
            ),
            Self::InvalidOp(line, chr, c) => write!(
                f,
                "{line}:{chr} !!INTERNAL BUG!! Lexer tried to parse invalid operator '{c}'"
            ),
            Self::InvalidChar(line, chr, c) => write!(
                f,
                "{line}:{chr} Invalid character '{c}' cannot be used in an identifier and is not a valid operator"
            ),
            Self::OpenExprError(line, chr) => write!(
                f,
                "{line}:{chr} All expressions must be ended with a semicolon ';'"
            ),
            Self::OpenBlockError(line, chr) => write!(f, "{line}:{chr} All code block starts '{{' must be ended '}}'"),
            Self::OpenParenError(line, chr) => write!(f, "{line}:{chr} All open parentheses '(' must be closed ')'"),
            Self::OpenBracketError(line, chr) => write!(f, "{line}:{chr} All open brackets '[' must be closed ']'"),
            Self::HangingLetError(line, chr) => write!(
            f,
            "{line}:{chr} 'let' must be followed by a variable name or a name and an assignment"
            ),
            Self::UnexpectedToken(t, line, chr) => write!(f, "{line}:{chr} Unexpected token {t:?}"),
            Self::EmptyIndex(line, chr) => write!(f, "{line}:{chr} Index operation requires a value"),
            Self::KeywordIsCall(k, line, chr) => write!(f, "{line}:{chr} \"{k}\" is a function and must be called as such"),
            Self::ExprPartialParse(line, chr, end_l, end_c) => write!(f, "{line}:{chr} Full expression could not be parsed; Stopped at {end_l}:{end_c}"),
        }
    }
}

impl std::cmp::PartialEq for ToppleError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ToppleError::OpenStringError(..), ToppleError::OpenStringError(..)) |
            (ToppleError::LineReadError(..), ToppleError::LineReadError(..)) |
            (ToppleError::NumberParseError(..), ToppleError::NumberParseError(..)) |
            (ToppleError::EmptyBinaryNumError(..), ToppleError::EmptyBinaryNumError(..)) |
            (ToppleError::InvalidOp(..), ToppleError::InvalidOp(..)) |
            (ToppleError::InvalidChar(..), ToppleError::InvalidChar(..)) |
            (ToppleError::OpenExprError(..), ToppleError::OpenExprError(..)) |
            (ToppleError::OpenBlockError(..), ToppleError::OpenBlockError(..)) |
            (ToppleError::OpenParenError(..), ToppleError::OpenParenError(..)) |
            (ToppleError::OpenBracketError(..), ToppleError::OpenBracketError(..)) |
            (ToppleError::HangingLetError(..), ToppleError::HangingLetError(..)) |
            (ToppleError::UnexpectedToken(..), ToppleError::UnexpectedToken(..)) |
            (ToppleError::KeywordIsCall(..), ToppleError::KeywordIsCall(..)) |
            (ToppleError::ExprPartialParse(..), ToppleError::ExprPartialParse(..)) |
            (ToppleError::EmptyIndex(..), ToppleError::EmptyIndex(..)) => true,
            _ => false,
        }
    }
}

impl Error for ToppleError {}
