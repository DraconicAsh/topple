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
                "{}:{} Strings must be ended with a double quote (\") and cannot be on multiple lines. Use single quotes (\' \') for Raw Strings", line + 1, chr + 1
            ),
            Self::LineReadError(line, e) => write!(
                f,
                "{}:_ Lower-Level error while attempting to read line; {e}", line + 1
            ),
            Self::NumberParseError(line, chr, e) => {
                write!(f, "{}:{} Lower-Level error when parsing number; {e}", line + 1, chr + 1)
            }
            Self::EmptyBinaryNumError(line, chr) => write!(
                f,
                "{}:{} Binary encoded numbers must have at least 1 digit", line + 1, chr + 1
            ),
            Self::InvalidOp(line, chr, c) => write!(
                f,
                "{}:{} !!INTERNAL BUG!! Lexer tried to parse invalid operator '{c}'", line + 1, chr + 1
            ),
            Self::InvalidChar(line, chr, c) => write!(
                f,
                "{}:{} Invalid character '{c}' cannot be used in an identifier and is not a valid operator", line + 1, chr + 1
            ),
            Self::OpenExprError(line, chr) => write!(
                f,
                "{}:{} All expressions must be ended with a semicolon ';'", line + 1, chr + 1
            ),
            Self::OpenBlockError(line, chr) => write!(f, "{}:{} All code block starts '{{' must be ended '}}'", line + 1, chr + 1),
            Self::OpenParenError(line, chr) => write!(f, "{}:{} All open parentheses '(' must be closed ')'", line + 1, chr + 1),
            Self::OpenBracketError(line, chr) => write!(f, "{}:{} All open brackets '[' must be closed ']'", line + 1, chr + 1),
            Self::HangingLetError(line, chr) => write!(
            f,
            "{}:{} 'let' must be followed by a variable name or a name and an assignment", line + 1, chr + 1
            ),
            Self::UnexpectedToken(t, line, chr) => write!(f, "{}:{} Unexpected token {t:?}", line + 1, chr + 1),
            Self::EmptyIndex(line, chr) => write!(f, "{}:{} Index operation requires a value", line + 1, chr + 1),
            Self::KeywordIsCall(k, line, chr) => write!(f, "{}:{} \"{k}\" is a function and must be called as such", line + 1, chr + 1),
            Self::ExprPartialParse(line, chr, end_l, end_c) => write!(f, "{}:{} Full expression could not be parsed; Stopped at {end_l}:{end_c}", line + 1, chr + 1),
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
