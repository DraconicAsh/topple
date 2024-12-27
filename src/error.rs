use std::error::Error;
use std::fmt::Display;

pub type ToppleResult<T> = Result<T, ToppleError>;

#[derive(Debug)]
pub enum ToppleError {
    OpenStringError(usize, usize),
    LineReadError(usize, Box<dyn Error>),
    NumberParseError(usize, usize, Box<dyn Error>),
    EmptyBinaryNumError(usize, usize),
    InvalidOp(usize, usize, char),
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
        }
    }
}

impl Error for ToppleError {}
