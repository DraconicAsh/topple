use std::error::Error;
use std::fmt::Display;

pub type ToppleResult<T> = Result<T, ToppleError>;

#[derive(Debug)]
pub enum ToppleError {
    LexerError(LexerError),
}

impl Display for ToppleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexerError(e) => write!(f, "{e}"),
        }
    }
}

impl Error for ToppleError {}

#[derive(Debug)]
pub struct LexerError {
    txt: String,
    line: u64,
    source: Option<Box<dyn Error>>,
}

impl LexerError {
    pub fn new<T: ToString>(txt: T, line: u64) -> Self {
        Self {
            txt: txt.to_string(),
            line,
            source: None,
        }
    }

    pub fn source(mut self, err: Box<dyn Error>) -> Self {
        self.source = Some(err);
        self
    }

    pub fn wrap(self) -> ToppleError {
        ToppleError::LexerError(self)
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let src_txt = match &self.source {
            Some(e) => format!("\nTrace: {e}"),
            None => String::new(),
        };
        write!(
            f,
            "Lexer Error at Line {}: {}{}",
            self.line, self.txt, src_txt
        )
    }
}

impl Error for LexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_deref()
    }
}
