use crate::{Span};

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnterminatedString { span: Span },
    UnsupportedEscapeSequence { span: Span },
    IllegalCharacter { span: Span, found: char },
}

pub type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}