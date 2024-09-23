use crate::{Span};

#[derive(Debug, PartialEq)]
pub enum Error {
    UnterminatedString { span: Span },
    UnsupportedEscapeSequence { span: Span },
    IllegalCharacter { span: Span, char: char },
}

pub type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}