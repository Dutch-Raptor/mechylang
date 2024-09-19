use std::{
    fmt::{Display},
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    LexerError(crate::lexer::Error),
    ParserError(crate::parser::Error),
    EvaluatorError(Box<crate::evaluator::Error>),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl From<crate::lexer::Error> for Error {
    fn from(err: crate::lexer::Error) -> Self {
        Error::LexerError(err)
    }
}
impl From<crate::parser::Error> for Error {
    fn from(err: crate::parser::Error) -> Self {
        Error::ParserError(err)
    }
}

impl From<Box<crate::evaluator::Error>> for Error {
    fn from(err: Box<crate::evaluator::Error>) -> Self {
        Error::EvaluatorError(err)
    }
}