use crate::Position;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnterminatedString { string_start_position: Position},
}

pub type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}