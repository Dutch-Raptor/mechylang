use core::fmt;
use std::fmt::{Display, Formatter};

use color_print::cformat;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub line_nr: usize,
    pub column: usize,
    pub message: String,
    pub line: String,
    pub context: Option<String>,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedToken,
    ParseError,
    MissingPrecedence,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let blank = " ".repeat(match self.column {
            0 => 0,
            _ => self.column - 1,
        });

        write!(
            f,
            "{}",
            cformat!(
                "<r>Error ({:?}):</> {} at line {}:{}\n{}\n{}^",
                self.kind,
                self.message,
                self.line_nr,
                self.column,
                self.line,
                blank
            )
        )?;

        if let Some(context) = &self.context {
            write!(f, "\nContext: {} \n", context)?;
        }

        Ok(())
    }
}
