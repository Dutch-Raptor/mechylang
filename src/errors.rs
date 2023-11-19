use core::fmt;
use std::fmt::{Display, Formatter};

use color_print::cformat;

use crate::evaluator::builtins::BuiltinError;

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
    MissingPrecedence,
    MissingPrefix,
    MissingInfix,
    InvalidIdentifier,
    InvalidNumber,
    TypeMismatch,
    UnknownOperator,
    IdentifierNotFound,
    InvalidOperator,
    TypeError,
    WrongNumberOfArguments,
    BuiltInError(BuiltinError),
    IndexOutOfBounds,
    IndexOperatorNotSupported,
    InvalidLeftHandSide,
    MutateError,
    MethodError,
    PropertyNotFound,
    InvalidDereference,
    AnonFunction,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let caret_pos = match self.column {
            0 => 0,
            _ => self.column - 1,
        };

        let pos = format!("{}:{}", self.line_nr, self.column);
        let pos_len = pos.len();

        let context = match &self.context {
            Some(context) => "| Context: ".to_owned() + context,
            None => "".to_owned(),
        };

        let message_parts = self.message.split('\n').collect::<Vec<&str>>();
        let first_line = message_parts[0];
        let rest = match message_parts.len() {
            1 => &[],
            _ => &message_parts[1..],
        };

        write!(
            f,
            "{}\n{}\n{}",
            cformat!(
                "{:pos_len$} | <r>Error: {:?}</r> {}",
                "",
                self.kind,
                context
            ),
            cformat!("{} | {}", pos, self.line),
            cformat!("{:pos_len$} | {:caret_pos$}<r>^</> {}", "", "", first_line),
        )?;

        for line in rest {
            write!(f, "\n{:pos_len$} | {:caret_pos$}  {}", "", "", line,)?;
        }

        Ok(())
    }
}
