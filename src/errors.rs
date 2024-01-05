use core::fmt;
use std::{
    fmt::{Display, Formatter},
    ops::Deref,
    rc::Rc,
};

use color_print::cformat;
use itertools::Itertools;

use crate::evaluator::builtins::BuiltinError;

#[derive(Debug, PartialEq)]
pub struct InterpreterErrors(pub Vec<Error>);

impl Display for InterpreterErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|err| err.to_string()).join("\n"))
    }
}

impl Deref for InterpreterErrors {
    type Target = Vec<Error>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub line_nr: usize,
    pub column: usize,
    pub message: String,
    pub line: String,
    pub context: Option<String>,
    pub lines: Rc<[String]>,
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
        let caret_offset = match self.column {
            c if c < 2 => 0,
            _ => self.column - 2, // -1 for 1 indexing, -1 becuase it is an offset
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

        // Print context before line with error
        if self.line_nr > 1 {
            let lines_before = 2;
            let start_idx = if self.line_nr - 1 < lines_before {
                0
            } else {
                self.line_nr - lines_before - 1
            };

            for i in start_idx..self.line_nr - 1 {
                let line = match self.lines.get(i) {
                    Some(line) => line,
                    None => continue,
                };

                writeln!(f, "{}", cformat!("<dim>{:<pos_len$} | {}</>", i + 1, line))?;
            }
        }

        writeln!(
            f,
            "{}",
            cformat!(
                "{:pos_len$} | <r>Error: {:?}</r> {}",
                "",
                self.kind,
                context
            ),
        )?;

        writeln!(f, "{}", cformat!("{} | {}", pos, self.line),)?;

        writeln!(
            f,
            "{}",
            cformat!(
                "{:pos_len$} | {:caret_offset$}<r>^</> {}",
                "",
                "",
                first_line
            ),
        )?;

        for line in rest {
            write!(f, "\n{:pos_len$} | {:caret_offset$}  {}", "", "", line,)?;
        }

        // Print context after line with error
        if self.lines.len() >= self.line_nr - 1 {
            let lines_after = 2;
            let end_idx = if self.line_nr + lines_after - 1 > self.lines.len() {
                self.lines.len()
            } else {
                self.line_nr + lines_after
            };

            for i in self.line_nr..end_idx {
                let line = match self.lines.get(i) {
                    Some(line) => line,
                    None => continue,
                };

                writeln!(f, "{}", cformat!("<dim>{:<pos_len$} | {}</>", i + 1, line))?;
            }
        }

        Ok(())
    }
}
