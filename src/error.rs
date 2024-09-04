use core::fmt;
use std::{
    fmt::{Display, Formatter},
    ops::Deref,
    rc::Rc,
};
use std::vec::IntoIter;
use color_print::cformat;
use crate::evaluator::runtime::builtins::BuiltinError;
use crate::{Span};

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

#[derive(Debug, PartialEq)]
pub struct InterpreterErrors(pub Vec<ErrorOld>);

impl Display for InterpreterErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter().map(|err| err.to_string()).collect::<Vec<String>>().join("\n"))
    }
}

impl std::error::Error for InterpreterErrors {}

impl Deref for InterpreterErrors {
    type Target = Vec<ErrorOld>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl IntoIterator for InterpreterErrors {
    type Item = ErrorOld;
    type IntoIter = IntoIter<ErrorOld>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct ErrorOld {
    pub kind: ErrorKind,
    pub span: Box<Span>,
    pub message: String,
    pub line: Option<String>,
    pub context: Option<String>,
    pub lines: Rc<[String]>,
}

impl ErrorOld {
    pub fn new(
        kind: ErrorKind,
        message: impl ToString,
        span: Span,
        lines: &[impl ToString],
        context: Option<String>,
    ) -> Self {
        let message = message.to_string();

        let line = span.start.line
            .checked_sub(1)
            .and_then(|line| lines.get(line))
            .map(|line| line.to_string());

        Self {
            kind,
            span: Box::new(span),
            message,
            line,
            context,
            lines: Rc::from(
                lines
                    .iter()
                    .map(|line| line.to_string())
                    .collect::<Vec<_>>(),
            ),
        }
    }
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

impl Display for ErrorOld {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let caret_offset = self.span.start.column.saturating_sub(1);

        let caret_len = self.span.length();

        let pos = format!("{}:{}", self.span.start.line, self.span.start.column);
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
        if self.span.start.line > 1 {
            let lines_before = 2;
            let start_idx = if self.span.start.line - 1 < lines_before {
                0
            } else {
                self.span.start.line - lines_before - 1
            };

            for i in start_idx..self.span.start.line - 1 {
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

        self.line
            .as_ref()
            .map(|line| writeln!(f, "{}", cformat!("{:pos_len$} | {}", pos, line)))
            .unwrap_or(Ok(()))?;

        writeln!(
            f,
            "{}",
            cformat!(
                "{:pos_len$} | {:caret_offset$}<r>{:^<caret_len$}</> {}",
                "",
                "",
                "",
                first_line
            ),
        )?;

        for line in rest {
            write!(f, "\n{:pos_len$} | {:caret_offset$}  {}", "", "", line, )?;
        }

        // Print context after line with error
        if self.lines.len() >= self.span.start.line - 1 {
            let lines_after = 2;
            let end_idx = if self.span.start.line + lines_after - 1 > self.lines.len() {
                self.lines.len()
            } else {
                self.span.start.line + lines_after
            };

            for i in self.span.start.line..end_idx {
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