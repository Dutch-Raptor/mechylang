use core::fmt;
use std::{
    fmt::{Display, Formatter},
    ops::Deref,
    rc::Rc,
};
use std::vec::IntoIter;
use color_print::cformat;
use itertools::Itertools;

use crate::{evaluator::runtime::builtins::BuiltinError, Token};

#[derive(Debug, PartialEq)]
pub struct InterpreterErrors(pub Vec<Error>);

impl Display for InterpreterErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter().map(|err| err.to_string()).join("\n"))
    }
}

impl Deref for InterpreterErrors {
    type Target = Vec<Error>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl IntoIterator for InterpreterErrors {
    type Item = Error;
    type IntoIter = IntoIter<Error>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub token: Option<Token>,
    pub message: String,
    pub line: Option<String>,
    pub context: Option<String>,
    pub lines: Rc<[String]>,
}

impl Error {
    pub fn new(
        kind: ErrorKind,
        message: impl ToString,
        token: Option<&Token>,
        lines: &[impl ToString],
        context: Option<String>,
    ) -> Self {
        let message = message.to_string();

        let line = token
            .map(|token| lines.get(token.position.line - 1))
            .flatten()
            .map(|line| line.to_string());

        Self {
            kind,
            token: token.cloned(),
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

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.token {
            Some(ref token) => self.fmt_with_token(f, token),
            None => self.fmt_without_token(f),
        }
    }
}

impl Error {
    fn fmt_without_token(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let message = self.message.split('\n').collect::<Vec<&str>>();
        let first_line = message[0];
        let rest = match message.len() {
            1 => &[],
            _ => &message[1..],
        };
        
        writeln!(f, "{}", cformat!("{:?}", self.kind))?;
        
        self.line
            .as_ref()
            .map(|line| writeln!(f, "{}", cformat!("{}", line)))
            .unwrap_or(Ok(()))?;
        
        writeln!(f, "{}", cformat!("{}", first_line))?;
        
        for line in rest {
            write!(f, "\n{}", cformat!("{}", line))?;
        }
        
        Ok(())
    }

    fn fmt_with_token(&self, f: &mut Formatter<'_>, token: &Token) -> fmt::Result {
        let caret_offset = match token.position.column {
            c if c < 1 => 0,
            _ => token.position.column - 1,
        };

        let caret_len = token.position.length;

        let pos = format!("{}:{}", token.position.line, token.position.column);
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
        if token.position.line > 1 {
            let lines_before = 2;
            let start_idx = if token.position.line - 1 < lines_before {
                0
            } else {
                token.position.line - lines_before - 1
            };

            for i in start_idx..token.position.line - 1 {
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
            write!(f, "\n{:pos_len$} | {:caret_offset$}  {}", "", "", line,)?;
        }

        // Print context after line with error
        if self.lines.len() >= token.position.line - 1 {
            let lines_after = 2;
            let end_idx = if token.position.line + lines_after - 1 > self.lines.len() {
                self.lines.len()
            } else {
                token.position.line + lines_after
            };

            for i in token.position.line..end_idx {
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
