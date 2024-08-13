use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::parser::Parser;
use crate::{Error, Span, TokenKind, trace};
use crate::error::ErrorKind;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct BooleanLiteral {
    pub span: Span,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Parser {

    pub(super) fn parse_boolean(&self) -> Result<BooleanLiteral, Error> {
        let _trace = trace!("parse_boolean");
        let value = match self.cur_token.kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected boolean literal, got {:?}", self.cur_token.kind),
                ))
            }
        };

        Ok(BooleanLiteral { span: self.cur_token.span.clone(), value })
    }
}