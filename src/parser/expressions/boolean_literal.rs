use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::parser::{Parser, Error, Result};
use crate::{Span, TokenKind, trace};


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

impl<'a> Parser<'a> {
    pub(super) fn parse_boolean(&self) -> Result<BooleanLiteral> {
        let _trace = trace!("parse_boolean");
        let value = match self.cur_token.kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => {
                return Err(Error::UnexpectedToken {
                    span: self.cur_token.span.clone(),
                    expected: vec![TokenKind::True, TokenKind::False],
                    found: self.cur_token.kind.clone(),
                })
            }
        };

        Ok(BooleanLiteral { span: self.cur_token.span.clone(), value })
    }
}