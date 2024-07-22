use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::Parser;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct BooleanLiteral {
    pub token: Token,
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
        let token = self.cur_token.clone();
        let value = match token.kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected boolean literal, got {:?}", token.kind),
                ))
            }
        };

        Ok(BooleanLiteral { token, value })
    }
}