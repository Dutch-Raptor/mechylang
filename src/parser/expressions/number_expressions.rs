use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::error::ErrorKind;
use crate::{Error, Expression, Parser, Token, TokenKind, trace};
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FloatLiteral {
    pub token: Token,
    pub value: f64,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Parser {
    pub(super) fn parse_number(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_number");
        let token = self.cur_token.clone();
        debug_assert!(matches!(token.kind, TokenKind::Number(_)), "Expected current token to be a number");

        let literal = match token.kind {
            TokenKind::Number(ref literal) => literal.clone(),
            _ => {
                return Err(
                    self.error_current(ErrorKind::UnexpectedToken, "Expected a number".to_string())
                )
            }
        };

        if let Ok(value) = literal.parse::<i64>() {
            Ok(Expression::IntegerLiteral(IntegerLiteral { token, value }))
        } else if let Ok(value) = literal.parse::<f64>() {
            Ok(Expression::FloatLiteral(FloatLiteral { token, value }))
        } else {
            Err(self.error_current(
                ErrorKind::InvalidNumber,
                "Invalid number literal".to_string(),
            ))
        }
    }
}