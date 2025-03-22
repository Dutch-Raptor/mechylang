use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::{Expression, Parser, Span, TokenKind, trace};
use crate::parser::{Error, Result};
use crate::parser::error::Location;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FloatLiteral {
    pub span: Span,
    pub value: f64,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IntegerLiteral {
    pub span: Span,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Parser<'_> {
    pub(super) fn parse_number(&mut self) -> Result<Expression> {
        let _trace = trace!("parse_number");
        let token = &self.cur_token;
        debug_assert!(matches!(token.kind, TokenKind::Number(_)), "Expected current token to be a number");

        let literal = token.kind
            .as_number()
            .ok_or_else(|| Error::UnexpectedToken {
                span: self.cur_token.span.clone(),
                expected: vec![TokenKind::Number(String::new())],
                found: self.cur_token.kind.clone(),
                location: Some(Location::Expression),
            })?;
        
        let span = token.span.clone();
        
        match (literal.parse::<i64>(), literal.parse::<f64>()) {
            (Ok(value), _) => Ok(Expression::IntegerLiteral(IntegerLiteral { span, value })),
            (_, Ok(value)) => Ok(Expression::FloatLiteral(FloatLiteral { span, value })),
            (Err(_), Err(_)) => Err(
                Error::InvalidNumber {
                    span: self.cur_token.span.clone(),
                    found: self.cur_token.kind.clone(),
                }
            ),
        }

    }
}