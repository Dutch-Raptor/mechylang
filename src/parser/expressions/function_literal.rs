use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::parser::expressions::identifier::Identifier;
use crate::parser::{Parser, Result};
use crate::{trace, TokenKind, Span};
use crate::parser::error::Location;
use crate::parser::expressions::block_expression::BlockExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FunctionLiteral {
    pub span: Span,
    pub parameters: Rc<[Identifier]>,
    pub body: BlockExpression,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}

impl Parser<'_> {

    pub(super) fn parse_function_literal(&mut self) -> Result<FunctionLiteral> {
        let _trace = trace!("parse_function_literal");
        let start = self.cur_token.span.clone();

        self.expect_peek(TokenKind::LeftParen, Some(Location::FunctionExpression))?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenKind::LeftSquirly, Some(Location::FunctionExpression))?;

        let body = self.parse_block_expression()?;

        Ok(FunctionLiteral {
            span: self.span_with_start(&start),
            parameters: parameters.into(),
            body,
        })
    }

    pub(in crate::parser) fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token.kind == TokenKind::RightParen {
            self.next_token()?;
            return Ok(identifiers);
        }

        self.next_token()?;

        let ident = self.parse_identifier()?;

        identifiers.push(ident);

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token()?;
            self.next_token()?;

            let ident = self.parse_identifier()?;

            identifiers.push(ident);
        }

        self.expect_peek(TokenKind::RightParen, Some(Location::FunctionExpression))?;

        Ok(identifiers)
    }
}