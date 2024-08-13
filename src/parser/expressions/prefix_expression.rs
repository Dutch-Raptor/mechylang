use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Error, Expression, Parser, Span, TokenKind, trace};
use crate::error::ErrorKind;
use crate::parser::expressions::Precedence;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct PrefixExpression {
    pub span: Span,
    pub operator: PrefixOperator,
    pub right: Rc<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PrefixOperator {
    Bang,
    Minus,
    BitwiseNot,
    Ampersand,
    Asterisk,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PrefixOperator::Bang => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
            PrefixOperator::BitwiseNot => write!(f, "~"),
            PrefixOperator::Ampersand => write!(f, "&"),
            PrefixOperator::Asterisk => write!(f, "*"),
        }
    }
}

impl Parser {
    pub(crate) fn has_prefix(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Identifier(_) => true,
            TokenKind::Number(_) => true,

            TokenKind::Fn => true,

            TokenKind::LeftParen => true,

            TokenKind::True | TokenKind::False => true,

            TokenKind::Bang => true,
            TokenKind::Minus => true,
            TokenKind::BitwiseNot => true,
            TokenKind::Ampersand => true,

            // Control flow expressions
            TokenKind::If => true,
            TokenKind::For => true,
            TokenKind::While => true,

            TokenKind::String(_) => true,
            TokenKind::Struct => true,

            // Block expressions
            TokenKind::LeftSquirly => true,

            // Array expressions
            TokenKind::LeftSquare => true,

            // Range expressions
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => true,
            _ => false,
        }
    }

    pub(crate) fn parse_prefix(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_prefix");
        match self.cur_token.kind {
            TokenKind::Identifier(_) => Ok(Expression::Identifier(self.parse_identifier()?)),
            TokenKind::Number(_) => self.parse_number(),
            TokenKind::True | TokenKind::False => Ok(Expression::Boolean(self.parse_boolean()?)),

            TokenKind::Fn => Ok(Expression::Function(self.parse_function_literal()?)),

            TokenKind::If => Ok(Expression::If(self.parse_if_expression()?)),

            TokenKind::Bang |
            TokenKind::Minus |
            TokenKind::BitwiseNot |
            TokenKind::Ampersand => Ok(Expression::Prefix(self.parse_prefix_expression()?)),

            // Block expression
            TokenKind::LeftSquirly => Ok(Expression::Block(self.parse_block_expression()?)),

            TokenKind::Struct => Ok(Expression::StructLiteral(self.parse_struct_literal()?)),
            TokenKind::String(_) => Ok(Expression::StringLiteral(self.parse_string()?)),

            TokenKind::LeftParen => self.parse_grouped_expression(),

            TokenKind::LeftSquare => Ok(Expression::ArrayLiteral(self.parse_array_expression()?)),

            TokenKind::For => Ok(Expression::For(self.parse_for_expression()?)),
            TokenKind::While => Ok(Expression::While(self.parse_while_expression()?)),

            TokenKind::RangeExclusive | TokenKind::RangeInclusive => self.parse_range_prefix_expression(),
            _ => Err(self.error_current(
                ErrorKind::MissingPrefix,
                format!("No registered prefix function for {:?}", self.cur_token.kind),
            )),
        }
    }

    pub(super) fn parse_prefix_expression(&mut self) -> Result<PrefixExpression, Error> {
        let _trace = trace!("parse_prefix_expression");
        let start = self.cur_token.span.start.clone();

        let operator = match &self.cur_token.kind {
            TokenKind::Bang => PrefixOperator::Bang,
            TokenKind::Minus => PrefixOperator::Minus,
            TokenKind::BitwiseNot => PrefixOperator::BitwiseNot,
            TokenKind::Ampersand => PrefixOperator::Ampersand,
            _ => {
                return Err(self.error_current(
                    ErrorKind::MissingPrefix,
                    format!("Expected a prefix operator, got {:?}", self.cur_token.kind),
                ))
            }
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(PrefixExpression {
            span: self.span_with_start(start),
            operator,
            right: Rc::new(right),
        })
    }
}
