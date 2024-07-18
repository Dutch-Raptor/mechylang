use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::Parser;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;
use crate::parser::expressions::precedence::Precedence;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct PrefixExpression {
    pub token: Token,
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

    pub(crate) fn parse_prefix(&mut self, token: TokenKind) -> Result<Expression, Error> {
        let _trace = trace!("parse_prefix");
        match token {
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Number(_) => self.parse_number(),
            TokenKind::True | TokenKind::False => self.parse_boolean(),

            TokenKind::Fn => self.parse_function_literal(),

            TokenKind::If => self.parse_if_expression(),

            TokenKind::Bang => self.parse_prefix_expression(),
            TokenKind::Minus => self.parse_prefix_expression(),
            TokenKind::BitwiseNot => self.parse_prefix_expression(),
            TokenKind::Ampersand => self.parse_prefix_expression(),

            // Block expression
            TokenKind::LeftSquirly => Ok(Expression::Block(self.parse_block_expression()?)),

            TokenKind::Struct => self.parse_struct_literal(),
            TokenKind::String(_) => self.parse_string(),

            TokenKind::LeftParen => self.parse_grouped_expression(),

            TokenKind::LeftSquare => self.parse_array_expression(),

            TokenKind::For => self.parse_for_expression(),
            TokenKind::While => self.parse_while_expression(),

            TokenKind::RangeExclusive | TokenKind::RangeInclusive => self.parse_range_prefix_expression(),
            _ => Err(self.error_current(
                ErrorKind::MissingPrefix,
                format!("No registered prefix function for {:?}", token),
            )),
        }
    }

    pub(super) fn parse_prefix_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_prefix_expression");
        let token = self.cur_token.clone();

        let operator = match token.kind {
            TokenKind::Bang => PrefixOperator::Bang,
            TokenKind::Minus => PrefixOperator::Minus,
            TokenKind::BitwiseNot => PrefixOperator::BitwiseNot,
            TokenKind::Ampersand => PrefixOperator::Ampersand,
            _ => {
                return Err(self.error_current(
                    ErrorKind::MissingPrefix,
                    format!("Expected a prefix operator, got {:?}", token),
                ))
            }
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right: Rc::new(right),
        }))
    }
}
