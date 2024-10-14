use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Expression, Parser, Span, TokenKind, trace};
use crate::parser::{
    expressions::Precedence,
    Error,
    Result,
};
use crate::parser::expressions::ExpressionSpanExt;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct PrefixExpression {
    pub span: Span,
    pub right_span: Span,
    pub operator_span: Span,
    pub operator: PrefixOperator,
    pub right: Rc<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Copy)]
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

impl<'a> Parser<'a> {
    pub(crate) fn parse_prefix(&mut self) -> Result<Expression> {
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
            _ => Err(Error::InvalidPrefix {
                span: self.cur_token.span.clone(),
                found: self.cur_token.kind.clone(),
            }),
        }
    }

    pub(super) fn parse_prefix_expression(&mut self) -> Result<PrefixExpression> {
        let _trace = trace!("parse_prefix_expression");
        let operator_span = self.cur_token.span.clone();

        let operator = match &self.cur_token.kind {
            TokenKind::Bang => PrefixOperator::Bang,
            TokenKind::Minus => PrefixOperator::Minus,
            TokenKind::BitwiseNot => PrefixOperator::BitwiseNot,
            TokenKind::Ampersand => PrefixOperator::Ampersand,
            _ => {
                return Err(Error::InvalidPrefix {
                    span: self.cur_token.span.clone(),
                    found: self.cur_token.kind.clone(),
                })
            }
        };

        self.next_token()?;

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(PrefixExpression {
            span: self.span_with_start(&operator_span),
            right_span: right.span().clone(),
            operator_span,
            operator,
            right: Rc::new(right),
        })
    }
}
