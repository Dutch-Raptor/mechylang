pub mod precedence;
pub mod identifier;
pub mod number_expressions;
pub mod boolean_literal;
pub mod string_literal;
pub mod prefix_expression;
pub mod infix_expression;
pub mod if_expression;
pub mod function_literal;
pub mod call_expression;
pub mod array_literal;
pub mod index_expression;
pub mod range_expressions;
pub mod for_expression;
pub mod while_expression;
pub mod member_expression;
pub mod struct_literal;
pub mod block_expression;

/// # Parsing Expressions
///
/// This module contains the parsing logic for expressions.
///
/// ## Expressions
///
/// An expression is a combination of values, variables, operators, and functions that are interpreted to produce another value.
/// For example, `1 + 2` is an expression that evaluates to `3`.
/// Expressions can be as simple as a single value, or as complex as a function call with multiple arguments.
///
///
/// This file contains the following:
/// - `Expression` enum
/// - All expression variants
/// - implementations for `Display` for all expression variants
/// - implementations for the parse functions for all expression variants for the parser
use std::fmt::{self, Display, Formatter};

use serde::Serialize;

use crate::lexer::tokens::{Token, TokenKind};
pub use crate::parser::expressions::array_literal::ArrayLiteral;
pub use crate::parser::expressions::boolean_literal::BooleanLiteral;
pub use crate::parser::expressions::call_expression::CallExpression;
pub use crate::parser::expressions::number_expressions::{FloatLiteral, IntegerLiteral};
pub use crate::parser::expressions::for_expression::ForExpression;
pub use crate::parser::expressions::function_literal::FunctionLiteral;
pub use crate::parser::expressions::identifier::Identifier;
pub use crate::parser::expressions::if_expression::IfExpression;
pub use crate::parser::expressions::index_expression::IndexExpression;
pub use crate::parser::expressions::infix_expression::{InfixExpression, InfixOperator};
pub use crate::parser::expressions::member_expression::MemberExpression;
pub use crate::parser::expressions::precedence::Precedence;
pub use crate::parser::expressions::prefix_expression::{PrefixExpression, PrefixOperator};
pub use crate::parser::expressions::range_expressions::{RangeExpression, RangeFromExpression, RangeFullExpression, RangeToExpression};
pub use crate::parser::expressions::string_literal::StringLiteral;
pub use crate::parser::expressions::struct_literal::StructLiteral;
pub use crate::parser::expressions::while_expression::WhileExpression;
use crate::parser::Parser;
use crate::{Error, trace};
use crate::errors::ErrorKind;
use crate::parser::expressions::block_expression::BlockExpression;


#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanLiteral),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Block(BlockExpression),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    Index(IndexExpression),
    Range(RangeExpression),
    RangeTo(RangeToExpression),
    RangeFrom(RangeFromExpression),
    RangeFull(RangeFullExpression),
    For(ForExpression),
    While(WhileExpression),
    Member(MemberExpression),
    Unit(Token),
    StructLiteral(StructLiteral),
}

pub trait ExpressionToken {
    fn token(&self) -> &Token;
}

impl ExpressionToken for Expression {
    fn token(&self) -> &Token {
        match self {
            Expression::Identifier(ident) => &ident.token,
            Expression::IntegerLiteral(lit) => &lit.token,
            Expression::FloatLiteral(lit) => &lit.token,
            Expression::StructLiteral(lit) => &lit.token,
            Expression::Prefix(expr) => &expr.token,
            Expression::Infix(expr) => &expr.token,
            Expression::Boolean(boolean) => &boolean.token,
            Expression::If(if_expr) => &if_expr.token,
            Expression::Function(func) => &func.token,
            Expression::Call(call) => &call.token,
            Expression::Block(block) => &block.token,
            Expression::StringLiteral(lit) => &lit.token,
            Expression::ArrayLiteral(lit) => &lit.token,
            Expression::Index(index) => &index.token,
            Expression::Range(range) => &range.token,
            Expression::RangeTo(range) => &range.token,
            Expression::RangeFrom(range) => &range.token,
            Expression::RangeFull(range) => &range.token,
            Expression::For(for_expr) => &for_expr.token,
            Expression::While(while_expr) => &while_expr.token,
            Expression::Member(member) => &member.token,
            Expression::Unit(token) => token,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(lit) => write!(f, "{}", lit),
            Expression::FloatLiteral(lit) => write!(f, "{}", lit),
            Expression::Prefix(expr) => write!(f, "{}", expr),
            Expression::Infix(expr) => write!(f, "{}", expr),
            Expression::Boolean(boolean) => write!(f, "{}", boolean),
            Expression::If(if_expr) => write!(f, "{}", if_expr),
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::Block(block) => write!(f, "{}", block),
            Expression::StringLiteral(lit) => write!(f, "{}", lit),
            Expression::ArrayLiteral(lit) => write!(f, "{}", lit),
            Expression::Index(index) => write!(f, "{}", index),
            Expression::Range(range) => write!(f, "{}", range),
            Expression::RangeTo(range) => write!(f, "{}", range),
            Expression::RangeFrom(range) => write!(f, "{}", range),
            Expression::RangeFull(range) => write!(f, "{}", range),
            Expression::For(for_expr) => write!(f, "{}", for_expr),
            Expression::While(while_expr) => write!(f, "{}", while_expr),
            Expression::Member(member) => write!(f, "{}", member),
            Expression::Unit(_) => write!(f, "()"),
            Expression::StructLiteral(lit) => write!(f, "{}", lit),
        }
    }
}

impl Parser {
    pub(crate) fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let _trace = trace!("parse_expression");
        let token = self.cur_token.clone();

        if !self.has_prefix(&token.kind) {
            return Err(self.error_current_with_context(
                ErrorKind::MissingPrefix,
                format!("Expected a value, got {:?}", token.kind),
                "parse_expression".to_string(),
            ));
        }

        let mut left_exp = self.parse_prefix(token.kind)?;

        while !self.is_peek_token(TokenKind::Semicolon) && precedence < self.peek_precedence() {
            let peek_token = self.peek_token.clone();

            if !self.has_infix(&peek_token.kind) {
                return Ok(left_exp);
            }

            self.next_token();

            left_exp = self.parse_infix(peek_token.kind, left_exp)?;
        }

        return Ok(left_exp);
    }


    pub(super) fn parse_grouped_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_grouped_expression");
        self.next_token();

        if self.cur_token.kind == TokenKind::RightParen {
            return Ok(Expression::Unit(
                self.cur_token.clone()
            ));
        }

        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RightParen)?;

        Ok(expression)
    }

    pub(super) fn parse_expression_list(&mut self, end: TokenKind) -> Result<Vec<Expression>, Error> {
        let mut arguments = Vec::new();

        if self.peek_token.kind == end {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();

        arguments.push(match self.parse_expression(Precedence::Lowest) {
            Err(Error { kind: ErrorKind::MissingPrefix, .. }) => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an expression or a {:?}, got {:?} instead", end, self.cur_token.kind),
                ))
            }
            Err(e) => return Err(e),
            Ok(e) => e,
        });

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();

            // allow trailing commas
            if self.peek_token.kind == end {
                break;
            }

            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(arguments)
    }

}















