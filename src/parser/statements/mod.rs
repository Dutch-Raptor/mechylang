pub mod function_statement;
pub mod let_statement;
pub mod return_statement;
pub mod expression_statement;
pub mod break_statement;
pub mod continue_statement;

use std::fmt;
use std::fmt::{Display, Formatter};
use color_print::cformat;
use serde::Serialize;
use crate::{Error, trace};
use crate::errors::ErrorKind;
use crate::lexer::tokens::TokenKind;
use crate::parser::Parser;
use crate::parser::statements::break_statement::BreakStatement;
use crate::parser::statements::continue_statement::ContinueStatement;
use crate::parser::statements::expression_statement::ExpressionStatement;
use crate::parser::statements::function_statement::FunctionStatement;
use crate::parser::statements::let_statement::LetStatement;
use crate::parser::statements::return_statement::ReturnStatement;

/// Represents the various types of statements in Mechylang.
///
/// A statement in Mechylang can be one of the following:
#[derive(Debug, PartialEq, Serialize)]
pub enum Statement {
    /// A `let` statement for variable declarations.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_eval_ok;
    /// # test_eval_ok(r#"
    /// let x = 5;
    /// # "#)
    /// ```
    Let(LetStatement),
    /// A `return` statement for returning values from functions.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_eval_ok;
    /// # test_eval_ok(r#"
    /// fn add(a, b) {
    ///     return a + b;
    /// }
    /// # "#)
    /// ```
    Return(ReturnStatement),
    /// An expression statement for general expressions.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_eval_ok;
    /// # test_eval_ok(r#"
    /// 5 + 10;
    /// # "#)
    /// ```
    Expression(ExpressionStatement),
   /// A `break` statement for breaking out of loops.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_eval_ok;
    /// # test_eval_ok(r#"
    /// let x = 0;
    /// while (x < 10) {
    ///     if (x == 5) {
    ///         break;
    ///     }
    ///     x = x + 1;
    /// }
    /// # "#)
    /// ```
    Break(BreakStatement),
    /// A `continue` statement for continuing to the next iteration of loops.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_eval_ok;
    /// # test_eval_ok(r#"
    /// let x = 0;
    /// let sum = 0;
    /// while (x < 10) {
    ///     x = x + 1;
    ///     if (x % 2 == 0) {
    ///         continue;
    ///     }
    ///     sum = sum + x;
    /// }
    /// # "#)
    /// ```
    Continue(ContinueStatement),
    /// A function declaration statement.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_eval_ok;
    /// # test_eval_ok(r#"
    /// fn add(a, b) {
    ///     return a + b;
    /// }
    /// # "#)
    /// ```
    Function(FunctionStatement),
}
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s),
            Statement::Return(s) => write!(f, "{}", s),
            Statement::Expression(s) => write!(f, "{}", s),
            Statement::Break(s) => write!(f, "{}", s),
            Statement::Continue(s) => write!(f, "{}", s),
            Statement::Function(s) => write!(f, "{}", s),
        }
    }
}

impl Parser {
    pub(crate) fn parse_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_statement");

        let statement = match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement()?,
            TokenKind::Return => self.parse_return_statement()?,
            TokenKind::Semicolon => {
                self.next_token();
                self.parse_statement()?
            }
            TokenKind::Break => self.parse_break_statement()?,
            TokenKind::Continue => Statement::Continue(ContinueStatement { token: self.cur_token.clone() }),
            TokenKind::Fn => self.parse_function_statement()?,
            _ => self.parse_expression_statement()?,
        };


        if !self.peek_token.is_statement_terminator(&self.cur_token) {
            return Err(self.error_peek(
                ErrorKind::UnexpectedToken,
                cformat!(
                "Expected end of statement, got <i>{:?}</i> instead",
                self.peek_token.kind
                ),
            ));
        }

        if self.peek_token.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(statement)
    }
}
