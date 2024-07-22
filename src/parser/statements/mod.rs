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
use crate::{Error, Token, trace};
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
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// let x = 5;
    /// # "#)
    /// ```
    Let(LetStatement),
    /// A `return` statement for returning values from functions.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
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
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// 5 + 10;
    /// # "#)
    /// ```
    Expression(ExpressionStatement),
    /// A `break` statement for breaking out of loops.
    ///
    /// Syntax:
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
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
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
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
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
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
    /// Parses a single statement in Mechylang.
    ///
    /// This function handles the parsing of various types of statements, including `let`, `return`,
    /// `break`, `continue`, and function statements. If the current token does not match any of these
    /// specific types, it treats the statement as an expression statement.
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing either:
    /// - `Ok(Statement)` if the statement was successfully parsed.
    /// - `Err(Error)` if there was an error during parsing, such as an unexpected token.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// - An error occurs while parsing an expression or any other statement type.
    /// - After parsing a statement, the next token is not a statement terminator.
    pub(crate) fn parse_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_statement");

        let statement = match self.cur_token.kind {
            TokenKind::Let => Statement::Let(self.parse_let_statement()?),
            TokenKind::Return => Statement::Return(self.parse_return_statement()?),
            TokenKind::Semicolon => {
                self.next_token();
                self.parse_statement()?
            }
            TokenKind::Break => Statement::Break(self.parse_break_statement()?),
            TokenKind::Continue => Statement::Continue(ContinueStatement { token: self.cur_token.clone() }),
            TokenKind::Fn => self.parse_function_statement()?,
            _ => Statement::Expression(self.parse_expression_statement()?),
        };


        if !Parser::is_statement_terminator(&self.peek_token, &self.cur_token) {
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


    /// Determines if the current token is a statement terminator in Mechylang.
    ///
    /// This function checks if the current token indicates the end of a statement. The statement
    /// terminators include semicolons (`;`), closing braces (`}`), end of file (`EOF`), closing
    /// parentheses (`)`), closing square brackets (`]`), and the `else` keyword. Additionally, if the
    /// current token is on a different line than the previous token, it is considered a statement
    /// terminator.
    ///
    /// # Arguments
    ///
    /// * `current_token` - The current token.
    /// * `previous_token` - The token that was parsed just before the current token.
    ///
    /// # Returns
    ///
    /// * `true` if the current token is a statement terminator.
    /// * `false` otherwise. 
    fn is_statement_terminator(current_token: &Token, previous_token: &Token) -> bool {
        if current_token.kind == TokenKind::Semicolon {
            return true;
        }

        if current_token.kind == TokenKind::RightSquirly {
            return true;
        }

        if current_token.kind == TokenKind::EOF {
            return true;
        }

        if current_token.kind == TokenKind::RightParen {
            return true;
        }

        if current_token.kind == TokenKind::RightSquare {
            return true;
        }

        if current_token.kind == TokenKind::Else {
            return true;
        }

        // if previous token was on a different line
        if current_token.position.line != previous_token.position.line {
            return true;
        }

        return false;
    }
}
