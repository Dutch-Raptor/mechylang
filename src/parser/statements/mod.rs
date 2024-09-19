mod function_statement;
mod let_statement;
mod return_statement;
mod expression_statement;
mod break_statement;
mod continue_statement;

use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::{Token, trace, TokenKind, Parser};
use crate::parser::{Result};

pub use function_statement::FunctionStatement;
pub use let_statement::LetStatement;
pub use return_statement::ReturnStatement;
pub use expression_statement::ExpressionStatement;
pub use break_statement::BreakStatement;
pub use continue_statement::ContinueStatement;

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

impl<'a> Parser<'a> {
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
    pub(crate) fn parse_statement(&mut self) -> Result<Statement> {
        let _trace = trace!("parse_statement");

        let statement = match self.cur_token.kind {
            TokenKind::Let => Statement::Let(self.parse_let_statement()?),
            TokenKind::Return => Statement::Return(self.parse_return_statement()?),
            TokenKind::Semicolon => {
                self.next_token()?;
                self.parse_statement()?
            }
            TokenKind::Break => Statement::Break(self.parse_break_statement()?),
            TokenKind::Continue => Statement::Continue(ContinueStatement { span: self.cur_token.span.clone() }),
            TokenKind::Fn => self.parse_function_statement()?,
            _ => Statement::Expression(self.parse_expression_statement()?),
        };


        if !Parser::is_statement_terminator(&self.peek_token) {
            // Skip unterminated statement error for now
            // return Err(Error::UnterminatedStatement { span: self.cur_token.span.clone() });
        }

        if self.peek_token.kind == TokenKind::Semicolon {
            self.next_token()?;
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
    fn is_statement_terminator(current_token: &Token) -> bool {
        if current_token.kind == TokenKind::Semicolon
            || current_token.kind == TokenKind::RightSquirly
            || current_token.kind == TokenKind::EOF
            || current_token.kind == TokenKind::RightParen
            || current_token.kind == TokenKind::RightSquare
            || current_token.kind == TokenKind::Else
        {
            return true;
        }

        false
    }
}
