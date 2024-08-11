use std::fmt;
use std::fmt::{Display, Formatter};
use color_print::cformat;
use serde::Serialize;
use crate::parser::Parser;
use crate::{Error, Token, trace, TokenKind};
use crate::error::ErrorKind;
use crate::parser::expressions::{Expression, Precedence};

#[derive(Debug, PartialEq, Serialize)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.return_value {
            Some(value) => write!(f, "{} {};", self.token, value),
            None => write!(f, "{};", self.token),
        }
    }
}

impl Parser {
    /// Parses a `return` statement in Mechylang.
    ///
    /// This function handles the parsing of a `return` statement, which is used to return a value
    /// from a function. It expects the current token to be the `return` keyword, optionally followed
    /// by an expression (the return value) and a semicolon (`;`).
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing either:
    /// - `Ok(ReturnStatement)` if the `return` statement was successfully parsed.
    /// - `Err(Error)` if there was an error during parsing, such as an unexpected token or missing expression.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// - Neither an expression nor a semicolon followed the `return` keyword.
    /// - An error occurs while parsing the expression.
    pub(super) fn parse_return_statement(&mut self) -> Result<ReturnStatement, Error> {
        let _trace = trace!("parse_return_statement");
        debug_assert!(self.is_cur_token(TokenKind::Return), "Expected current token to be `Return`");
        let token = self.cur_token.clone();
        self.next_token();

        if let TokenKind::Semicolon = self.cur_token.kind {
             return Ok(ReturnStatement {
                 token,
                 return_value: None,
             });
        }

        let expression = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            // If there is no expression, let the user know that they need to return something or
            // use a semicolon
            Err(Error {
                    kind: ErrorKind::MissingPrefix, ..
                }) => {
                return Err(self.error(
                    ErrorKind::UnexpectedToken,
                    cformat!(
                        "Expected an expression or semicolon after <i>`return`</i>, got <i>{:?}</i> instead",
                        self.cur_token.kind
                    ),
                    Some(&self.cur_token),
                    None,
                ));
            }
            Err(err) => return Err(err),
        };

        Ok(ReturnStatement {
            token,
            return_value: Some(expression),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::parser::expressions::Expression;

    #[test]
    fn test_parse_return_statement_with_value() {
        let source_code = r#"
        return 5;
        "#;
        let mut parser = Parser::from_source(source_code);

        let result = parser.parse_return_statement();

        match result {
            Ok(ret) => {
                assert!(ret.return_value.is_some());
                if let Some(Expression::IntegerLiteral(num)) = ret.return_value {
                    assert_eq!(num.value, 5);
                } else {
                    panic!("Expected Number expression but found {:#?}", ret.return_value);
                }
            }
            other => {
                panic!("Expected Return but found {:#?}", other)
            },
        }
    }
    
    #[test]
    fn test_parse_return_statement_without_value() {
        let source_code = r#"
        return;
        "#;
        let mut parser = Parser::from_source(source_code);

        let result = parser.parse_return_statement();

        match result {
            Ok(ret) => {
                assert!(ret.return_value.is_none());
            }
            other => {
                panic!("Expected Return but found {:#?}", other)
            },
        } 
    }
}