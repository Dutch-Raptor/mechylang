use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::parser::expressions::{Expression, Identifier, Precedence};
use crate::parser::Parser;
use crate::{TokenKind, trace};
use crate::lexer::Span;

use crate::parser::{Error, Result};
use crate::parser::error::Location;

/// Represents a `let` statement in Mechylang.
///
/// A `let` statement is used to declare a new variable and assign it an initial value.
/// This struct captures the components of a `let` statement, including the token representing
/// the `let` keyword, the variable name, and the initial value assigned to the variable.
///
/// # Fields
///
/// * `token` - The token representing the `let` keyword.
/// * `name` - The identifier for the variable being declared.
/// * `value` - The expression representing the initial value assigned to the variable.
///
/// # Example
///
/// ```
/// # use mechylang::test_utils::test_parse_ok;
/// # test_parse_ok(r#"
/// let x = 5;
/// # "#);
/// ```
///
/// The above code snippet would be represented by a `LetStatement` with:
/// - `token` being the `Token` for the `let` keyword.
/// - `name` being the `Identifier` for `x`.
/// - `value` being the `Expression` for `5`.
///
/// # Usage
///
/// A `LetStatement` is typically created during the parsing phase of the interpreter
/// and is included in the abstract syntax tree (AST) to represent variable declarations.
#[derive(Debug, PartialEq, Serialize)]
pub struct LetStatement {
    /// The span of the `let` statement in the source code.
    pub span: Span,
    /// The identifier for the variable being declared.
    pub name: Identifier,
    /// The expression representing the initial value assigned to the variable.
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

impl Parser<'_> {
    /// Parses a `let` statement in Mechylang.
    ///
    /// This function handles the parsing of a `let` statement, which is used to declare variables.
    /// It expects the current token to be the `let` keyword, followed by an identifier (the variable name),
    /// an equals sign (`=`), and an expression (the variable's value).
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing either:
    /// - `Ok(LetStatement)` if the `let` statement was successfully parsed.
    /// - `Err(Error)` if there was an error during parsing, such as an unexpected token.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// - The token following the `let` keyword is not an identifier.
    /// - The token following the identifier is not an equals sign (`=`).
    /// - An error occurs while parsing the expression.
    pub(super) fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let _trace = trace!("parse_let_statement");
        debug_assert!(self.is_cur_token(TokenKind::Let), "Expected current token to be `Let`");
        let start = self.cur_token.span.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => name.clone(),
            _ => {
                return Err(
                    Error::UnexpectedToken {
                        span: self.peek_token.span.clone(),
                        expected: vec![TokenKind::Identifier(String::new())],
                        found: self.peek_token.kind.clone(),
                        location: Some(Location::LetStatement),
                    }
                )
            }
        };

        let name = Identifier {
            span: self.peek_token.span.clone(),
            value: name.into(),
        };

        self.next_token()?;

        self.expect_peek(TokenKind::AssignEqual, Some(Location::LetStatement))?;

        self.next_token()?;

        let expression = self.parse_expression(Precedence::Lowest)?;

        Ok(LetStatement {
            span: self.span_with_start(&start),
            name,
            value: expression,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{Parser, TokenKind};
    use crate::parser::Error;
    use crate::parser::error::Location;
    use crate::parser::expressions::Expression;

    #[test]
    fn test_parse_let_statement_with_valid_input() {
        let source_code = r#"
        let x = 5;
        "#;
        let mut parser = Parser::from_source(source_code);
        // read tokens into cur and peek
        parser.next_token().unwrap();
        parser.next_token().unwrap();

        let result = parser.parse_let_statement();

        match result {
            Ok(let_statement) => {
                assert_eq!(let_statement.name.value.as_ref(), "x");
                if let Expression::IntegerLiteral(num) = let_statement.value {
                    assert_eq!(num.value, 5);
                } else {
                    panic!("Expected Number expression but found {:#?}", let_statement.value);
                }
            }
            other => {
                panic!("Expected LetStatement but found {:#?}", other)
            },
        } 
    }
    
    #[test]
    fn test_parse_let_statement_missing_identifier() {
        let source_code = r#"
        let = 5;
        "#;
        let mut parser = Parser::from_source(source_code);
        // read tokens into cur and peek
        parser.next_token().unwrap();
        parser.next_token().unwrap();

        let result = parser.parse_let_statement();
        assert!(result.is_err());

        match result {
            Err(Error::UnexpectedToken {
                    span: _, expected, found, location
                }) => {
                assert_eq!(expected, vec![TokenKind::Identifier(Default::default())]);
                assert_eq!(found, TokenKind::AssignEqual);
                assert_eq!(location, Some(Location::LetStatement));
            },
            _ => panic!("Expected UnexpectedToken error but found {:#?}", result),
        }
    }
    
    #[test]
    fn test_parse_let_statement_missing_assign() {
        let source_code = r#"
        let x 5;
        "#;
        let mut parser = Parser::from_source(source_code);
        // read tokens into cur and peek
        parser.next_token().unwrap();
        parser.next_token().unwrap();

        let result = parser.parse_let_statement();
        assert!(result.is_err());

        match result {
            Err(Error::UnexpectedToken {
                    span: _, expected, found, location
                }) => {
                assert_eq!(expected, vec![TokenKind::AssignEqual]);
                assert_eq!(found, TokenKind::Number("5".into()));
                assert_eq!(location, Some(Location::LetStatement));
            },
            _ => panic!("Expected UnexpectedToken error but found {:#?}", result),
        }
    }
}
