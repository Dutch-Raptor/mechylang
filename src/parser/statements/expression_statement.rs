use std::fmt::{Display, Formatter, self};
use serde::Serialize;
use crate::parser::expressions::{Expression, Precedence};
use crate::parser::Parser;
use crate::{trace, TokenKind, Span};
use crate::parser::{Result};

/// Represents an expression statement in Mechylang.
///
/// An expression statement is a statement that evaluates an expression.
/// In Mechylang, an expression statement can consist of various expressions
/// such as arithmetic operations, function calls, or variable references.
///
/// # Fields
///
/// * `token` - The token associated with the statement. Typically, this is the first
///   token of the expression.
/// * `expression` - The expression that this statement evaluates.
///
/// # Example
///
/// ```
/// use mechylang::test_utils::test_parse_ok;
/// test_parse_ok(r#"
/// 2 + 2 == 4
/// "#);
/// ```
#[derive(Debug, PartialEq, Serialize)]
pub struct ExpressionStatement {
    /// The span of the expression statement in the source code.
    pub span: Span,
    /// token of the expression.
    /// The expression that this statement evaluates.
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", self.expression)
    }
}

impl Parser {
    /// Parses an expression statement in Mechylang.
    ///
    /// This function handles the parsing of an expression statement, which consists of an expression
    /// optionally followed by a semicolon (`;`). The expression is parsed with the lowest precedence
    /// to allow all kinds of expressions to be parsed correctly.
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing either:
    /// - `Ok(ExpressionStatement)` if the expression statement was successfully parsed.
    /// - `Err(Error)` if there was an error during parsing, such as an unexpected token.
    ///
    /// # Errors
    ///
    /// This function returns an error if an error occurs while parsing the expression.
    pub(super) fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let _trace = trace!("parse_expression_statement");
        
        let start = self.cur_token.span.start.clone();
        
        let expression = self.parse_expression(Precedence::Lowest)?;
        
        let statement = ExpressionStatement {
            span: self.span_with_start(start),
            expression,
        };

        if self.is_cur_token(TokenKind::Semicolon) {
            self.next_token()?;
        }

        Ok(statement)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Parser};
    use crate::parser::expressions::{CallExpression, Expression, InfixExpression, InfixOperator};

    #[test]
    fn test_parse_simple_expression_statement() {
        let source_code = r#"
        5 + 10;
        "#;
        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_expression_statement();
        assert!(result.is_ok());

        match result {
            Ok(statement) => {
                match statement.expression {
                    Expression::Infix(
                        InfixExpression {
                            left,
                            operator,
                            right,
                            ..
                        }) => {
                        if let Expression::IntegerLiteral(ref integer) = left.as_ref() {
                            assert_eq!(integer.value, 5);
                        } else {
                            panic!("Expected left expression to be an integer literal, got {:?}", left);
                        }

                        assert_eq!(operator, InfixOperator::Plus);

                        if let Expression::IntegerLiteral(ref integer) = right.as_ref() {
                            assert_eq!(integer.value, 10);
                        } else {
                            panic!("Expected right expression to be an integer literal, got {:?}", right);
                        }
                    }
                    other => panic!("Expected infix expression, got {:?}", other)
                }
            }
            Err(err) => panic!("Unexpected error: {:?}", err)
        }
    }
    
    #[test]
    fn test_parse_expression_statement_without_semicolon() {
        let source_code = r#"
        42
        "#;
        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_expression_statement();
        assert!(result.is_ok());

        match result {
            Ok(statement) => {
                if let Expression::IntegerLiteral(ref integer) = statement.expression {
                    assert_eq!(integer.value, 42);
                } else {
                    panic!("Expected expression to be a number, got {:?}", statement.expression);
                }
            }
            Err(err) => panic!("Unexpected error: {:?}", err)
        }
    }
    
    #[test]
    fn test_parse_expression_statement_with_identifier() {
        let source_code = r#"
        some_var;
        "#;
        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_expression_statement();
        assert!(result.is_ok());

        match result {
            Ok(statement) => {
                if let Expression::Identifier(ref identifier) = statement.expression {
                    assert_eq!(identifier.value.as_ref(), "some_var");
                } else {
                    panic!("Expected expression to be an identifier, got {:?}", statement.expression);
                }
            }
            Err(err) => panic!("Unexpected error: {:?}", err)
        }
    }
    
    #[test]
    fn test_parse_expression_statement_with_function_call() {
        let source_code = r#"
        some_function(1, 2);
        "#;
        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_expression_statement();
        assert!(result.is_ok());

        match result {
            Ok(statement) => {
                match statement.expression {
                    Expression::Call(CallExpression {
                        ref function,
                        ref arguments,
                        ..
                    }) => {
                        if let Expression::Identifier(ref identifier) = function.as_ref() {
                            assert_eq!(identifier.value.as_ref(), "some_function");
                        } else {
                            panic!("Expected function to be an identifier, got {:?}", function);
                        }

                        assert_eq!(arguments.len(), 2);

                        if let Expression::IntegerLiteral(ref integer) = arguments[0] {
                            assert_eq!(integer.value, 1);
                        } else {
                            panic!("Expected first argument to be a number, got {:?}", arguments[0]);
                        }

                        if let Expression::IntegerLiteral(ref integer) = arguments[1] {
                            assert_eq!(integer.value, 2);
                        } else {
                            panic!("Expected second argument to be a number, got {:?}", arguments[1]);
                        }
                    }
                    other => panic!("Expected call expression, got {:?}", other)
                }
            }
            Err(err) => panic!("Unexpected error: {:?}", err)
        }
    }
}
