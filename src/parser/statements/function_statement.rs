use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::identifier::Identifier;
use crate::parser::Parser;
use crate::parser::statements::Statement;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;
use crate::parser::expressions::block_expression::BlockExpression;

#[derive(Debug, PartialEq, Serialize)]
pub struct FunctionStatement {
    pub token: Token,
    pub name: Identifier,
    pub parameters: Rc<[Identifier]>,
    pub body: BlockExpression,
}

impl Display for FunctionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let params = self.parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", ");
        write!(
            f,
            "{} {}({}) {}",
            self.token, self.name, params, self.body
        )
    }
}
impl Parser {
    /// Parses a function statement in Mechylang.
    ///
    /// This method handles the parsing of function declarations and anonymous functions in the source code.
    /// It expects a function to be defined with a name, followed by a list of parameters enclosed in parentheses,
    /// and a block of code enclosed in curly braces.
    ///
    /// # Returns
    ///
    /// A `Result` which is:
    /// * `Ok(Statement::Function)` containing the parsed function statement if successful.
    /// * `Ok(Statement::Expression)` if an anonymous function is parsed instead.
    /// * `Err(Error)` containing any errors encountered during parsing.
    ///
    /// # Errors
    ///
    /// If any parsing error occurs, the method returns an `Err` variant with an `Error` containing the encountered error(s).
    pub(super) fn parse_function_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_function_statement");
        debug_assert!(self.is_cur_token(TokenKind::Fn), "Expected current token to be `Fn`");

        let token = self.cur_token.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => Identifier {
                token: self.cur_token.clone(),
                value: name.clone().into(),
            },
            TokenKind::LeftParen => {
                // Looks like this statement is actually an anonymous function
                // So we'll just parse it as such
                return Ok(Statement::Expression(self.parse_expression_statement()?));
            }
            _ => {
                return Err(self.error_peek(
                    ErrorKind::UnexpectedToken, format!("Expected an identifier, got {:?}", self.peek_token.kind),
                ))
            }
        };

        self.next_token();

        self.expect_peek(TokenKind::LeftParen)?;

        let parameters = self.parse_function_parameters()?.into();

        self.expect_peek(TokenKind::LeftSquirly)?;
        let body = self.parse_block_expression()?;

        let statement = Statement::Function(FunctionStatement {
            name,
            token,
            parameters,
            body,
        });

        Ok(statement)
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::parser::expressions::Expression;
    use crate::parser::statements::Statement;

    #[test]
    fn test_parse_function_statement_with_name() {
        let source_code = r#"
        fn add(x, y) {
            return x + y;
        }
        "#;
        let mut parser = Parser::from_source(source_code);

        let result = parser.parse_function_statement();
        assert!(result.is_ok());

        match result {
            Ok(Statement::Function(function)) => {
                assert_eq!(function.name.value.as_ref(), "add");
                assert_eq!(function.parameters.len(), 2);
                assert_eq!(function.parameters[0].value.as_ref(), "x");
                assert_eq!(function.parameters[1].value.as_ref(), "y");
            }
            other => {
                panic!("Expected Function but found {:#?}", other)
            },
        } 
    }
    
    #[test]
    fn test_parse_anonymous_function() {
        let source_code = r#"
        fn (x, y) {
            return x + y;
        }
        "#;
        let mut parser = Parser::from_source(source_code);

        let result = parser.parse_function_statement();
        assert!(result.is_ok());

        match result {
            Ok(Statement::Expression(expression)) => {
                if let Expression::Function(function) = expression.expression {
                    assert_eq!(function.parameters.len(), 2);
                    assert_eq!(function.parameters[0].value.as_ref(), "x");
                    assert_eq!(function.parameters[1].value.as_ref(), "y");
                } else {
                    panic!("Expected Function expression but found {:#?}", expression.expression);
                }
            }
            other => {
                panic!("Expected Expression but found {:#?}", other)
            },
        } 
    }
}
