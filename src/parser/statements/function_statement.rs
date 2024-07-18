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
    pub body: BlockExpression, }

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

    pub(crate) fn parse_function_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_function_statement");

        let token = self.cur_token.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => Identifier {
                token: self.cur_token.clone(),
                value: name.clone().into(),
            },
            TokenKind::LeftParen => {
                // Looks like this statement is actually an anonymous function
                // So we'll just parse it as such
                return self.parse_expression_statement();
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
    use crate::parser::statements::function_statement::FunctionStatement;
    use crate::parser::statements::Statement;
    use crate::parser::tests::parse;

    #[test]
    fn test_function_statement() {
        let input = "fn foo() { 1; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Function(FunctionStatement { name, body, .. }) => {
                assert_eq!(name.value.as_ref(), "foo");
                assert_eq!(body.statements.len(), 1);
            }
            _ => panic!("expected function statement"),
        };
    }
}
