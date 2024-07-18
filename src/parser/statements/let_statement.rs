use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::expressions::identifier::Identifier;
use crate::parser::expressions::precedence::Precedence;
use crate::parser::Parser;
use crate::parser::statements::Statement;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;

#[derive(Debug, PartialEq, Serialize)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {} = {};", self.token, self.name, self.value)
    }
}

impl Parser {

    pub(crate) fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_let_statement");
        let token = self.cur_token.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => name.clone(),
            _ => {
                return Err(self.error_peek(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an identifier, got {:?}", self.peek_token.kind),
                ))
            }
        };

        let name = Identifier {
            token: self.peek_token.clone(),
            value: name.into(),
        };

        self.next_token();

        self.expect_peek(TokenKind::AssignEqual)?;

        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        let statement = Statement::Let(LetStatement {
            token,
            name,
            value: expression,
        });
        Ok(statement)
    }
}
