use std::fmt;
use std::fmt::{Display, Formatter};
use color_print::cformat;
use serde::Serialize;
use crate::parser::Parser;
use crate::parser::statements::Statement;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;
use crate::lexer::tokens::TokenKind;
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

    pub(crate) fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_return_statement");
        let token = self.cur_token.clone();
        self.next_token();

        if let TokenKind::Semicolon = self.cur_token.kind {
            let statement = Statement::Return(ReturnStatement {
                token,
                return_value: None,
            });
            return Ok(statement);
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

        let statement = Statement::Return(ReturnStatement {
            token,
            return_value: Some(expression),
        });

        Ok(statement)
    }
}
