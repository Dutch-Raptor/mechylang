use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::{Expression, Precedence};
use crate::parser::Parser;
use crate::parser::statements::Statement;
use crate::{Error, Token, trace};

#[derive(Debug, PartialEq, Serialize)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", self.expression)
    }
}

impl Parser {
    pub(crate) fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_expression_statement");
        let statement = Statement::Expression(ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest)?,
        });

        if self.is_cur_token(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(statement)
    }
}
