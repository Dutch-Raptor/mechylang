use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::parser::expressions::Expression;
use crate::parser::expressions::precedence::Precedence;
use crate::parser::Parser;
use crate::{Error, Token, TokenKind, trace};

#[derive(Debug, PartialEq, Serialize)]
pub struct BreakStatement {
    pub token: Token,
    pub value: Option<Expression>,
}

impl Display for BreakStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{} {};", self.token, value),
            None => write!(f, "{};", self.token),
        }
    }
}

impl Parser {
    pub(super) fn parse_break_statement(&mut self) -> Result<BreakStatement, Error> {
        let _trace = trace!("parse_break_statement");
        debug_assert!(self.is_cur_token(TokenKind::Break), "Expected current token to be `Break`");
        let token = self.cur_token.clone();


        // check if we have a value to return
        let value = if !Parser::is_statement_terminator(&self.peek_token, &self.cur_token) {
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            Some(value)
        } else {
            None
        };

        Ok(BreakStatement { token, value })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::statements::Statement;
    use crate::parser::tests::parse;

    #[test]
    fn test_break_continue() {
        let input = r#"
        break;
        continue;
        break 1;
        "#;

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 3);

        match &statements[0] {
            Statement::Break(ref break_stmt) => {
                assert_eq!(break_stmt.value, None);
            }
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Continue(_) => {}
            _ => panic!("expected expression statement"),
        };

        match &statements[2] {
            Statement::Break(ref break_stmt) => {
                assert_eq!(break_stmt.value.is_some(), true);
                assert_eq!(break_stmt.value.as_ref().unwrap().to_string(), "1");
            }
            _ => panic!("expected expression statement"),
        };
    }

}
