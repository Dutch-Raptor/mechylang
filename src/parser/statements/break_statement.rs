use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::parser::expressions::Expression;
use crate::parser::expressions::precedence::Precedence;
use crate::parser::Parser;
use crate::parser::statements::Statement;
use crate::{Error, Token, trace};

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
    pub(crate) fn parse_break_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_break_statement");
        let token = self.cur_token.clone();


        // check if we have a value to return
        let value = if !self.peek_token.is_statement_terminator(&self.cur_token) {
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            Some(value)
        } else {
            None
        };

        Ok(Statement::Break(BreakStatement { token, value }))
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
