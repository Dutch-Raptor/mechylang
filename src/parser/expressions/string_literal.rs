use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::Parser;
use crate::{Error, Token};
use crate::errors::ErrorKind;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StringLiteral {
    pub token: Token,
    pub value: Rc<str>,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Parser {

    pub(super) fn parse_string(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let value = match self.cur_token.kind {
            TokenKind::String(ref s) => s.clone(),
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a string, got {:?}", self.cur_token),
                ))
            }
        };

        Ok(Expression::StringLiteral(StringLiteral { token, value: value.into() }))
    }
}
#[cfg(test)]
mod tests {
    use crate::lexer::tokens::TokenKind;
    use crate::parser::expressions::Expression;
    use crate::parser::expressions::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::StringLiteral(ref literal) => {
                    assert_eq!(literal.value, "hello world".into());
                    assert_eq!(
                        literal.token.kind,
                        TokenKind::String("hello world".into())
                    );
                }
                _ => panic!("expected string literal expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}