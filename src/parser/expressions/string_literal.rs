use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Error, Parser, Span};
use crate::error::ErrorKind;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StringLiteral {
    pub span: Span,
    pub value: Rc<str>,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Parser {
    pub(super) fn parse_string(&mut self) -> Result<StringLiteral, Error> {
        let start = self.cur_token.span.start.clone();
        
        let value = self.cur_token.kind
            .as_string()
            .ok_or_else(|| self.error_current(
                ErrorKind::UnexpectedToken,
                format!("Expected a string, got {:?}", self.cur_token.kind),
            ))?;

        Ok(StringLiteral { span: self.span_with_start(start), value: value.into() })
    }
}
#[cfg(test)]
mod tests {
    use crate::TokenKind;
    use crate::parser::expressions::Expression;
    use crate::parser::tests::parse;
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
                }
                _ => panic!("expected string literal expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}