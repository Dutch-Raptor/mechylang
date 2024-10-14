use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Parser, Span, TokenKind};
use crate::parser::{Error, Result};
use crate::parser::error::Location;

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

impl<'a> Parser<'a> {
    pub(super) fn parse_string(&mut self) -> Result<StringLiteral> {
        let start = self.cur_token.span.clone();
        
        let value = self.cur_token.kind
            .as_string()
            .ok_or_else(|| Error::UnexpectedToken {
                span: self.cur_token.span.clone(),
                expected: vec![TokenKind::String(String::new())],
                found: self.cur_token.kind.clone(),
                location: Some(Location::Expression),
            })?;

        Ok(StringLiteral { span: self.span_with_start(&start), value: value.into() })
    }
}
#[cfg(test)]
mod tests {
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