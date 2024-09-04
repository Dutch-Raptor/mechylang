use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::parser::{Parser, Error, Result};
use crate::{trace, TokenKind, Span};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Identifier {
    pub span: Span,
    pub value: Rc<str>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<Identifier> for Rc<str> {
    fn from(ident: Identifier) -> Self {
        ident.value
    }
}
 impl Parser {

     pub(super) fn parse_identifier(&mut self) -> Result<Identifier> {
         let _trace = trace!("parse_identifier");

         let literal = match self.cur_token.kind {
             TokenKind::Identifier(ref literal) => literal.clone(),
             _ => {
                 return Err(Error::UnexpectedToken {
                     span: self.cur_token.span.clone(),
                     expected: vec![TokenKind::Identifier(String::new())],
                     found: self.cur_token.kind.clone(),
                 })
             }
         };

         Ok(Identifier {
             span: self.cur_token.span.clone(),
             value: literal.into(),
         })
     }
 }

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let statements = parse(input).unwrap();

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => {
                match expr.expression {
                    Expression::Identifier(ref ident) => {
                        assert_eq!(ident.value, "foobar".into());
                    }
                    _ => panic!("expected identifier expression"),
                };
            }
            _ => panic!("expected expression statement"),
        };
    }
}