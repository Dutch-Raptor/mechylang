use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::Parser;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Identifier {
    pub token: Token,
    pub value: Rc<str>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Into<Rc<str>> for Identifier {
    fn into(self) -> Rc<str> {
        self.value
    }
}
 impl Parser {

     pub(super) fn parse_identifier(&mut self) -> Result<Expression, Error> {
         let _trace = trace!("parse_identifier");
         let token = self.cur_token.clone();

         let literal = match token.kind {
             TokenKind::Identifier(ref literal) => literal.clone(),
             _ => {
                 return Err(self.error_current(
                     ErrorKind::InvalidIdentifier,
                     "Expected an identifier".to_string(),
                 ))
             }
         };

         Ok(Expression::Identifier(Identifier {
             token,
             value: literal.into(),
         }))
     }
 }

#[cfg(test)]
mod tests {
    use crate::lexer::tokens::TokenKind;
    use crate::parser::expressions::Expression;
    use crate::parser::expressions::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let statements = parse(input).unwrap();

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => {
                assert_eq!(expr.token.kind, TokenKind::Identifier("foobar".to_string()));
                match expr.expression {
                    Expression::Identifier(ref ident) => {
                        assert_eq!(ident.value, "foobar".into());
                        assert_eq!(
                            ident.token.kind,
                            TokenKind::Identifier("foobar".into())
                        );
                    }
                    _ => panic!("expected identifier expression"),
                };
            }
            _ => panic!("expected expression statement"),
        };
    }
}