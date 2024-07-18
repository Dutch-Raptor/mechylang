use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::Parser;
use crate::{Error, Token};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Rc<[Expression]>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let elements = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "[{}]", elements)
    }
}

impl Parser {

    pub(super) fn parse_array_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(TokenKind::RightSquare)?;

        Ok(Expression::ArrayLiteral(ArrayLiteral { token, elements: elements.into() }))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_array_literal_expression() {
        let input = "[1, 2 * 2, 3 + 3]";

        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::ArrayLiteral(ref literal) => {
                    assert_eq!(literal.elements.len(), 3);
                    assert_eq!(literal.elements[0].to_string(), "1");
                    assert_eq!(literal.elements[1].to_string(), "(2 * 2)");
                    assert_eq!(literal.elements[2].to_string(), "(3 + 3)");
                }
                _ => panic!("expected array literal expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}