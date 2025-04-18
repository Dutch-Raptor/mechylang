use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::parser::Parser;
use crate::{Expression, Span, TokenKind};
use crate::parser::expressions::{ExpressionSpanExt, Precedence};
use crate::parser::{Result};
use crate::parser::error::Location;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IndexExpression {
    pub span: Span,
    pub left_span: Span,
    pub index_span: Span,
    pub left: Rc<Expression>,
    pub index: Rc<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}
impl Parser<'_> {
    pub(super) fn parse_index_expression(&mut self, left: Expression) -> Result<IndexExpression> {
        let start = self.cur_token.span.clone();
        debug_assert!(self.is_cur_token(TokenKind::LeftSquare), "Expected current token to be `[`");

        self.next_token()?;

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RightSquare, Some(Location::Expression))?;

        Ok(IndexExpression {
            span: self.span_with_start(&start),
            left_span: left.span().clone(),
            index_span: index.span().clone(),
            left: Rc::new(left),
            index: Rc::new(index),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_index_expression() {
        let input = "myArray[1 + 1]";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Index(ref index) => {
                    assert_eq!(index.left.to_string(), "myArray");
                    assert_eq!(index.index.to_string(), "(1 + 1)");
                }
                _ => panic!("expected index expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}