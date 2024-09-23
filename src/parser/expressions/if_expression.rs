use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{trace, TokenKind, Expression, Parser, Span};
use crate::parser::expressions::{BlockExpression, Precedence};
use crate::parser::{Result};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IfExpression {
    pub span: Span,
    pub condition: Rc<Expression>,
    pub consequence: BlockExpression,
    pub alternative: Option<BlockExpression>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {}", alt)?;
        }
        Ok(())
    }
}

impl<'a> Parser<'a> {

    pub(super) fn parse_if_expression(&mut self) -> Result<IfExpression> {
        let _trace = trace!("parse_if_expression");
        debug_assert!(self.is_cur_token(TokenKind::If), "Expected current token to be `if`");
        let start = self.cur_token.span.clone();

        self.next_token()?;
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::LeftSquirly)?;

        let consequence = self.parse_block_expression()?;
        debug_assert!(self.is_cur_token(TokenKind::RightSquirly), "Expected current token to be `}}` after parse_block_expression call");

        let alternative = self.parse_else_block()?;

        Ok(IfExpression {
            span: self.span_with_start(start),
            condition: Rc::new(condition),
            consequence,
            alternative,
        })
    }


    pub(super) fn parse_else_block(&mut self) -> Result<Option<BlockExpression>> {
        let else_block = if self.peek_token.kind == TokenKind::Else {
            self.next_token()?;
            self.expect_peek(TokenKind::LeftSquirly)?;
            Some(self.parse_block_expression()?)
        } else {
            None
        };
        Ok(else_block)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::statements::Statement;
    use crate::parser::tests::parse;

    #[test]
    fn test_if_expression() {
        let input = "if x < y { x; }";


        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::If(ref if_expr) => {
                    assert_eq!(if_expr.condition.to_string(), "(x < y)");
                    assert_eq!(if_expr.consequence.to_string().split_whitespace().collect::<Vec<&str>>().join(" "), "{ x; }");
                    assert_eq!(if_expr.alternative.is_none(), true);
                }
                _ => panic!("expected if expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}