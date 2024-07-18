use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::expressions::precedence::Precedence;
use crate::parser::Parser;
use crate::{Error, Token, trace};
use crate::parser::expressions::block_expression::BlockExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IfExpression {
    pub token: Token,
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

impl Parser {

    pub(super) fn parse_if_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_if_expression");
        let token = self.cur_token.clone();


        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::LeftSquirly)?;

        // parse_block_statement handles opening and closing braces
        let consequence = self.parse_block_expression()?;

        let mut alternative = None;

        if self.peek_token.kind == TokenKind::Else {
            self.next_token();

            self.expect_peek(TokenKind::LeftSquirly)?;

            alternative = Some(self.parse_block_expression()?);
        }

        Ok(Expression::If(IfExpression {
            token,
            condition: Rc::new(condition),
            consequence,
            alternative,
        }))
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