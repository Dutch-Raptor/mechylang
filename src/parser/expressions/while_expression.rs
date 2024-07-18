use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::expressions::precedence::Precedence;
use crate::parser::Parser;
use crate::{Error, Token};
use crate::parser::expressions::block_expression::BlockExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct WhileExpression {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub body: BlockExpression,
    pub else_block: Option<BlockExpression>,
}

impl Display for WhileExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while {} {{\n{}\n}}", self.condition, self.body)
    }
}

impl Parser {

    /// Parses a while expression
    ///
    /// valid syntax:
    ///
    /// ```text
    /// while <condition> { <body> }
    pub(super) fn parse_while_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::LeftSquirly)?;

        let body = self.parse_block_expression()?;

        let else_block = if self.peek_token.kind == TokenKind::Else {
            self.next_token();
            self.expect_peek(TokenKind::LeftSquirly)?;
            Some(self.parse_block_expression()?)
        } else {
            None
        };

        Ok(Expression::While(WhileExpression {
            token,
            condition: Rc::new(condition),
            body,
            else_block,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::statements::Statement;
    use crate::parser::tests::parse;

    #[test]
    fn test_while_expression() {
        let input = "while true { 1; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::While(ref while_expr) => {
                    assert_eq!(while_expr.condition.to_string(), "true");
                    assert_eq!(while_expr.body.statements.len(), 1);
                }
                _ => panic!("expected while expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}
