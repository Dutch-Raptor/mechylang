use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::parser::expressions::Expression;
use crate::parser::expressions::identifier::Identifier;
use crate::parser::expressions::precedence::Precedence;
use crate::parser::Parser;
use crate::{Error, Span, TokenKind, trace};
use crate::parser::expressions::block_expression::BlockExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ForExpression {
    pub span: Span,
    pub iterator: Identifier,
    pub iterable: Rc<Expression>,
    pub body: BlockExpression,
    pub index: Option<Identifier>,
    pub else_block: Option<BlockExpression>,
}

impl Display for ForExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "for {} in {} {}",
            match &self.index {
                Some(index) => format!("({}, {})", index, self.iterator),
                None => format!("{}", self.iterator),
            },
            self.iterable,
            self.body
        )
    }
}

impl Parser {

    /// Parses a for expression
    ///
    /// valid syntax:
    /// ```text
    /// for <iterator> in <iterable> { <body> }
    /// for (<index>, <iterator>) in <iterable> { <body> }
    /// ```
    pub(super) fn parse_for_expression(&mut self) -> Result<ForExpression, Error> {
        let _trace = trace!("parse_for_expression");
        let start = self.cur_token.span.start.clone();

        self.next_token();

        // check if we have a for expression with an index
        let index = match self.cur_token.kind {
            TokenKind::LeftParen => {
                self.next_token();
                let index_ident = self.parse_identifier()?;
                
                self.expect_peek(TokenKind::Comma)?;
                self.next_token();
                Some(index_ident)
            }
            _ => None,
        };

        let iterator = self.parse_identifier()?;

        if index.is_some() {
            self.expect_peek(TokenKind::RightParen)?;
        }

        self.expect_peek(TokenKind::In)?;
        self.next_token();

        let iterable = self.parse_expression(Precedence::Lowest)?;

        // set current token to the '{' token
        self.next_token();

        let body = self.parse_block_expression()?;

        let else_block = self.parse_else_block()?;

        Ok(ForExpression {
            span: self.span_with_start(start),
            iterator,
            iterable: Rc::new(iterable),
            body,
            index,
            else_block,
        })
    }
}


#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_for_loop() {
        let input = "for i in 1..5 { i; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::For(ref for_loop) => {
                    assert_eq!(for_loop.iterator.to_string(), "i");
                    assert_eq!(for_loop.iterable.to_string(), "(1..5)");
                    assert_eq!(for_loop.body.statements.len(), 1);
                }
                _ => panic!("expected for loop expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_for_loop_with_index() {
        let input = "for (i, j) in 1..5 { i; j; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::For(ref for_loop) => {
                    println!("{:#?}", for_loop);
                    assert_eq!(for_loop.iterator.to_string(), "j");
                    assert_eq!(for_loop.iterable.to_string(), "(1..5)");
                    assert_eq!(for_loop.body.statements.len(), 2);

                    match for_loop.index {
                        Some(ref index) => {
                            assert_eq!(index.to_string(), "i");
                        }
                        None => panic!("expected index"),
                    }
                }
                _ => panic!("expected for loop expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}
