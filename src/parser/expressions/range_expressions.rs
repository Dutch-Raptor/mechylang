use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::Parser;
use crate::{Error, Token};
use crate::errors::ErrorKind;
use crate::parser::expressions::precedence::Precedence;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct RangeExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
    pub inclusive: bool,
}

impl Display for RangeExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}{}{})",
            self.left,
            if self.inclusive { "..=" } else { ".." },
            self.right
        )
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct RangeToExpression {
    pub token: Token,
    pub right: Rc<Expression>,
    pub inclusive: bool,
}

impl Display for RangeToExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(..{})", self.right)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct RangeFromExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub inclusive: bool,
}

impl Display for RangeFromExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}{}..)",
            self.left,
            if self.inclusive { "=" } else { "" }
        )
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct RangeFullExpression {
    pub token: Token,
}

impl Display for RangeFullExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(..)")
    }
}

impl Parser {

    pub(super) fn parse_range_infix_expression(&mut self, token_kind: TokenKind, left: Expression) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        let precedence = self.cur_precedence();
        let inclusive = match token_kind {
            TokenKind::RangeInclusive => true,
            TokenKind::RangeExclusive => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a range operator, got {:?}", self.cur_token),
                ))
            }
        };

        // check if we have a RangeFrom expression
        // RangeFrom expressions are bounded by either square brackets or a parenthesis
        //
        // ```text
        // (1..) // RangeFrom
        // [1..] // RangeFrom
        if self.peek_token.kind == TokenKind::RightParen || self.peek_token.kind == TokenKind::RightSquare {
            return Ok(Expression::RangeFrom(RangeFromExpression {
                token,
                left: Rc::new(left),
                inclusive,
            }));
        }

        self.next_token();


        // if the next token is not a closing parenthesis or square bracket, we assume that we have a Range expression

        let right = self.parse_expression(precedence)?;

        Ok(Expression::Range(RangeExpression {
            token,
            left: Rc::new(left),
            right: Rc::new(right),
            inclusive,
        }))
    }
    /// Parses a RangeTo expression
    /// e.g. `..5` or `..=5`
    pub(super) fn parse_range_prefix_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        // check if we are dealing with a FullRange expression
        // e.g. `..`
        // if so, we would have a `..` token followed by a ')' or a ']' token
        if self.peek_token.kind == TokenKind::RightParen || self.peek_token.kind == TokenKind::RightSquare {
            return Ok(Expression::RangeFull(RangeFullExpression { token }));
        }

        self.next_token();


        let right = self.parse_expression(Precedence::Prefix)?;

        let inclusive = match token.kind {
            TokenKind::RangeInclusive => true,
            TokenKind::RangeExclusive => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a range operator, got {:?}", self.cur_token),
                ))
            }
        };

        Ok(Expression::RangeTo(RangeToExpression {
            token,
            right: Rc::new(right),
            inclusive,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_range_expression() {
        let input = "1..5; 1..=5;";

        let statements = parse(input).unwrap();
        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Range(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, false);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Range(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, true);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_from_expressions() {
        let input = "(1..); (1..=);";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeFrom(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.inclusive, false);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeFrom(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.inclusive, true);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_to_expressions() {
        let input = "..5; ..=5;";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeTo(ref range) => {
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, false);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeTo(ref range) => {
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, true);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_full_expression() {
        let input = "(..); array[..];";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeFull(_) => {}
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Index(ref index) => {
                    assert_eq!(index.left.to_string(), "array");
                    assert_eq!(index.index.to_string(), "(..)");
                }
                _ => panic!("expected index expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}