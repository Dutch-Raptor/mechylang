use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::lexer::tokens::TokenKind;
use crate::parser::expressions::Expression;
use crate::parser::Parser;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub operator: InfixOperator,
    pub right: Rc<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum InfixOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    CompareEqual,
    CompareNotEqual,
    CompareLess,
    CompareLessEqual,
    CompareGreater,
    CompareGreaterEqual,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    BitwiseLeftShift,
    BitwiseRightShift,

    AssignEqual,
    AssignPlus,
    AssignMinus,
    AssignAsterisk,
    AssignSlash,
    AssignPercent,
    AssignBitwiseAnd,
    AssignBitwiseOr,
    AssignBitwiseXor,
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Asterisk => write!(f, "*"),
            InfixOperator::Slash => write!(f, "/"),
            InfixOperator::Percent => write!(f, "%"),
            InfixOperator::CompareEqual => write!(f, "=="),
            InfixOperator::CompareNotEqual => write!(f, "!="),
            InfixOperator::CompareLess => write!(f, "<"),
            InfixOperator::CompareLessEqual => write!(f, "<="),
            InfixOperator::CompareGreater => write!(f, ">"),
            InfixOperator::CompareGreaterEqual => write!(f, ">="),
            InfixOperator::LogicalAnd => write!(f, "&&"),
            InfixOperator::LogicalOr => write!(f, "||"),
            InfixOperator::BitwiseAnd => write!(f, "&"),
            InfixOperator::BitwiseOr => write!(f, "|"),
            InfixOperator::BitwiseXor => write!(f, "^"),
            InfixOperator::BitwiseLeftShift => write!(f, "<<"),
            InfixOperator::BitwiseRightShift => write!(f, ">>"),
            InfixOperator::AssignEqual => write!(f, "="),
            InfixOperator::AssignPlus => write!(f, "+="),
            InfixOperator::AssignMinus => write!(f, "-="),
            InfixOperator::AssignAsterisk => write!(f, "*="),
            InfixOperator::AssignSlash => write!(f, "/="),
            InfixOperator::AssignPercent => write!(f, "%="),
            InfixOperator::AssignBitwiseAnd => write!(f, "&="),
            InfixOperator::AssignBitwiseOr => write!(f, "|="),
            InfixOperator::AssignBitwiseXor => write!(f, "^="),
        }
    }
}


impl Parser {
    pub(super) fn has_infix(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Divide
            | TokenKind::Multiply
            | TokenKind::Modulo
            | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess
            | TokenKind::CompareLessEqual
            // Open parenthesis for function calls
            | TokenKind::LeftParen
            // Open square bracket for array indexing
            | TokenKind::LeftSquare

            // logical operators
            | TokenKind::LogicalAnd
            | TokenKind::LogicalOr

            // bitwise operators
            | TokenKind::Ampersand
            | TokenKind::BitwiseOr
            | TokenKind::BitwiseXor
            | TokenKind::BitwiseLeftShift
            | TokenKind::BitwiseRightShift

            // Assignment operators
            | TokenKind::AssignEqual
            | TokenKind::AssignPlus
            | TokenKind::AssignMinus
            | TokenKind::AssignMultiply
            | TokenKind::AssignDivide
            | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseAnd
            | TokenKind::AssignBitwiseOr
            | TokenKind::AssignBitwiseXor

            // Range operator
            | TokenKind::RangeExclusive
            | TokenKind::RangeInclusive

            // Method expressions
            | TokenKind::Dot

            => true,
            _ => false,
        }
    }

    pub(super) fn parse_infix(&mut self, token: TokenKind, left: Expression) -> Result<Expression, Error> {
        let _trace = trace!("parse_infix");
        match token {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Divide
            | TokenKind::Multiply | TokenKind::Modulo | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual | TokenKind::CompareGreater | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess | TokenKind::CompareLessEqual | TokenKind::LogicalAnd
            | TokenKind::LogicalOr | TokenKind::Ampersand | TokenKind::BitwiseOr
            | TokenKind::BitwiseXor | TokenKind::BitwiseLeftShift | TokenKind::BitwiseRightShift

            // Assignments
            | TokenKind::AssignEqual | TokenKind::AssignPlus | TokenKind::AssignMinus
            | TokenKind::AssignMultiply | TokenKind::AssignDivide | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseAnd | TokenKind::AssignBitwiseOr | TokenKind::AssignBitwiseXor

            => self.parse_infix_expression(left),
            TokenKind::LeftParen => self.parse_call_expression(left),
            TokenKind::LeftSquare => self.parse_index_expression(left),
            TokenKind::Dot => self.parse_member(left),
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => self.parse_range_infix_expression(token, left),
            _ => Err(self.error_current(
                ErrorKind::MissingInfix,
                format!("No registered infix function for {:?}", token),
            )),
        }
    }


    pub(super) fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, Error> {
        let _trace = trace!("parse_infix_expression");
        let token = self.cur_token.clone();

        let operator = match token.kind {
            TokenKind::Plus => InfixOperator::Plus,
            TokenKind::Minus => InfixOperator::Minus,
            TokenKind::Multiply => InfixOperator::Asterisk,
            TokenKind::Divide => InfixOperator::Slash,
            TokenKind::Modulo => InfixOperator::Percent,
            TokenKind::CompareEqual => InfixOperator::CompareEqual,
            TokenKind::CompareNotEqual => InfixOperator::CompareNotEqual,
            TokenKind::CompareLess => InfixOperator::CompareLess,
            TokenKind::CompareGreater => InfixOperator::CompareGreater,
            TokenKind::CompareLessEqual => InfixOperator::CompareLessEqual,
            TokenKind::CompareGreaterEqual => InfixOperator::CompareGreaterEqual,

            TokenKind::BitwiseXor => InfixOperator::BitwiseXor,
            TokenKind::Ampersand => InfixOperator::BitwiseAnd,
            TokenKind::BitwiseOr => InfixOperator::BitwiseOr,
            TokenKind::BitwiseLeftShift => InfixOperator::BitwiseLeftShift,
            TokenKind::BitwiseRightShift => InfixOperator::BitwiseRightShift,

            TokenKind::LogicalAnd => InfixOperator::LogicalAnd,
            TokenKind::LogicalOr => InfixOperator::LogicalOr,

            // Assignment
            TokenKind::AssignEqual => InfixOperator::AssignEqual,
            TokenKind::AssignPlus => InfixOperator::AssignPlus,
            TokenKind::AssignMinus => InfixOperator::AssignMinus,
            TokenKind::AssignMultiply => InfixOperator::AssignAsterisk,
            TokenKind::AssignDivide => InfixOperator::AssignSlash,
            TokenKind::AssignModulo => InfixOperator::AssignPercent,
            TokenKind::AssignBitwiseXor => InfixOperator::AssignBitwiseXor,
            TokenKind::AssignBitwiseAnd => InfixOperator::AssignBitwiseAnd,
            TokenKind::AssignBitwiseOr => InfixOperator::AssignBitwiseOr,

            _ => {
                return Err(self.error_current(
                    ErrorKind::MissingInfix,
                    format!("expected infix operator, got {:?}", token),
                ))
            }
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left: Rc::new(left),
            right: Rc::new(right),
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::expressions::infix_expression::InfixOperator;
    use crate::parser::tests::parse;
    use crate::parser::statements::Statement;

    #[test]
    fn test_mutating_values() {
        let input = "let a = 1; a = 2;";

        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 2);

        let stmt = &statements[1];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignEqual);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "2");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_special_mutation_operators() {
        let input = "let a = 1; a += 2; a -= 3; a *= 4; a /= 5; a %= 6;";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 6);

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignPlus);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "2");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[2] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignMinus);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "3");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[3] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignAsterisk);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "4");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[4] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignSlash);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "5");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[5] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignPercent);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "6");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}