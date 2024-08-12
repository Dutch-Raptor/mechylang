use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Error, Expression, Parser, Span, Token, TokenKind, trace};
use crate::error::ErrorKind;
use crate::parser::expressions::ExpressionSpanExt;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct InfixExpression {
    pub span: Span,
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

impl InfixOperator {
    pub fn is_assignment(&self) -> bool {
        matches!(self, InfixOperator::AssignEqual
        | InfixOperator::AssignPlus | InfixOperator::AssignMinus
        | InfixOperator::AssignAsterisk | InfixOperator::AssignSlash
        | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseAnd
        | InfixOperator::AssignBitwiseOr | InfixOperator::AssignBitwiseXor)
    }
    
    /// Returns the related operator to an assignment operator.
    /// For example, the `AssignPlus` operator is related to the `Plus` operator.
    /// 
    /// For non-assignment operators, returns `None`.
    pub fn assignment_related_operator(&self) -> Option<InfixOperator> {
        match self {
            InfixOperator::AssignPlus => Some(InfixOperator::Plus),
            InfixOperator::AssignMinus => Some(InfixOperator::Minus),
            InfixOperator::AssignAsterisk => Some(InfixOperator::Asterisk),
            InfixOperator::AssignSlash => Some(InfixOperator::Slash),
            InfixOperator::AssignPercent => Some(InfixOperator::Percent),
            InfixOperator::AssignBitwiseAnd => Some(InfixOperator::BitwiseAnd),
            InfixOperator::AssignBitwiseOr => Some(InfixOperator::BitwiseOr),
            InfixOperator::AssignBitwiseXor => Some(InfixOperator::BitwiseXor),
            _ => None,
        }
    }
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
            TokenKind::Plus | TokenKind::Minus | TokenKind::Divide
            | TokenKind::Multiply | TokenKind::Modulo | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual | TokenKind::CompareGreater | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess | TokenKind::CompareLessEqual

            // Open parenthesis for function calls
            | TokenKind::LeftParen
            // Open square bracket for array indexing
            | TokenKind::LeftSquare

            // logical operators
            | TokenKind::LogicalAnd | TokenKind::LogicalOr

            // bitwise operators
            | TokenKind::Ampersand | TokenKind::BitwiseOr | TokenKind::BitwiseXor
            | TokenKind::BitwiseLeftShift | TokenKind::BitwiseRightShift

            // Assignment operators
            | TokenKind::AssignEqual | TokenKind::AssignPlus | TokenKind::AssignMinus
            | TokenKind::AssignMultiply | TokenKind::AssignDivide | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseAnd | TokenKind::AssignBitwiseOr | TokenKind::AssignBitwiseXor

            // Range operator
            | TokenKind::RangeExclusive | TokenKind::RangeInclusive

            // Method expressions
            | TokenKind::Dot

            => true,
            _ => false,
        }
    }

    /// Parses an infix expression in Mechylang.
    ///
    /// This function takes the left-hand side of an expression and the current token,
    /// and parses it into a corresponding infix `Expression`. It handles various infix
    /// operators such as arithmetic, comparison, logical, bitwise, and assignment operators,
    /// as well as function calls, indexing, member access, and range expressions.
    ///
    /// # Parameters
    ///
    /// - `left`: The left-hand side `Expression` of the infix operation.
    ///
    /// # Returns
    ///
    /// Returns a `Result<Expression, Error>` where:
    /// - `Ok(Expression)` is returned if the current token is successfully parsed into an `Expression`.
    /// - `Err(Error)` is returned if the current token does not correspond to a valid infix operator
    ///   or if parsing fails.
    ///
    /// # Errors
    ///
    /// The function will return an error if:
    /// - The `TokenKind` does not have a registered infix function. This could occur if the
    ///   token type is unexpected or unsupported, resulting in an `ErrorKind::MissingInfix`
    ///   error with a descriptive message.
    pub(super) fn parse_infix(&mut self, left: Expression) -> Result<Expression, Error> {
        let _trace = trace!("parse_infix");
        match self.cur_token.kind {
            // **** infix operators ****
            // Arithmetic operators
            TokenKind::Plus | TokenKind::Minus | TokenKind::Divide
            | TokenKind::Multiply | TokenKind::Modulo

            // Comparison operators
            | TokenKind::CompareEqual | TokenKind::CompareNotEqual | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual | TokenKind::CompareLess | TokenKind::CompareLessEqual

            // Logical operators
            | TokenKind::LogicalAnd | TokenKind::LogicalOr

            // Bitwise operators
            | TokenKind::Ampersand | TokenKind::BitwiseOr | TokenKind::BitwiseXor
            | TokenKind::BitwiseLeftShift | TokenKind::BitwiseRightShift

            // Assignments
            | TokenKind::AssignEqual | TokenKind::AssignPlus | TokenKind::AssignMinus
            | TokenKind::AssignMultiply | TokenKind::AssignDivide | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseAnd | TokenKind::AssignBitwiseOr | TokenKind::AssignBitwiseXor

            => Ok(Expression::Infix(self.parse_infix_expression(left)?)),

            TokenKind::LeftParen => Ok(Expression::Call(self.parse_call_expression(left)?)),
            TokenKind::LeftSquare => Ok(Expression::Index(self.parse_index_expression(left)?)),
            TokenKind::Dot => Ok(Expression::Member(self.parse_member(left)?)),
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => self.parse_range_infix_expression(left),

            _ => Err(self.error_current(
                ErrorKind::MissingInfix,
                format!("No registered infix function for {:?}", self.cur_token.kind),
            )),
        }
    }


    pub(super) fn parse_infix_expression(&mut self, left: Expression) -> Result<InfixExpression, Error> {
        let _trace = trace!("parse_infix_expression");

        let operator = Self::parse_infix_operator(&self.cur_token)
            .ok_or(self.error_current(
                ErrorKind::MissingInfix,
                format!("expected infix operator, got {:?}", self.cur_token.kind),
            ))?;

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(InfixExpression {
            span: self.span_with_start(left.span().start.clone()),
            operator,
            left: Rc::new(left),
            right: Rc::new(right),
        })
    }

    fn parse_infix_operator(token: &Token) -> Option<InfixOperator> {
        Some(match token.kind {
            // Arithmetic operators
            TokenKind::Plus => InfixOperator::Plus,
            TokenKind::Minus => InfixOperator::Minus,
            TokenKind::Multiply => InfixOperator::Asterisk,
            TokenKind::Divide => InfixOperator::Slash,
            TokenKind::Modulo => InfixOperator::Percent,

            // Comparison operators
            TokenKind::CompareEqual => InfixOperator::CompareEqual,
            TokenKind::CompareNotEqual => InfixOperator::CompareNotEqual,
            TokenKind::CompareLess => InfixOperator::CompareLess,
            TokenKind::CompareGreater => InfixOperator::CompareGreater,
            TokenKind::CompareLessEqual => InfixOperator::CompareLessEqual,
            TokenKind::CompareGreaterEqual => InfixOperator::CompareGreaterEqual,

            // Bitwise operators
            TokenKind::BitwiseXor => InfixOperator::BitwiseXor,
            TokenKind::Ampersand => InfixOperator::BitwiseAnd,
            TokenKind::BitwiseOr => InfixOperator::BitwiseOr,
            TokenKind::BitwiseLeftShift => InfixOperator::BitwiseLeftShift,
            TokenKind::BitwiseRightShift => InfixOperator::BitwiseRightShift,

            // Logical operators
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
                return None;
            }
        })
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