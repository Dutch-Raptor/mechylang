//! # Parsing Expressions
//!
//! This module contains the parsing logic for expressions.
//!
//! ## Expressions
//!
//! An expression is a combination of values, variables, operators, and functions that are interpreted to produce another value.
//! For example, `1 + 2` is an expression that evaluates to `3`.
//! Expressions can be as simple as a single value, or as complex as a function call with multiple arguments.
//!
//!
//! This file contains the following:
//! - `Expression` enum
//! - All expression variants
//! - implementations for `Display` for all expression variants
//! - implementations for the parse functions for all expression variants for the parser
mod precedence;
mod identifier;
mod number_expressions;
mod boolean_literal;
mod string_literal;
mod prefix_expression;
mod infix_expression;
mod if_expression;
mod function_literal;
mod call_expression;
mod array_literal;
mod index_expression;
mod range_expressions;
mod for_expression;
mod while_expression;
mod member_expression;
mod struct_literal;
mod block_expression;

use std::fmt::{self, Display, Formatter};

use serde::Serialize;

pub use precedence::Precedence;
pub use array_literal::ArrayLiteral;
pub use boolean_literal::BooleanLiteral;
pub use call_expression::CallExpression;
pub use number_expressions::{FloatLiteral, IntegerLiteral};
pub use for_expression::ForExpression;
pub use function_literal::FunctionLiteral;
pub use identifier::Identifier;
pub use if_expression::IfExpression;
pub use index_expression::IndexExpression;
pub use infix_expression::{InfixExpression, InfixOperator};
pub use member_expression::MemberExpression;
pub use prefix_expression::{PrefixExpression, PrefixOperator};
pub use range_expressions::{RangeExpression, RangeFromExpression, RangeFullExpression, RangeToExpression};
pub use string_literal::StringLiteral;
pub use struct_literal::StructLiteral;
pub use while_expression::WhileExpression;
pub use block_expression::BlockExpression;
use crate::parser::Parser;
use crate::error::ErrorKind;
use crate::{Error, Token, TokenKind, trace};


/// Represents an expression in Mechylang.
///
/// An `Expression` can be a variety of constructs such as literals, identifiers, operations, or
/// more complex expressions like function calls and conditionals. This enum defines all the
/// possible types of expressions that can be encountered while parsing Mechylang code.
///
/// # Variants
///
/// * `Identifier(Identifier)` - Represents a variable or function name.
/// * `IntegerLiteral(IntegerLiteral)` - Represents an integer literal.
/// * `FloatLiteral(FloatLiteral)` - Represents a floating-point literal.
/// * `Prefix(PrefixExpression)` - Represents a prefix operation (e.g., `!`, `-`).
/// * `Infix(InfixExpression)` - Represents an infix operation (e.g., `+`, `-`, `*`, `/`).
/// * `Boolean(BooleanLiteral)` - Represents a boolean literal (`true` or `false`).
/// * `If(IfExpression)` - Represents an if-else conditional expression.
/// * `Function(FunctionLiteral)` - Represents a function literal (lambda or named function).
/// * `Call(CallExpression)` - Represents a function call expression.
/// * `Block(BlockExpression)` - Represents a block of statements.
/// * `StringLiteral(StringLiteral)` - Represents a string literal.
/// * `ArrayLiteral(ArrayLiteral)` - Represents an array literal.
/// * `Index(IndexExpression)` - Represents an indexing operation (e.g., accessing an array element).
/// * `Range(RangeExpression)` - Represents a range expression (e.g., `1..10`).
/// * `RangeTo(RangeToExpression)` - Represents a range-to expression (e.g., `..10`).
/// * `RangeFrom(RangeFromExpression)` - Represents a range-from expression (e.g., `1..`).
/// * `RangeFull(RangeFullExpression)` - Represents a full range expression (e.g., `..`).
/// * `For(ForExpression)` - Represents a for loop expression.
/// * `While(WhileExpression)` - Represents a while loop expression.
/// * `Member(MemberExpression)` - Represents a member access expression (e.g., accessing a field of a struct).
/// * `Unit(Token)` - Represents a unit value, often used for expressions that evaluate to no value (`()`).
/// * `StructLiteral(StructLiteral)` - Represents a struct literal (instantiation of a struct).
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Expression {
    /// Represents a variable or function name.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// foobar;
    /// # "#);
    /// ```
    Identifier(Identifier),
    /// Represents an integer literal.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// 2
    /// # "#);
    /// ```
    IntegerLiteral(IntegerLiteral),
    /// Represents a floating-point literal.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// 2.5
    /// # "#);
    /// ```
    FloatLiteral(FloatLiteral),
    /// Represents a prefix operation (e.g., `!`, `-`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// !x;
    /// # "#);
    /// ```
    Prefix(PrefixExpression),
    /// Represents an infix operation (e.g., `+`, `-`, `*`, `/`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// 2 + 3;
    /// # "#);
    /// ```
    Infix(InfixExpression),
    /// Represents a boolean literal (`true` or `false`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// true;
    /// # "#);
    /// ```
    Boolean(BooleanLiteral),
    /// Represents an if-else conditional expression. The `else` branch is optional.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// if x {
    ///     2
    /// } else {
    ///     3
    /// }
    /// # "#);
    /// ```
    If(IfExpression),
    /// Represents an anonymous function literal.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// fn(x, y) {
    ///     x + y
    /// }
    /// # "#);
    /// ```
    Function(FunctionLiteral),
    /// Represents a function call expression.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// add(1, 2);
    /// # "#);
    /// ```
    Call(CallExpression),
    /// Represents a block of statements.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// {
    ///     x = 1;
    ///     y = 2;
    /// }
    /// # "#);
    /// ```
    Block(BlockExpression),
    /// Represents a string literal.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// "Hello, world!";
    /// # "#);
    /// ```
    StringLiteral(StringLiteral),
    /// Represents an array literal.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// [1, 2, 3];
    /// # "#);
    /// ```
    ArrayLiteral(ArrayLiteral),
    /// Represents an indexing operation (e.g., accessing an array element).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// [1, 2, 3][1];
    /// # "#);
    /// ```
    Index(IndexExpression),
    /// Represents a range expression (e.g., `1..10`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// 1..10;
    /// # "#);
    /// ```
    Range(RangeExpression),
    /// Represents a range-to expression (e.g., `..10`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// ..10;
    /// # "#);
    /// ```
    RangeTo(RangeToExpression),
    /// Represents a range-from expression (e.g., `1..`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// (1..);
    /// # "#);
    /// ```
    RangeFrom(RangeFromExpression),
    /// Represents a full range expression (e.g., `..`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// (..);
    /// # "#);
    /// ```
    RangeFull(RangeFullExpression),
    /// Represents a for loop expression.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// for x in 1..10 {
    ///     x * 2
    /// }
    /// # "#);
    /// ```
    For(ForExpression),
    /// Represents a while loop expression.
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// while x < 10 {
    ///     x = x + 1;
    /// }
    /// # "#);
    /// ```
    While(WhileExpression),
    /// Represents a member access expression (e.g., accessing a field of a struct).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// foo.bar;
    /// # "#);
    /// ```
    Member(MemberExpression),
    /// Represents a unit value, often used for expressions that evaluate to no value (`()`).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// ();
    /// # "#);
    /// ```
    Unit(Token),
    /// Represents a struct literal (instantiation of an anonymous struct).
    /// ```
    /// # use mechylang::test_utils::test_parse_ok;
    /// # test_parse_ok(r#"
    /// struct { x: 1, y: 2 };
    /// # "#);
    /// ```
    StructLiteral(StructLiteral),
}

pub trait ExpressionToken {
    fn token(&self) -> &Token;
}

impl ExpressionToken for Expression {
    fn token(&self) -> &Token {
        match self {
            Expression::Identifier(ident) => &ident.token,
            Expression::IntegerLiteral(lit) => &lit.token,
            Expression::FloatLiteral(lit) => &lit.token,
            Expression::StructLiteral(lit) => &lit.token,
            Expression::Prefix(expr) => &expr.token,
            Expression::Infix(expr) => &expr.token,
            Expression::Boolean(boolean) => &boolean.token,
            Expression::If(if_expr) => &if_expr.token,
            Expression::Function(func) => &func.token,
            Expression::Call(call) => &call.token,
            Expression::Block(block) => &block.token,
            Expression::StringLiteral(lit) => &lit.token,
            Expression::ArrayLiteral(lit) => &lit.token,
            Expression::Index(index) => &index.token,
            Expression::Range(range) => &range.token,
            Expression::RangeTo(range) => &range.token,
            Expression::RangeFrom(range) => &range.token,
            Expression::RangeFull(range) => &range.token,
            Expression::For(for_expr) => &for_expr.token,
            Expression::While(while_expr) => &while_expr.token,
            Expression::Member(member) => &member.token,
            Expression::Unit(token) => token,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(lit) => write!(f, "{}", lit),
            Expression::FloatLiteral(lit) => write!(f, "{}", lit),
            Expression::Prefix(expr) => write!(f, "{}", expr),
            Expression::Infix(expr) => write!(f, "{}", expr),
            Expression::Boolean(boolean) => write!(f, "{}", boolean),
            Expression::If(if_expr) => write!(f, "{}", if_expr),
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::Block(block) => write!(f, "{}", block),
            Expression::StringLiteral(lit) => write!(f, "{}", lit),
            Expression::ArrayLiteral(lit) => write!(f, "{}", lit),
            Expression::Index(index) => write!(f, "{}", index),
            Expression::Range(range) => write!(f, "{}", range),
            Expression::RangeTo(range) => write!(f, "{}", range),
            Expression::RangeFrom(range) => write!(f, "{}", range),
            Expression::RangeFull(range) => write!(f, "{}", range),
            Expression::For(for_expr) => write!(f, "{}", for_expr),
            Expression::While(while_expr) => write!(f, "{}", while_expr),
            Expression::Member(member) => write!(f, "{}", member),
            Expression::Unit(_) => write!(f, "()"),
            Expression::StructLiteral(lit) => write!(f, "{}", lit),
        }
    }
}

impl Parser {
    /// Parses an expression in Mechylang with the given precedence.
    ///
    /// This method parses an expression from the current token, handling different levels of precedence. It supports
    /// prefix and infix operators, and processes the expression according to the provided precedence level. The function
    /// starts by parsing a prefix expression, then continues to parse infix expressions based on the precedence of operators.
    ///
    /// # Arguments
    ///
    /// * `precedence` - The precedence level to determine the order of operations for infix expressions. Higher precedence
    ///   values mean that the operator binds more tightly.
    ///
    /// # Returns
    ///
    /// Returns a `Result` which is:
    /// * `Ok(Expression)` containing the parsed expression if successful. The expression may include prefix and infix
    ///   operations, and is structured according to the precedence rules.
    /// * `Err(Error)` if there was an error during parsing. This includes cases where the current token does not have a
    ///   valid prefix operator or where an unexpected token is encountered.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// * The current token does not have a valid prefix operator, which indicates a parsing issue or invalid expression.
    /// * An infix operator is expected but not found, or the precedence of operators is not respected.
    ///
    /// # Notes
    ///
    /// The `parse_expression` function assumes that the expression parsing is initiated with a valid prefix operator
    /// and then proceeds to handle infix operators based on their precedence. The function uses the provided precedence
    /// level to correctly parse and associate operations, ensuring that expressions are parsed according to Mechylang's
    /// operator precedence rules.
    pub(crate) fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let _trace = trace!("parse_expression");
        let token = self.cur_token.clone();

        if !self.has_prefix(&token.kind) {
            return Err(self.error_current_with_context(
                ErrorKind::MissingPrefix,
                format!("Expected a value, got {:?}", token.kind),
                "parse_expression".to_string(),
            ));
        }

        let mut left_exp = self.parse_prefix()?;

        while !self.is_peek_token(TokenKind::Semicolon) && precedence < self.peek_precedence() {
            let peek_token_kind = self.peek_token.kind.clone();

            if !self.has_infix(&peek_token_kind) {
                return Ok(left_exp);
            }

            self.next_token();

            left_exp = self.parse_infix(left_exp)?;
        }

        Ok(left_exp)
    }


    pub(super) fn parse_grouped_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_grouped_expression");
        debug_assert!(self.is_cur_token(TokenKind::LeftParen), "Expected current token to be `(`");
        self.next_token();

        if self.cur_token.kind == TokenKind::RightParen {
            return Ok(Expression::Unit(
                self.cur_token.clone()
            ));
        }

        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RightParen)?;

        Ok(expression)
    }

    pub(super) fn parse_expression_list(&mut self, end: TokenKind) -> Result<Vec<Expression>, Error> {
        let mut arguments = Vec::new();

        if self.peek_token.kind == end {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();

        arguments.push(match self.parse_expression(Precedence::Lowest) {
            Err(Error { kind: ErrorKind::MissingPrefix, .. }) => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an expression or a {:?}, got {:?} instead", end, self.cur_token.kind),
                ))
            }
            Err(e) => return Err(e),
            Ok(e) => e,
        });

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();

            // allow trailing commas
            if self.peek_token.kind == end {
                break;
            }

            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(arguments)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::{Expression, InfixExpression, InfixOperator, Precedence, PrefixExpression};
    use crate::{Parser, TokenKind};

    #[test]
    fn test_parse_expression_with_prefix_and_infix() {
        let source_code = r#"
        -x + y * z
        "#;
        let mut parser = Parser::from_source(source_code);

        let result = parser.parse_expression(Precedence::Lowest);
        assert!(result.is_ok());

        if let Ok(Expression::Infix(InfixExpression { left, operator, right, .. })) = result {
            // Check the infix expression
            if let Expression::Prefix(
                PrefixExpression {
                    token: ref left_token,
                    right: ref expr,
                    ..
                }
            ) = *left {
                assert_eq!(left_token.kind, TokenKind::Minus);
                assert!(matches!(expr.as_ref(), Expression::Identifier(_)));
            } else {
                panic!("Expected prefix expression for left operand");
            }

            assert_eq!(operator, InfixOperator::Plus);

            if let Expression::Infix(
                InfixExpression {
                    left: ref right_left,
                    operator: ref right_op,
                    right: ref right_right,
                    ..
                }
            ) = *right {
                assert!(matches!(right_left.as_ref(), Expression::Identifier(_)));
                assert_eq!(right_op, &InfixOperator::Asterisk);
                assert!(matches!(right_right.as_ref(), Expression::Identifier(_)));
            } else {
                panic!("Expected infix expression for right operand");
            }
        } else {
            panic!("Expected infix expression");
        }
    }
    
    #[test]
    fn test_parse_expression_with_parentheses() {
        let source_code = r#"
        (x + y) * z
        "#;
        let mut parser = Parser::from_source(source_code);
        
        let result = parser.parse_expression(Precedence::Lowest);
        assert!(result.is_ok());
        
        if let Ok(Expression::Infix(InfixExpression { left, operator, right, .. })) = result {
            if let Expression::Infix(
                InfixExpression { 
                    left: ref inner_left, 
                    operator: ref inner_op, 
                    right: ref inner_right, .. }
            ) = *left {
                assert!(matches!(inner_left.as_ref(), Expression::Identifier(_)));
                assert_eq!(inner_op, &InfixOperator::Plus);
                assert!(matches!(inner_right.as_ref(), Expression::Identifier(_)));
            } else {
                panic!("Expected infix expression within parentheses");
            }
            
            assert_eq!(operator, InfixOperator::Asterisk);
            
            assert!(matches!(right.as_ref(), Expression::Identifier(_)));
        } else {
            panic!("Expected infix expression");
        }
    }
    
    #[test]
    fn test_parse_expression_with_invalid_token() {
        let source_code = r#"
        x ++ y
        "#;
        let mut parser = Parser::from_source(source_code);
        
        let result = parser.parse_expression(Precedence::Lowest);
        
        assert!(result.is_err(), "Expected parsing error for invalid token");
    }
}















