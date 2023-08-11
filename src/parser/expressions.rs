/// # Parsing Expressions
///
/// This module contains the parsing logic for expressions.
///
/// ## Expressions
///
/// An expression is a combination of values, variables, operators, and functions that are interpreted to produce another value.
/// For example, `1 + 2` is an expression that evaluates to `3`.
/// Expressions can be as simple as a single value, or as complex as a function call with multiple arguments.
///
///
/// This file contains the following:
/// - `Expression` enum
/// - All expression variants
/// - implementations for `Display` for all expression variants
/// - implementations for the parse functions for all expression variants for the parser
use std::fmt::{self, Display, Formatter};

use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::parser::Precedence;

use super::parser::BlockStatement;

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanLiteral),
    If(IfExpression),
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
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct FloatLiteral {
    pub token: Token,
    pub value: f64,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "if ({}) {{\n{}\n}}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {{\n{}\n}}", alt)?;
        }
        Ok(())
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

pub trait PrecedenceTrait {
    fn precedence(&self) -> Option<Precedence>;
}

impl PrecedenceTrait for TokenKind {
    fn precedence(&self) -> Option<Precedence> {
        let presedence = match self {
            TokenKind::CompareEqual | TokenKind::CompareNotEqual => Precedence::Equals,
            TokenKind::CompareLess
            | TokenKind::CompareLessEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual => Precedence::LessGreater,
            TokenKind::Asterisk | TokenKind::Slash | TokenKind::Percent => Precedence::Product,
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::EOF => Precedence::Lowest,
            TokenKind::RightParen => Precedence::Lowest,

            // some tokens are not used in expressions
            //
            // But can still show up as a peeked token
            TokenKind::LeftSquirly => Precedence::Lowest,
            TokenKind::RightSquirly => Precedence::Lowest,
            _ => {
                eprintln!("No presedence implemented for token: {:?}", self);
                return None;
            }
        };

        Some(presedence)
    }
}

impl PrecedenceTrait for Token {
    fn precedence(&self) -> Option<Precedence> {
        self.kind.precedence()
    }
}
