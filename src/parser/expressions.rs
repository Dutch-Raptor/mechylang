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
use std::rc::Rc;

use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::parser::Precedence;

use super::parser::BlockStatement;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanLiteral),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Block(BlockStatement),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    Index(IndexExpression),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: Rc<str>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Into<Rc<str>> for Identifier {
    fn into(self) -> Rc<str> {
        self.value
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteral {
    pub token: Token,
    pub value: f64,
}

impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: Rc<str>,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: PrefixOperator,
    pub right: Rc<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PrefixOperator::Bang => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub operator: InfixOperator,
    pub right: Rc<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Rc<Expression>,
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
            TokenKind::LeftSquare => Precedence::Index,
            TokenKind::CompareEqual | TokenKind::CompareNotEqual => Precedence::Equals,

            TokenKind::CompareLess
            | TokenKind::CompareLessEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual => Precedence::LessGreater,

            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => Precedence::Product,

            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,

            TokenKind::LogicalAnd => Precedence::LogicalAnd,
            TokenKind::LogicalOr => Precedence::LogicalOr,

            TokenKind::BitwiseAnd => Precedence::BitwiseAnd,
            TokenKind::BitwiseOr => Precedence::BitwiseOr,
            TokenKind::BitwiseXor => Precedence::BitwiseXor,

            TokenKind::BitwiseLeftShift | TokenKind::BitwiseRightShift => Precedence::BitShift,

            TokenKind::EOF => Precedence::Lowest,
            TokenKind::RightParen => Precedence::Lowest,

            TokenKind::LeftParen => Precedence::Call,

            TokenKind::AssignEqual
            | TokenKind::AssignPlus
            | TokenKind::AssignMinus
            | TokenKind::AssignMultiply
            | TokenKind::AssignDivide
            | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseOr
            | TokenKind::AssignBitwiseAnd
            | TokenKind::AssignBitwiseXor => Precedence::Assign,

            // some tokens are not used in expressions
            //
            // But can still show up as a peeked token
            TokenKind::LeftSquirly => Precedence::Lowest,
            TokenKind::RightSquirly => Precedence::Lowest,
            TokenKind::Comma => Precedence::Lowest,
            TokenKind::Return => Precedence::Lowest,
            TokenKind::RightSquare => Precedence::Lowest,
            TokenKind::Identifier(_) => Precedence::Lowest,
            TokenKind::Let => Precedence::Lowest,
            _ => {
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

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Rc<[Identifier]>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{}({}) {{\n{}\n}}", self.token, params, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Rc<Expression>,
    pub arguments: Rc<[Expression]>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{}({})", self.function, args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Rc<[Expression]>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let elements = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "[{}]", elements)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub index: Rc<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}
