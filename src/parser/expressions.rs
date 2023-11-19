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

use super::parser::BlockStatement;

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Range,       // .. or ..=
    LogicalOr,   // ||
    LogicalAnd,  // &&
    Equals,      // ==
    LessGreater, // > or <
    BitwiseOr,   // |
    BitwiseXor,  // ^
    BitwiseAnd,  // &
    BitShift,    // << or >>
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -X or !X
    Index,       // array[index]
    Call,        // myFunction(X)
    Member,      // object.member
}

pub trait PrecedenceTrait {
    fn precedence(&self) -> Option<Precedence>;
}

impl PrecedenceTrait for TokenKind {
    fn precedence(&self) -> Option<Precedence> {
        let presedence = match self {
            // Precedences listed in ascending order, but lowest precedence at the end
            // *** Assign ***
            TokenKind::AssignEqual
            | TokenKind::AssignPlus
            | TokenKind::AssignMinus
            | TokenKind::AssignMultiply
            | TokenKind::AssignDivide
            | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseOr
            | TokenKind::AssignBitwiseAnd
            | TokenKind::AssignBitwiseXor => Precedence::Assign,

            // *** Range ***
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => Precedence::Range,

            // *** Logical Or ***
            TokenKind::LogicalOr => Precedence::LogicalOr,

            // *** Logical And ***
            TokenKind::LogicalAnd => Precedence::LogicalAnd,

            // *** Equals ***
            TokenKind::CompareEqual | TokenKind::CompareNotEqual => Precedence::Equals,

            // *** Less Greater ***
            TokenKind::CompareLess
            | TokenKind::CompareLessEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual => Precedence::LessGreater,

            // *** Bitwise Or ***
            TokenKind::BitwiseOr => Precedence::BitwiseOr,

            // *** Bitwise Xor ***
            TokenKind::BitwiseXor => Precedence::BitwiseXor,

            // *** Bitwise And ***
            TokenKind::Ampersand => Precedence::BitwiseAnd,

            // *** Bit Shift ***
            TokenKind::BitwiseLeftShift | TokenKind::BitwiseRightShift => Precedence::BitShift,

            // *** Sum ***
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,

            // *** Product ***
            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => Precedence::Product,

            // *** Prefix ***
            // does not get inferred from token kind, but is used in parsing prefix expressions

            // *** Index ***
            TokenKind::LeftSquare => Precedence::Index,

            // *** Call ***
            TokenKind::LeftParen => Precedence::Call,

            // *** Member ***
            TokenKind::Dot => Precedence::Member,

            // *** Lowest precedence ***
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
            TokenKind::EOF => Precedence::Lowest,
            TokenKind::RightParen => Precedence::Lowest,
            TokenKind::For => Precedence::Lowest,
            TokenKind::In => Precedence::Lowest,
            TokenKind::If => Precedence::Lowest,
            TokenKind::Number(_) => Precedence::Lowest,
            TokenKind::String(_) => Precedence::Lowest,
            TokenKind::Fn => Precedence::Lowest,
            TokenKind::While => Precedence::Lowest,
            TokenKind::Else => Precedence::Lowest,
            TokenKind::True => Precedence::Lowest,
            TokenKind::False => Precedence::Lowest,
            TokenKind::Unit => Precedence::Lowest,
            TokenKind::Break => Precedence::Lowest,
            TokenKind::Continue => Precedence::Lowest,
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
    Range(RangeExpression),
    RangeTo(RangeToExpression),
    RangeFrom(RangeFromExpression),
    RangeFull(RangeFullExpression),
    For(ForExpression),
    While(WhileExpression),
    Member(MemberExpression),
    Unit(Token),
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
    BitwiseNot,
    Ampersand,
    Asterisk,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PrefixOperator::Bang => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
            PrefixOperator::BitwiseNot => write!(f, "~"),
            PrefixOperator::Ampersand => write!(f, "&"),
            PrefixOperator::Asterisk => write!(f, "*"),
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct RangeFullExpression {
    pub token: Token,
}

impl Display for RangeFullExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(..)")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForExpression {
    pub token: Token,
    pub iterator: Identifier,
    pub iterable: Rc<Expression>,
    pub body: BlockStatement,
    pub index: Option<Identifier>,
    pub else_block: Option<BlockStatement>,
}

impl Display for ForExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "for {} in {} {{\n{}\n}}",
            match &self.index {
                Some(index) => format!("({}, {})", index, self.iterator),
                None => format!("{}", self.iterator),
            },
            self.iterable,
            self.body
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileExpression {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub body: BlockStatement,
    pub else_block: Option<BlockStatement>,
}

impl Display for WhileExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while {} {{\n{}\n}}", self.condition, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberExpression {
    pub token: Token,
    pub object: Rc<Expression>,
    pub property: Identifier,
}

impl Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.object, self.property)
    }
}
