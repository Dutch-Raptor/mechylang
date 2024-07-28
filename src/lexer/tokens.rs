use std::fmt::{self, Display, Formatter};

use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Default, Serialize)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub file: Option<String>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

impl Token {
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::EOF,
            position: Position::default(),
        }
    }
}

/// Tokens for the lexer
// ignore the unused variants for now
#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum TokenKind {
    // Keywords
    Let,
    If,
    Else,
    While,
    For,
    In,
    Return,
    Break,
    Continue,
    Fn,
    Struct,
    Enum,
    Match,
    As,
    Use,
    True,
    False,

    // Literals
    Number(String),
    String(String),
    Char(char),
    Unit,

    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Bang,
    Question,
    Dot,
    Colon,
    Semicolon,
    Comma,

    // Logical
    LogicalAnd,
    LogicalOr,

    // Bitwise
    Ampersand,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseRightShift,
    BitwiseLeftShift,

    // Assignment
    AssignEqual,
    AssignPlus,
    AssignMinus,
    AssignMultiply,
    AssignDivide,
    AssignModulo,
    AssignBitwiseXor,
    AssignBitwiseAnd,
    AssignBitwiseOr,

    // Comparison
    CompareEqual,
    CompareNotEqual,
    CompareLess,
    CompareLessEqual,
    CompareGreater,
    CompareGreaterEqual,

    // Brackets
    LeftParen,
    RightParen,
    LeftSquirly,
    RightSquirly,
    LeftSquare,
    RightSquare,

    RangeExclusive,
    RangeInclusive,

    // Other
    Identifier(String),
    EOF,
    Illegal(String),
    Ellipsis,
}

impl TokenKind {
    pub fn is_keyword(string: &str) -> Option<TokenKind> {
        match string {
            "let" => Some(TokenKind::Let),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "while" => Some(TokenKind::While),
            "for" => Some(TokenKind::For),
            "in" => Some(TokenKind::In),
            "return" => Some(TokenKind::Return),
            "break" => Some(TokenKind::Break),
            "continue" => Some(TokenKind::Continue),
            "fn" => Some(TokenKind::Fn),
            "struct" => Some(TokenKind::Struct),
            "enum" => Some(TokenKind::Enum),
            "match" => Some(TokenKind::Match),
            "as" => Some(TokenKind::As),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            _ => None,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;

        match self {
            // Keywords
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            While => write!(f, "while"),
            For => write!(f, "for"),
            In => write!(f, "in"),
            Return => write!(f, "return"),
            Break => write!(f, "break"),
            Continue => write!(f, "continue"),
            Fn => write!(f, "fn"),
            Struct => write!(f, "struct"),
            Enum => write!(f, "enum"),
            Match => write!(f, "match"),
            As => write!(f, "as"),
            Use => write!(f, "use"),
            True => write!(f, "true"),
            False => write!(f, "false"),

            // Literals
            Number(value) => write!(f, "{}", value),
            String(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Unit => write!(f, "()"),

            // Operators
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Modulo => write!(f, "%"),
            Bang => write!(f, "!"),
            Question => write!(f, "?"),
            Dot => write!(f, "."),
            Colon => write!(f, ":"),
            Semicolon => write!(f, ";"),
            Comma => write!(f, ","),

            // Logical
            LogicalAnd => write!(f, "&&"),
            LogicalOr => write!(f, "||"),

            // Bitwise
            Ampersand => write!(f, "&"),
            BitwiseOr => write!(f, "|"),
            BitwiseXor => write!(f, "^"),
            BitwiseNot => write!(f, "~"),
            BitwiseRightShift => write!(f, ">>"),
            BitwiseLeftShift => write!(f, "<<"),

            // Assignment
            AssignEqual => write!(f, "="),
            AssignPlus => write!(f, "+="),
            AssignMinus => write!(f, "-+"),
            AssignMultiply => write!(f, "*="),
            AssignDivide => write!(f, "/="),
            AssignModulo => write!(f, "%="),
            AssignBitwiseXor => write!(f, "^="),
            AssignBitwiseAnd => write!(f, "&="),
            AssignBitwiseOr => write!(f, "|="),

            // Comparison
            CompareEqual => write!(f, "=="),
            CompareNotEqual => write!(f, "=="),
            CompareLess => write!(f, "<"),
            CompareLessEqual => write!(f, "<="),
            CompareGreater => write!(f, ">"),
            CompareGreaterEqual => write!(f, ">="),

            // Brackets
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftSquirly => write!(f, "{{"),
            RightSquirly => write!(f, "}}"),
            LeftSquare => write!(f, "["),
            RightSquare => write!(f, "]"),

            RangeExclusive => write!(f, ".."),
            RangeInclusive => write!(f, "..="),

            // Other
            Identifier(name) => write!(f, "Identifier({})", name),
            EOF => write!(f, "EOF"),
            Illegal(msg) => write!(f, "Illegal({})", msg),
            Ellipsis => write!(f, "Ellipsis"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

