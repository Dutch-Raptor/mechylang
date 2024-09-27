use std::fmt::{self, Display, Formatter};
use std::ops::Range;
use std::sync::Arc;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Default, Serialize)]
pub struct Span {
    /// The range of bytes in the source file
    pub bytes: Range<usize>,
    pub file: Option<Arc<str>>,
}

impl Span {
    pub fn length(&self) -> usize {
        self.bytes.end - self.bytes.start
    }
    
    pub fn start(&self) -> usize {
        self.bytes.start
    }
    
    pub fn end(&self) -> usize {
        self.bytes.end
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn length(&self) -> usize {
        self.span.length()
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::EOF,
            span: Span::default(),
        }
    }
}

/// Tokens for the lexer
// ignore the unused variants for now
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
    
    pub fn as_number(&self) -> Option<&str> {
        match self {
            TokenKind::Number(value) => Some(value),
            _ => None,
        }
    }
    
    pub fn as_string(&self) -> Option<&str> {
        match self {
            TokenKind::String(value) => Some(value),
            _ => None,
        }
    }
    
    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            TokenKind::Identifier(value) => Some(value),
            _ => None,
        }
    }
    
    pub fn as_char(&self) -> Option<char> {
        match self {
            TokenKind::Char(value) => Some(*value),
            _ => None,
        }
    }
    
    
    pub fn name(&self) -> &str {
        match self {
            TokenKind::Let => "let",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::Return => "return",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Fn => "fn",
            TokenKind::Struct => "struct",
            TokenKind::Enum => "enum",
            TokenKind::Match => "match",
            TokenKind::As => "as",
            TokenKind::Use => "use",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Number(_) => "number",
            TokenKind::String(_) => "string",
            TokenKind::Char(_) => "char",
            TokenKind::Unit => "unit",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Multiply => "*",
            TokenKind::Divide => "/",
            TokenKind::Modulo => "%",
            TokenKind::Bang => "!",
            TokenKind::Question => "?",
            TokenKind::Dot => ".",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::Comma => ",",
            TokenKind::LogicalAnd => "&&",
            TokenKind::LogicalOr => "||",
            TokenKind::Ampersand => "&",
            TokenKind::BitwiseOr => "|",
            TokenKind::BitwiseXor => "^",
            TokenKind::BitwiseNot => "~",
            TokenKind::BitwiseRightShift => ">>",
            TokenKind::BitwiseLeftShift => "<<",
            TokenKind::AssignEqual => "=",
            TokenKind::AssignPlus => "+=",
            TokenKind::AssignMinus => "-=",
            TokenKind::AssignMultiply => "*=",
            TokenKind::AssignDivide => "/=",
            TokenKind::AssignModulo => "%=",
            TokenKind::AssignBitwiseXor => "^=",
            TokenKind::AssignBitwiseAnd => "&=",
            TokenKind::AssignBitwiseOr => "|=",
            TokenKind::CompareEqual => "==",
            TokenKind::CompareNotEqual => "!=",
            TokenKind::CompareLess => "<",
            TokenKind::CompareLessEqual => "<=",
            TokenKind::CompareGreater => ">",
            TokenKind::CompareGreaterEqual => ">=",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftSquirly => "{",
            TokenKind::RightSquirly => "}",
            TokenKind::LeftSquare => "[",
            TokenKind::RightSquare => "]",
            TokenKind::RangeExclusive => "..",
            TokenKind::RangeInclusive => "..=",
            TokenKind::Identifier(_) => "identifier",
            TokenKind::EOF => "EOF",
            TokenKind::Ellipsis => "ellipsis",
        }
    }
    
    pub const fn length_bytes(&self) -> Option<usize> {
        match self {
            TokenKind::Let => Some(3),
            TokenKind::If => Some(3),
            TokenKind::Else => Some(3),
            TokenKind::While => Some(3),
            TokenKind::For => Some(3),
            TokenKind::In => Some(3),
            TokenKind::Return => Some(3),
            TokenKind::Break => Some(3),
            TokenKind::Continue => Some(3),
            TokenKind::Fn => Some(3),
            TokenKind::Struct => Some(3),
            TokenKind::Enum => Some(3),
            TokenKind::Match => Some(3),
            TokenKind::As => Some(3),
            TokenKind::Use => Some(3),
            TokenKind::True => Some(3),
            TokenKind::False => Some(3),
            TokenKind::Number(_) => None,
            TokenKind::String(_) => None,
            TokenKind::Char(c) => Some(c.len_utf8()),
            TokenKind::Unit => Some(2),
            TokenKind::Plus => Some(1),
            TokenKind::Minus => Some(1),
            TokenKind::Multiply => Some(1),
            TokenKind::Divide => Some(1),
            TokenKind::Modulo => Some(1),
            TokenKind::Bang => Some(1),
            TokenKind::Question => Some(1),
            TokenKind::Dot => Some(1),
            TokenKind::Colon => Some(1),
            TokenKind::Semicolon => Some(1),
            TokenKind::Comma => Some(1),
            TokenKind::LogicalAnd => Some(2),
            TokenKind::LogicalOr => Some(2),
            TokenKind::Ampersand => Some(1),
            TokenKind::BitwiseOr => Some(1),
            TokenKind::BitwiseXor => Some(1),
            TokenKind::BitwiseNot => Some(1),
            TokenKind::BitwiseRightShift => Some(2),
            TokenKind::BitwiseLeftShift => Some(2),
            TokenKind::AssignEqual => Some(1),
            TokenKind::AssignPlus => Some(2),
            TokenKind::AssignMinus => Some(2),
            TokenKind::AssignMultiply => Some(2),
            TokenKind::AssignDivide => Some(2),
            TokenKind::AssignModulo => Some(2),
            TokenKind::AssignBitwiseXor => Some(2),
            TokenKind::AssignBitwiseAnd => Some(2),
            TokenKind::AssignBitwiseOr => Some(2),
            TokenKind::CompareEqual => Some(2),
            TokenKind::CompareNotEqual => Some(2),
            TokenKind::CompareLess => Some(1),
            TokenKind::CompareLessEqual => Some(2),
            TokenKind::CompareGreater => Some(1),
            TokenKind::CompareGreaterEqual => Some(2),
            TokenKind::LeftParen => Some(1),
            TokenKind::RightParen => Some(1),
            TokenKind::LeftSquirly => Some(1),
            TokenKind::RightSquirly => Some(1),
            TokenKind::LeftSquare => Some(1),
            TokenKind::RightSquare => Some(1),
            TokenKind::RangeExclusive => Some(2),
            TokenKind::RangeInclusive => Some(3),
            TokenKind::Identifier(_) => None,
            TokenKind::EOF => None,
            TokenKind::Ellipsis => Some(3),
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
            Ellipsis => write!(f, "Ellipsis"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

