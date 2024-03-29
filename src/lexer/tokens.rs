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
    pub(crate) fn is_statement_terminator(&self, previous_token: &Token) -> bool {
        if self.kind == TokenKind::Semicolon {
            return true;
        }

        if self.kind == TokenKind::RightSquirly {
            return true;
        }

        if self.kind == TokenKind::EOF {
            return true;
        }

        if self.kind == TokenKind::RightParen {
            return true;
        }

        if self.kind == TokenKind::RightSquare {
            return true;
        }

        if self.kind == TokenKind::Else {
            return true;
        }

        // if previous token was on a different line

        if self.position.line != previous_token.position.line {
            return true;
        }

        return false;
    }
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
    Tilde,
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
    BitwiseRightShift,
    BitwiseLeftShift,
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenKind::Identifier(string) => write!(f, "Ident({})", string),
            TokenKind::Illegal(string) => write!(f, "Illegal({})", string),
            TokenKind::Number(number) => write!(f, "Number({})", number),
            TokenKind::String(string) => write!(f, "String({})", string),
            TokenKind::Char(char) => write!(f, "Char({})", char),
            _ => write!(f, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
