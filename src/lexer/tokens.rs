use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

/// Tokens for the lexer
#[derive(Debug, PartialEq, Clone)]
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

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Caret,
    Ampersand,
    Pipe,
    Tilde,
    Bang,
    Question,
    Dot,
    Colon,
    Semicolon,
    Comma,

    // Assignment
    AssignEqual,
    AssignPlusEqual,
    AssignMinusEqual,
    AssignAsteriskEqual,
    AssignSlashEqual,
    AssignPercentEqual,
    AssignCaretEqual,
    AssignAmpersandEqual,
    AssignPipeEqual,

    // Comparison
    CompareEqual,
    CompareNotEqual,
    CompareLess,
    CompareLessEqual,
    CompareGreater,
    CompareGreaterEqual,

    // Logical
    And,
    Or,

    // Brackets
    LeftParen,
    RightParen,
    LeftSquirly,
    RightSquirly,
    LeftSquare,
    RightSquare,

    // Other
    Identifier(String),
    EOF,
    Illegal(String),
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
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
