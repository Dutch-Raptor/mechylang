/// Tokens for the lexer
#[derive(Debug, PartialEq)]
pub enum Token {
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

impl Token {
    pub fn is_keyword(string: &str) -> Option<Token> {
        match string {
            "let" => Some(Token::Let),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "while" => Some(Token::While),
            "for" => Some(Token::For),
            "in" => Some(Token::In),
            "return" => Some(Token::Return),
            "break" => Some(Token::Break),
            "continue" => Some(Token::Continue),
            "fn" => Some(Token::Fn),
            "struct" => Some(Token::Struct),
            "enum" => Some(Token::Enum),
            "match" => Some(Token::Match),
            "as" => Some(Token::As),
            "True" => Some(Token::True),
            "False" => Some(Token::False),
            _ => None,
        }
    }
}
