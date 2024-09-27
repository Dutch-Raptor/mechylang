use crate::{Span, TokenKind};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    /// Lexer error
    LexerError(crate::lexer::Error),
    /// Expected a statement to end, but found `{found}`
    UnterminatedStatement { span: Span },
    /// Expected one of the following tokens, but found `{found}`
    UnexpectedToken { span: Span, expected: Vec<TokenKind>, found: TokenKind },
    /// Expected a prefix operator, but found `{found}`
    InvalidPrefix { span: Span, found: TokenKind },
    /// Expected a number, but found `{found}`
    InvalidNumber { span: Span, found: TokenKind },
    /// Expected a struct key, but found `{found}`
    InvalidStructKey { span: Span, found: TokenKind },
    /// Expected an infix operator, but found `{found}`
    InvalidInfix { span: Span, found: TokenKind },
    /// Expected an expression list to either contain an(other) expression or the end token, but found `{found}`
    UnexpectedExpressionListEnd { list_span: Span, expected_end_token: TokenKind, found: TokenKind, parse_expression_error: Option<Box<Error>> },
    AmbiguousReturn { return_span: Span, found: TokenKind },
    MissingPrecedence { span: Span, for_token_kind: TokenKind },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}