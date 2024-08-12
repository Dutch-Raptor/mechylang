use color_print::cformat;
use crate::error::ErrorKind;
use crate::parser::Parser;
use crate::{Error, Token, TokenKind};
use crate::lexer::{Position, Span};

impl Parser {
    /// Advances the parser to the next token in the input stream.
    ///
    /// This method retrieves the next token from the lexer and updates the
    /// `cur_token` and `peek_token` fields accordingly.
    ///
    /// If there are no more tokens available from the lexer, it sets the `cur_token`
    /// to `EOF` (End of File) and adjusts its position based on the last peeked token.
    pub(crate) fn next_token(&mut self) {
        let token = self.lexer.next_token();
        let next = match token {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span {
                    start: Position {
                        line: self.peek_token.span.start.line,
                        column: self.peek_token.span.end.column + 1,
                        file: self.peek_token.span.start.file.clone(),
                    },
                    end: Position {
                        line: self.peek_token.span.start.line,
                        column: self.peek_token.span.end.column + 1,
                        file: self.peek_token.span.start.file.clone(),
                    },
                }
            },
        };

        self.cur_token = std::mem::replace(&mut self.peek_token, next);
    }

    /// Checks if the current token matches the expected token kind.
    ///
    /// If the current token matches the expected `token`, returns `Ok(())`.
    /// If the current token does not match the expected `token`, returns an `Error`.
    ///
    /// # Arguments
    ///
    /// * `token` - The expected `TokenKind` to compare against the current token.
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if the current token matches the expected `token`.
    /// Returns an `Error` otherwise, indicating an unexpected token.
    pub(crate) fn expect_current(&self, token: TokenKind) -> Result<(), Error> {
        if self.cur_token.kind == token {
            Ok(())
        } else {
            Err(self.error(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected token to be <strong><K> {} </></>, got <strong><K>{}</></> instead",
                    token,
                    self.cur_token.kind
                ),
                self.cur_token.span.clone(),
                None,
            ))
        }
    }

    /// Checks if the next token (peek token) matches the expected token kind and advances to the next token.
    ///
    /// If the next token (peek token) matches the expected `token`, advances to consume it and returns `Ok(())`.
    /// If the next token (peek token) does not match the expected `token`, returns an `Error`.
    ///
    /// # Arguments
    ///
    /// * `token` - The expected `TokenKind` to compare against the next token (peek token).
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if the next token (peek token) matches the expected `token` and advances to consume it.
    /// Returns an `Error` otherwise, indicating an unexpected token.
    pub(crate) fn expect_peek(&mut self, token_kind: TokenKind) -> Result<(), Error> {
        if self.peek_token.kind == token_kind {
            self.next_token();
            Ok(())
        } else {
            Err(self.error_peek(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected token to be <strong><K> {} </></>, got <strong><K>{}</></> instead",
                    token_kind,
                    self.peek_token.kind
                ),
            ))
        }
    }

    /// Checks if the current token matches the expected token kind.
    ///
    /// This method compares the `kind` of the current token (`cur_token`) with the provided `token`
    /// and returns `true` if they match, or `false` otherwise.
    ///
    /// # Parameters
    /// - `token`: The `TokenKind` to compare against the `cur_token`.
    ///
    /// # Returns
    /// - `bool`: `true` if the `cur_token`'s kind matches the provided `token`; otherwise, `false`.
    pub(crate) fn is_cur_token(&self, token: TokenKind) -> bool {
        self.cur_token.kind == token
    }
    /// Checks if the peek token matches the expected token kind.
    ///
    /// This method compares the `kind` of the peek token (`peek_token`) with the provided `token`
    /// and returns `true` if they match, or `false` otherwise.
    ///
    /// # Parameters
    /// - `token`: The `TokenKind` to compare against the `peek_token`.
    ///
    /// # Returns
    /// - `bool`: `true` if the `peek_token`'s kind matches the provided `token`; otherwise, `false`.
    pub(crate) fn is_peek_token(&self, token: TokenKind) -> bool {
        self.peek_token.kind == token
    }
    
    /// Creates a new `Span` with the given start position. And the end position is the current token's end position.
    ///
    /// # Arguments
    /// * `start` - The start position of the new `Span`.
    ///
    /// # Returns
    /// A new `Span` with the given start position and the end position set to the current token's end position.
    pub(super) fn span_with_start(&self, start: Position) -> Span {
        Span {
            start,
            end: self.cur_token.span.end.clone(),
        }
    }
}