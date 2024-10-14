use crate::parser::Parser;
use crate::{Token, TokenKind};
use crate::lexer::{Span};

use crate::parser::{Error, Result};
use crate::parser::error::Location;

impl<'a> Parser<'a> {
    /// Advances the parser to the next token in the input stream.
    ///
    /// This method retrieves the next token from the lexer and updates the
    /// `cur_token` and `peek_token` fields accordingly.
    ///
    /// If there are no more tokens available from the lexer, it sets the `cur_token`
    /// to `EOF` (End of File) and adjusts its position based on the last peeked token.
    pub(crate) fn next_token(&mut self) -> Result<()> {
        let token = self.lexer.next_token();
        let next = match token {
            Some(Ok(token)) => token,
            Some(Err(err)) => return Err(Error::LexerError(err)),
            None => {
                let peek_token_end_byte = self.peek_token.span.bytes.end;
                Token {
                    kind: TokenKind::EOF,
                    span: Span {
                        bytes: peek_token_end_byte..peek_token_end_byte + 1,
                        file: self.peek_token.span.file.clone(),
                    },
                }
            },
        };

        self.cur_token = std::mem::replace(&mut self.peek_token, next);
        Ok(())
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
    pub(crate) fn expect_current(&self, token: TokenKind, location: Option<Location>) -> Result<()> {
        if self.cur_token.kind == token {
            Ok(())
        } else {
            Err(
                Error::UnexpectedToken {
                    span: self.cur_token.span.clone(),
                    expected: vec![token],
                    found: self.cur_token.kind.clone(),
                    location,
                }
            )
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
    pub(crate) fn expect_peek(&mut self, expected_token_kind: TokenKind, location: Option<Location>) -> Result<()> {
        if self.peek_token.kind == expected_token_kind {
            self.next_token()?;
            Ok(())
        } else {
            Err(
                Error::UnexpectedToken {
                    span: self.peek_token.span.clone(),
                    expected: vec![expected_token_kind],
                    found: self.peek_token.kind.clone(),
                    location,
                }
            )
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
    pub(super) fn span_with_start(&self, start: &Span) -> Span {
        Span {
            bytes: start.bytes.start..self.cur_token.span.bytes.end,
            file: self.cur_token.span.file.clone(),
        }
    }
}