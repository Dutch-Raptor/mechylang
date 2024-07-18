use color_print::cformat;
use crate::errors::ErrorKind;
use crate::lexer::tokens::{Position, TokenKind};
use crate::parser::Parser;
use crate::{Error, Token};

impl Parser {
    /// Advances the parser to the next token in the input stream.
    ///
    /// This method retrieves the next token from the lexer and updates the
    /// `cur_token` and `peek_token` fields accordingly.
    ///
    /// If there are no more tokens available from the lexer, it sets the `cur_token`
    /// to `EOF` (End of File) and adjusts its position based on the last peeked token.
    ///
    /// # Examples
    ///
    /// ```
    /// use mechylang::Parser;
    /// use mechylang::Lexer;
    /// use mechylang::{Token, TokenKind};
    ///
    /// let mut lexer = Lexer::new("let x = 5;");
    /// let mut parser = Parser::new(lexer);
    /// assert_eq!(parser.cur_token.kind, TokenKind::Let);
    ///
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.kind, TokenKind::Identifier("x".to_string()));
    /// ```
    pub fn next_token(&mut self) {
        let token = self.lexer.next_token();
        let next = match token {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                position: Position {
                    line: self.peek_token.position.line,
                    column: self.peek_token.position.column + self.peek_token.position.length + 1,
                    length: 1,
                    file: self.peek_token.position.file.clone(),
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
    ///
    /// # Examples
    ///
    /// ```
    /// use mechylang::{Parser, TokenKind, Lexer};
    ///
    /// let mut lexer = Lexer::new("let x = 5;");
    /// let mut parser = Parser::new(lexer);
    ///
    /// // Ensure the current token is `TokenKind::Let`
    /// assert!(parser.expect_current(TokenKind::Let).is_ok());
    ///
    /// // Move to the next token
    /// parser.next_token();
    ///
    /// // Ensure the current token is an identifier "x"
    /// assert!(parser.expect_current(TokenKind::Identifier("x".to_string())).is_ok());
    /// ```
    pub fn expect_current(&self, token: TokenKind) -> Result<(), Error> {
        if self.cur_token.kind == token {
            Ok(())
        } else {
            Err(self.error(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected current token to be <i>{:?}</i>, got <i>{:?}</i> instead",
                    token,
                    self.cur_token.kind
                ),
                Some(&self.cur_token),
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
    ///
    /// # Examples
    ///
    /// ```
    /// use mechylang::{Parser, TokenKind, Lexer};
    ///
    /// let mut lexer = Lexer::new("let x = 5;");
    /// let mut parser = Parser::new(lexer);
    /// 
    /// // Ensure the next token (peek token) is `TokenKind::Equal` and consume it
    /// assert!(parser.expect_peek(TokenKind::Identifier("x".to_string())).is_ok());
    ///
    /// // Ensure the next token (peek token) is `TokenKind::Equal` and consume it
    /// assert!(parser.expect_peek(TokenKind::AssignEqual).is_ok());
    ///
    /// // Now ensure the next token (peek token) is `TokenKind::Integer(5)` and consume it
    /// assert!(parser.expect_peek(TokenKind::Number("5".to_string())).is_ok());
    /// ```
    pub fn expect_peek(&mut self, token: TokenKind) -> Result<(), Error> {
        if self.peek_token.kind == token {
            self.next_token();
            Ok(())
        } else {
            Err(self.error_peek(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected next token to be <i>{:?}</i>, got <i>{:?}</i> instead",
                    token,
                    self.peek_token.kind
                ),
            ))
        }
    }


    pub(crate) fn is_cur_token(&self, token: TokenKind) -> bool {
        self.cur_token.kind == token
    }

    pub(crate) fn is_peek_token(&self, token: TokenKind) -> bool {
        self.peek_token.kind == token
    }
}