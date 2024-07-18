use color_print::cformat;
use crate::errors::ErrorKind;
use crate::lexer::tokens::{Position, TokenKind};
use crate::parser::Parser;
use crate::{Error, Token};

impl Parser {
    pub(crate) fn next_token(&mut self) {
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



    /// Checks if the current token is `token`, returns an error if not
    pub(crate) fn expect_current(&self, token: TokenKind) -> Result<(), Error> {
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

    /// Checks if the peek token is the expected token and advances it to be the current token if it is.
    ///
    /// If the peek token is not the expected token `expect_peek` returns an Err
    pub(crate) fn expect_peek(&mut self, token: TokenKind) -> Result<(), Error> {
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