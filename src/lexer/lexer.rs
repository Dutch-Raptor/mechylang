use super::tokens::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    line: usize,
    column: usize,
}

/// Implement the Lexer struct
///
/// The Lexer struct is responsible for lexing the input string into tokens.
impl Lexer {
    /// Create a new Lexer
    ///
    /// # Arguments
    /// * `input` - A String slice that holds the input to be lexed
    ///
    /// # Example
    /// ```
    /// use interpreter::lexer::lexer::Lexer;
    ///
    /// let input = String::from(
    ///     r#"
    ///     fn main() {
    ///     print(\"Hello, World!\");
    ///     }"#
    /// );
    /// let lexer = Lexer::new(input);
    /// ```
    pub fn new<T: AsRef<str>>(input: T) -> Lexer {
        let input = input.as_ref().to_string();
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
            line: 1,
            column: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        // Update line and column
        match self.ch {
            '\n' => {
                self.line += 1;
                self.column = 0;
            }
            '\t' => self.column += 4,
            '\r' => self.column = 0,
            _ => self.column += 1,
        }

        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        // Since we keep reading until we find a non-letter character, we need to
        // go back one character to get the last letter.
        self.position -= 1;
        self.read_position -= 1;
        self.input[position..=self.position].to_string()
    }

    /// Read a number from the input string.
    ///
    /// This can be an integer or a floating point number.
    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) || self.ch == '.' {
            self.read_char();
        }
        self.position -= 1;
        self.read_position -= 1;
        self.input[position..=self.position].to_string()
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    /// Get the next token from the input string.
    ///
    /// This will skip whitespace and return the next token.
    /// If the end of the input string is reached, None
    /// will be returned.
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.read_position > self.input.len() {
            return None;
        }

        let line = self.line;
        let column = self.column;
        let token_kind = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareEqual)
                } else {
                    Some(TokenKind::AssignEqual)
                }
            }
            ';' => Some(TokenKind::Semicolon),
            '(' => Some(TokenKind::LeftParen),
            ')' => Some(TokenKind::RightParen),
            ',' => Some(TokenKind::Comma),
            '+' => Some(TokenKind::Plus),
            '-' => Some(TokenKind::Minus),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareNotEqual)
                } else {
                    Some(TokenKind::Bang)
                }
            }
            '/' => Some(TokenKind::Slash),
            '*' => Some(TokenKind::Asterisk),
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareLessEqual)
                } else {
                    Some(TokenKind::CompareLess)
                }
            }
            '%' => Some(TokenKind::Percent),
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareGreaterEqual)
                } else {
                    Some(TokenKind::CompareGreater)
                }
            }
            '{' => Some(TokenKind::LeftSquirly),
            '}' => Some(TokenKind::RightSquirly),
            '[' => Some(TokenKind::LeftSquare),
            ']' => Some(TokenKind::RightSquare),
            '\0' => Some(TokenKind::EOF),
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    if let Some(keyword_token) = TokenKind::is_keyword(&literal) {
                        Some(keyword_token)
                    } else {
                        Some(TokenKind::Identifier(literal))
                    }
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    Some(TokenKind::Number(literal))
                } else {
                    Some(TokenKind::Illegal(self.ch.to_string()))
                }
            }
        };

        self.read_char();

        if let Some(token_kind) = token_kind {
            Some(Token {
                kind: token_kind,
                line,
                column,
            })
        } else {
            None
        }
    }

    pub fn lines(&self) -> Vec<String> {
        self.input.lines().map(|s| s.to_string()).collect()
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
