use super::tokens::Token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
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
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
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
    ///
    /// # Examples
    /// ```
    /// use interpreter::lexer::lexer::Lexer;
    /// use interpreter::lexer::tokens::Token;
    ///
    /// let input = String::from("=+(){},;");
    /// let mut lexer = Lexer::new(input);
    /// let expected = vec![
    ///     Some(Token::AssignEqual),
    ///     Some(Token::Plus),
    ///     Some(Token::LeftParen),
    ///     Some(Token::RightParen),
    ///     Some(Token::LeftSquirly),
    ///     Some(Token::RightSquirly),
    ///     Some(Token::Comma),
    ///     Some(Token::Semicolon),
    ///     None,
    /// ];
    /// for expected_token in expected {
    ///    let token = lexer.next_token();
    ///    assert_eq!(token, expected_token);
    /// }
    /// ```
    ///
    /// ```
    /// use interpreter::lexer::lexer::Lexer;
    /// use interpreter::lexer::tokens::Token;
    ///
    /// let input = String::from(
    /// r#"let five = 5;
    /// let ten = 10;
    /// let add = fn(x, y) {
    ///    x + y
    /// };
    /// let result = add(five, ten);
    /// "#
    /// );
    ///
    /// let mut lexer = Lexer::new(input);
    /// let expected = vec![
    ///     Some(Token::Let),
    ///     Some(Token::Identifier(String::from("five"))),
    ///     Some(Token::AssignEqual),
    ///     Some(Token::Number(String::from("5"))),
    ///     Some(Token::Semicolon),
    ///     Some(Token::Let),
    ///     Some(Token::Identifier(String::from("ten"))),
    ///     Some(Token::AssignEqual),
    ///     Some(Token::Number(String::from("10"))),
    ///     Some(Token::Semicolon),
    ///     Some(Token::Let),
    ///     Some(Token::Identifier(String::from("add"))),
    ///     Some(Token::AssignEqual),
    ///     Some(Token::Fn),
    ///     Some(Token::LeftParen),
    ///     Some(Token::Identifier(String::from("x"))),
    ///     Some(Token::Comma),
    ///     Some(Token::Identifier(String::from("y"))),
    ///     Some(Token::RightParen),
    ///     Some(Token::LeftSquirly),
    ///     Some(Token::Identifier(String::from("x"))),
    ///     Some(Token::Plus),
    ///     Some(Token::Identifier(String::from("y"))),
    ///     Some(Token::RightSquirly),
    ///     Some(Token::Semicolon),
    ///     Some(Token::Let),
    ///     Some(Token::Identifier(String::from("result"))),
    ///     Some(Token::AssignEqual),
    ///     Some(Token::Identifier(String::from("add"))),
    ///     Some(Token::LeftParen),
    ///     Some(Token::Identifier(String::from("five"))),
    ///     Some(Token::Comma),
    ///     Some(Token::Identifier(String::from("ten"))),
    ///     Some(Token::RightParen),
    ///     Some(Token::Semicolon),
    ///     None,
    ///     None,
    /// ];
    ///
    /// for expected_token in expected {
    ///    let token = lexer.next_token();
    ///    println!("token: {:?}", token);
    ///    assert_eq!(token, expected_token);
    /// }
    ///
    ///
    /// ```
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.read_position > self.input.len() {
            return None;
        }

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(Token::CompareEqual)
                } else {
                    Some(Token::AssignEqual)
                }
            }
            ';' => Some(Token::Semicolon),
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            ',' => Some(Token::Comma),
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(Token::CompareNotEqual)
                } else {
                    Some(Token::Bang)
                }
            }
            '/' => Some(Token::Slash),
            '*' => Some(Token::Asterisk),
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(Token::CompareLessEqual)
                } else {
                    Some(Token::CompareLess)
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(Token::CompareGreaterEqual)
                } else {
                    Some(Token::CompareGreater)
                }
            }
            '{' => Some(Token::LeftSquirly),
            '}' => Some(Token::RightSquirly),
            '[' => Some(Token::LeftSquare),
            ']' => Some(Token::RightSquare),
            '\0' => Some(Token::EOF),
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    if let Some(keyword_token) = Token::is_keyword(&literal) {
                        Some(keyword_token)
                    } else {
                        Some(Token::Identifier(literal))
                    }
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    Some(Token::Number(literal))
                } else {
                    Some(Token::Illegal(self.ch.to_string()))
                }
            }
        };
        self.read_char();
        return token;
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}
