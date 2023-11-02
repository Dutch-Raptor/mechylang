use super::tokens::{Position, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    /// The current position in the input string
    position: usize,
    /// The next position in the input string
    read_position: usize,
    ch: char,
    line: usize,
    column: usize,

    file: Option<String>,
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
    /// use mechylang::Lexer;
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
            file: None,
        };

        l.read_char();
        l
    }

    /// Advance the lexer by one character
    fn read_char(&mut self) {
        // Update line and column
        match self.ch {
            '\n' => {
                self.line += 1;
                self.column = 0;
            }
            '\t' => {
                self.column += 4;
            }
            '\r' => self.column = 0,
            _ => self.column += 1,
        }

        if self.read_position >= self.input.chars().count() {
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
        while is_letter(self.peek_char()) || is_digit(self.peek_char()) || self.peek_char() == '_' {
            self.read_char();
        }
        self.get_string(position, self.position)
    }

    /// Read a string from the input string.
    /// * `start` - The index of the first character in the string
    /// * `end` - The index of the last character in the string
    fn get_string(&self, start: usize, end: usize) -> String {
        let (start, _) = self.input.char_indices().nth(start).unwrap();
        // get the first byte of the character after the string
        // We do this in case the last character is a multibyte character
        let (end, _) = self
            .input
            .char_indices()
            .nth(end + 1)
            .or(Some((self.input.len(), '\0')))
            .unwrap();
        self.input[start..end].to_string()
    }

    /// Read a number from the input string.
    ///
    /// This can be an integer or a floating point number.
    fn read_number(&mut self) -> String {
        let position = self.position;
        // Read all numbers

        while is_digit(self.peek_char()) {
            self.read_char();
        }

        // If the next character is a dot, and the second next character is a digit,
        // then we have a floating point number.
        if self.peek_char() == '.' && is_digit(self.peek_second_char()) {
            self.read_char();
            while is_digit(self.peek_char()) {
                self.read_char();
            }
        }

        self.get_string(position, self.position)
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn peek_second_char(&mut self) -> char {
        if self.read_position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position + 1).unwrap()
        }
    }

    /// Get the next token from the input string.
    ///
    /// This will skip whitespace and return the next token.
    /// If the end of the input string is reached, a single EOF token
    /// will be returned. Ater that, every call to next_token() will
    /// return None.
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.read_position > self.input.chars().count() + 1 {
            return None;
        }

        let line = self.line;
        let column = self.column + 1; // +1 to make it 1-indexed

        let mut token_length = 1;

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
            '+' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignPlus)
                } else {
                    Some(TokenKind::Plus)
                }
            }
            '-' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignMinus)
                } else {
                    Some(TokenKind::Minus)
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareNotEqual)
                } else {
                    Some(TokenKind::Bang)
                }
            }
            '~' => Some(TokenKind::BitwiseNot),
            '/' => {
                // Support comments
                if self.peek_char() == '/' {
                    self.read_char();
                    while self.ch != '\n' && self.read_position < self.input.len() {
                        self.read_char();
                    }
                    match self.next_token() {
                        Some(token) => return Some(token),
                        None => return None,
                    }
                // Support multiline comments
                } else if self.peek_char() == '*' {
                    self.read_char();
                    while !(self.ch == '*' && self.peek_char() == '/')
                        && self.read_position < self.input.len()
                    {
                        self.read_char();
                    }
                    self.read_char();
                    self.read_char();
                    match self.next_token() {
                        Some(token) => return Some(token),
                        None => return None,
                    }
                } else if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignDivide)
                } else {
                    Some(TokenKind::Divide)
                }
            }
            '*' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignMultiply)
                } else {
                    Some(TokenKind::Multiply)
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareLessEqual)
                } else if self.peek_char() == '<' {
                    self.read_char();
                    Some(TokenKind::BitwiseLeftShift)
                } else {
                    Some(TokenKind::CompareLess)
                }
            }
            '%' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignModulo)
                } else {
                    Some(TokenKind::Modulo)
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::CompareGreaterEqual)
                } else if self.peek_char() == '>' {
                    self.read_char();
                    Some(TokenKind::BitwiseRightShift)
                } else {
                    Some(TokenKind::CompareGreater)
                }
            }
            '&' => {
                if self.peek_char() == '&' {
                    self.read_char();
                    Some(TokenKind::LogicalAnd)
                } else if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignBitwiseAnd)
                } else {
                    Some(TokenKind::BitwiseAnd)
                }
            }
            '|' => {
                if self.peek_char() == '|' {
                    self.read_char();
                    Some(TokenKind::LogicalOr)
                } else if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignBitwiseOr)
                } else {
                    Some(TokenKind::BitwiseOr)
                }
            }
            '^' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Some(TokenKind::AssignBitwiseXor)
                } else {
                    Some(TokenKind::BitwiseXor)
                }
            }
            '.' => {
                if self.peek_char() == '.' && self.peek_second_char() == '.' {
                    self.read_char();
                    self.read_char();
                    Some(TokenKind::Ellipsis)
                } else if self.peek_char() == '.' && self.peek_second_char() == '=' {
                    self.read_char();
                    self.read_char();
                    Some(TokenKind::RangeInclusive)
                } else if self.peek_char() == '.' {
                    self.read_char();
                    Some(TokenKind::RangeExclusive)
                } else {
                    Some(TokenKind::Dot)
                }
            }
            '{' => Some(TokenKind::LeftSquirly),
            '}' => Some(TokenKind::RightSquirly),
            '[' => Some(TokenKind::LeftSquare),
            ']' => Some(TokenKind::RightSquare),
            '\0' => Some(TokenKind::EOF),
            '\"' => {
                let literal = match self.read_string() {
                    Ok(literal) => literal,
                    Err(err) => {
                        eprintln!("Error while lexing input: {}", err);
                        return None;
                    }
                };
                token_length = literal.len();
                Some(TokenKind::String(literal))
            }
            _ => {
                if is_letter(self.ch) || self.ch == '_' {
                    let literal = self.read_identifier();
                    token_length = literal.len();
                    if let Some(keyword_token) = TokenKind::is_keyword(&literal) {
                        Some(keyword_token)
                    } else {
                        Some(TokenKind::Identifier(literal))
                    }
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    token_length = literal.len();
                    Some(TokenKind::Number(literal))
                } else {
                    Some(TokenKind::Illegal(self.ch.to_string()))
                }
            }
        };

        self.read_char();

        token_kind.map(|kind| Token {
            kind,
            position: Position {
                line,
                column,
                length: token_length,
                file: self.file.clone(),
            },
        })
    }

    pub fn lines(&self) -> Vec<String> {
        self.input.lines().map(|s| s.to_string()).collect()
    }

    fn read_string(&mut self) -> Result<String, String> {
        let position = self.position + 1;
        // allow for escaped quotes
        while !(self.peek_char() == '"' && self.ch != '\\') {
            if self.ch == '\0' {
                return Err("Unterminated string".to_string());
            }
            self.read_char();
        }

        let end_position = self.position;
        // current char is the closing quote
        self.read_char();

        Ok(self
            .get_string(position, end_position)
            // replace escaped quotes with regular quotes
            .replace("\\\"", "\"")
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r"))
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::tokens::{Position, TokenKind},
        Token,
    };

    use super::Lexer;

    fn lex_token_kinds(input: &str) -> Vec<TokenKind> {
        Lexer::new(input).map(|token| token.kind).collect()
    }

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input).collect()
    }

    #[test]
    fn test_next_token_kind() {
        let tests = vec![
            (
                "let five = 5;",
                vec![
                    TokenKind::Let,
                    TokenKind::Identifier("five".to_string()),
                    TokenKind::AssignEqual,
                    TokenKind::Number("5".to_string()),
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            (
                "let ten = 10;",
                vec![
                    TokenKind::Let,
                    TokenKind::Identifier("ten".to_string()),
                    TokenKind::AssignEqual,
                    TokenKind::Number("10".to_string()),
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            (
                "let add = fn(x, y) { x + y; };",
                vec![
                    TokenKind::Let,
                    TokenKind::Identifier("add".to_string()),
                    TokenKind::AssignEqual,
                    TokenKind::Fn,
                    TokenKind::LeftParen,
                    TokenKind::Identifier("x".to_string()),
                    TokenKind::Comma,
                    TokenKind::Identifier("y".to_string()),
                    TokenKind::RightParen,
                    TokenKind::LeftSquirly,
                    TokenKind::Identifier("x".to_string()),
                    TokenKind::Plus,
                    TokenKind::Identifier("y".to_string()),
                    TokenKind::Semicolon,
                    TokenKind::RightSquirly,
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            (
                "let result = add(five, ten);",
                vec![
                    TokenKind::Let,
                    TokenKind::Identifier("result".to_string()),
                    TokenKind::AssignEqual,
                    TokenKind::Identifier("add".to_string()),
                    TokenKind::LeftParen,
                    TokenKind::Identifier("five".to_string()),
                    TokenKind::Comma,
                    TokenKind::Identifier("ten".to_string()),
                    TokenKind::RightParen,
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            // This starts a multiline comment
            ("!-/*5;", vec![TokenKind::Bang, TokenKind::Minus]),
            (
                "5 < 10 > 5;",
                vec![
                    TokenKind::Number("5".to_string()),
                    TokenKind::CompareLess,
                    TokenKind::Number("10".to_string()),
                    TokenKind::CompareGreater,
                    TokenKind::Number("5".to_string()),
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            (
                "5 <= 10 >= 5;",
                vec![
                    TokenKind::Number("5".to_string()),
                    TokenKind::CompareLessEqual,
                    TokenKind::Number("10".to_string()),
                    TokenKind::CompareGreaterEqual,
                    TokenKind::Number("5".to_string()),
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            (
                "let a = [1, 2, 3];",
                vec![
                    TokenKind::Let,
                    TokenKind::Identifier("a".to_string()),
                    TokenKind::AssignEqual,
                    TokenKind::LeftSquare,
                    TokenKind::Number("1".to_string()),
                    TokenKind::Comma,
                    TokenKind::Number("2".to_string()),
                    TokenKind::Comma,
                    TokenKind::Number("3".to_string()),
                    TokenKind::RightSquare,
                    TokenKind::Semicolon,
                    TokenKind::EOF,
                ],
            ),
            (
                r#"for i in 0..10 {
                    print(i);
                }"#,
                vec![
                    TokenKind::For,
                    TokenKind::Identifier("i".to_string()),
                    TokenKind::In,
                    TokenKind::Number("0".to_string()),
                    TokenKind::RangeExclusive,
                    TokenKind::Number("10".to_string()),
                    TokenKind::LeftSquirly,
                    TokenKind::Identifier("print".to_string()),
                    TokenKind::LeftParen,
                    TokenKind::Identifier("i".to_string()),
                    TokenKind::RightParen,
                    TokenKind::Semicolon,
                    TokenKind::RightSquirly,
                    TokenKind::EOF,
                ],
            ),
            (
                "let range = 1..=5",
                vec![
                    TokenKind::Let,
                    TokenKind::Identifier("range".to_string()),
                    TokenKind::AssignEqual,
                    TokenKind::Number("1".to_string()),
                    TokenKind::RangeInclusive,
                    TokenKind::Number("5".to_string()),
                    TokenKind::EOF,
                ],
            ),
            (
                r#"for i in 0..=10 {
                    if i % 2 == 0 {
                        continue;
                    }
                    print(i);
                    if i == 7 {
                        break;
                    }
                }"#,
                vec![
                    TokenKind::For,
                    TokenKind::Identifier("i".to_string()),
                    TokenKind::In,
                    TokenKind::Number("0".to_string()),
                    TokenKind::RangeInclusive,
                    TokenKind::Number("10".to_string()),
                    TokenKind::LeftSquirly,
                    TokenKind::If,
                    TokenKind::Identifier("i".to_string()),
                    TokenKind::Modulo,
                    TokenKind::Number("2".to_string()),
                    TokenKind::CompareEqual,
                    TokenKind::Number("0".to_string()),
                    TokenKind::LeftSquirly,
                    TokenKind::Continue,
                    TokenKind::Semicolon,
                    TokenKind::RightSquirly,
                    TokenKind::Identifier("print".to_string()),
                    TokenKind::LeftParen,
                    TokenKind::Identifier("i".to_string()),
                    TokenKind::RightParen,
                    TokenKind::Semicolon,
                    TokenKind::If,
                    TokenKind::Identifier("i".to_string()),
                    TokenKind::CompareEqual,
                    TokenKind::Number("7".to_string()),
                    TokenKind::LeftSquirly,
                    TokenKind::Break,
                    TokenKind::Semicolon,
                    TokenKind::RightSquirly,
                    TokenKind::RightSquirly,
                    TokenKind::EOF,
                ],
            ),
        ];

        for (input, expected_tokens) in tests {
            let tokens = lex_token_kinds(input);
            assert_eq!(tokens, expected_tokens);
        }
    }

    #[test]
    fn test_lexer_iter() {
        let input = "let five = 5;";
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("five".to_string()),
            TokenKind::AssignEqual,
            TokenKind::Number("5".to_string()),
            TokenKind::Semicolon,
            TokenKind::EOF,
        ];

        let lexer = Lexer::new(input);
        let tokens: Vec<TokenKind> = lexer.map(|token| token.kind).collect();

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_comments() {
        let input = r#"
            // this is a comment
            let five = 5; // this is another comment
            // this is a third comment
            /* 
        *
        * this is a block comment
        *
        */
                let ten = 10;
        "#;

        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("five".to_string()),
            TokenKind::AssignEqual,
            TokenKind::Number("5".to_string()),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Identifier("ten".to_string()),
            TokenKind::AssignEqual,
            TokenKind::Number("10".to_string()),
            TokenKind::Semicolon,
            TokenKind::EOF,
        ];

        let tokens = lex_token_kinds(input);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_funky_unicode() {
        let input = r#"
            "いろはにほへとちりぬるを"
            "👍🏿👍🏿"
            "๏ เป็นมนุษย์สุดประเสริฐเลิศคุณค่า"
            " Γαζέες καὶ μυρτιὲς δὲν θὰ βρῶ πιὰ στὸ χρυσαφὶ ξέφωτο"
            にほへとちり = 5;
        "#;

        let expected_tokens = vec![
            TokenKind::String("いろはにほへとちりぬるを".to_string()),
            TokenKind::String("👍🏿👍🏿".to_string()),
            TokenKind::String("๏ เป็นมนุษย์สุดประเสริฐเลิศคุณค่า".to_string()),
            TokenKind::String(" Γαζέες καὶ μυρτιὲς δὲν θὰ βρῶ πιὰ στὸ χρυσαφὶ ξέφωτο".to_string()),
            TokenKind::Identifier("にほへとちり".to_string()),
            TokenKind::AssignEqual,
            TokenKind::Number("5".to_string()),
            TokenKind::Semicolon,
            TokenKind::EOF,
        ];

        let tokens = lex_token_kinds(input);
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_token_position() {
        let input = r#"
            let five = 5;
                    let ten = 10;
            let add = 


                    fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        "#;

        let expected_tokens = vec![
            Token {
                kind: TokenKind::Let,
                position: Position {
                    line: 2,
                    column: 13,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("five".to_string()),
                position: Position {
                    line: 2,
                    column: 17,
                    length: 4,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::AssignEqual,
                position: Position {
                    line: 2,
                    column: 22,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Number("5".to_string()),
                position: Position {
                    line: 2,
                    column: 24,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                position: Position {
                    line: 2,
                    column: 25,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Let,
                position: Position {
                    line: 3,
                    column: 21,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("ten".to_string()),
                position: Position {
                    line: 3,
                    column: 25,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::AssignEqual,
                position: Position {
                    line: 3,
                    column: 29,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Number("10".to_string()),
                position: Position {
                    line: 3,
                    column: 31,
                    length: 2,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                position: Position {
                    line: 3,
                    column: 33,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Let,
                position: Position {
                    line: 4,
                    column: 13,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("add".to_string()),
                position: Position {
                    line: 4,
                    column: 17,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::AssignEqual,
                position: Position {
                    line: 4,
                    column: 21,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Fn,
                position: Position {
                    line: 7,
                    column: 21,
                    length: 2,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::LeftParen,
                position: Position {
                    line: 7,
                    column: 23,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("x".to_string()),
                position: Position {
                    line: 7,
                    column: 24,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Comma,
                position: Position {
                    line: 7,
                    column: 25,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("y".to_string()),
                position: Position {
                    line: 7,
                    column: 27,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::RightParen,
                position: Position {
                    line: 7,
                    column: 28,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::LeftSquirly,
                position: Position {
                    line: 7,
                    column: 30,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("x".to_string()),
                position: Position {
                    line: 8,
                    column: 17,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Plus,
                position: Position {
                    line: 8,
                    column: 19,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("y".to_string()),
                position: Position {
                    line: 8,
                    column: 21,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                position: Position {
                    line: 8,
                    column: 22,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::RightSquirly,
                position: Position {
                    line: 9,
                    column: 13,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                position: Position {
                    line: 9,
                    column: 14,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Let,
                position: Position {
                    line: 10,
                    column: 13,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("result".to_string()),
                position: Position {
                    line: 10,
                    column: 17,
                    length: 6,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::AssignEqual,
                position: Position {
                    line: 10,
                    column: 24,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("add".to_string()),
                position: Position {
                    line: 10,
                    column: 26,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::LeftParen,
                position: Position {
                    line: 10,
                    column: 29,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("five".to_string()),
                position: Position {
                    line: 10,
                    column: 30,
                    length: 4,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Comma,
                position: Position {
                    line: 10,
                    column: 34,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("ten".to_string()),
                position: Position {
                    line: 10,
                    column: 36,
                    length: 3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::RightParen,
                position: Position {
                    line: 10,
                    column: 39,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                position: Position {
                    line: 10,
                    column: 40,
                    length: 1,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::EOF,
                position: Position {
                    line: 11,
                    column: 9,
                    length: 1,
                    file: None,
                },
            },
        ];

        let tokens = lex(input);

        for (i, token) in tokens.iter().enumerate() {
            println!("{:?}", token);
            assert_eq!(token.kind, expected_tokens[i].kind);
            assert_eq!(token.position, expected_tokens[i].position);
        }
    }
}
