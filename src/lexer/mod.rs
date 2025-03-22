mod tokens;
mod error;

use std::sync::Arc;
pub use tokens::{Token, TokenKind, Span};
pub use error::{Error, Result};

#[derive(Debug)]
pub struct Lexer<'a> {
    whole: &'a str,
    rest: &'a str,
    byte: usize,
    file: Option<Arc<str>>,
}


/// Implement the Lexer struct
///
/// The Lexer struct is responsible for lexing the input string into tokens.
impl Lexer<'_> {
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
    /// let lexer = Lexer::new(&input);
    /// ```
    pub fn new(input: &str) -> Lexer {
        Lexer {
            whole: input,
            rest: input,
            byte: 0,
            file: None,
        }
    }

    /// Keeps advancing the lexer until it finds a non-whitespace character
    fn skip_whitespace(&mut self) {
        let first_non_whitespace = self.rest.find(|c: char| !c.is_whitespace()).unwrap_or(self.rest.len());
        self.rest = &self.rest[first_non_whitespace..];
        self.byte += first_non_whitespace;
    }

    /// Read an identifier from the current position. An identifier is a sequence of letters, digits and underscores.
    fn read_identifier(&mut self) -> String {
        let first_non_ident = self.rest.find(|c: char| !(c.is_alphabetic() || c == '_' || c.is_numeric()))
            .unwrap_or(self.rest.len());
        let start_byte = self.byte;
        let end_byte = self.byte + first_non_ident;

        // update positions
        self.byte += first_non_ident;
        self.rest = &self.rest[first_non_ident..];

        self.whole[start_byte..end_byte].to_string()
    }

    /// Read a number from the input string.
    ///
    /// This can be an integer or a floating point number.
    ///
    /// Underscores are allowed in numbers, but they are ignored.
    fn read_number(&mut self) -> String {
        const PERIOD_LEN_BYTES: usize = '.'.len_utf8();
        let start_byte = self.byte;

        let first_non_digit = self.rest
            .find(|c| !matches!(c, '0'..='9' | '_'))
            .unwrap_or(self.rest.len());

        self.byte += first_non_digit;
        self.rest = &self.rest[first_non_digit..];

        // If the next character is a dot, and the second next character is a digit,
        // then we have a floating point number.
        let c = self.rest.chars().next().unwrap_or('\0');
        if c == '.' && self.peek_char().is_ascii_digit() {
            // Here we skip the first char in self.rest as it as a `.`
            let second_non_digit = self.rest[PERIOD_LEN_BYTES..]
                .find(|c| !matches!(c, '0'..='9' | '_'))
                .map(|at| at + PERIOD_LEN_BYTES) // add the extra byte for `.`
                .unwrap_or(self.rest.len());

            self.byte += second_non_digit;
            self.rest = &self.rest[second_non_digit..];
        }

        self.whole[start_byte..self.byte].replace("_", "")
    }

    /// Peeks the 2nd char in `self.rest`, if None, reads '\0'
    fn peek_char(&mut self) -> char {
        self.rest.chars().nth(1).unwrap_or('\0')
    }


    /// Peeks the 3rd char in `self.rest`, if None, reads '\0'
    fn peek_second_char(&mut self) -> char {
        self.rest.chars().nth(2).unwrap_or('\0')
    }


    /// Advances the lexer by the length of `token_kind` and returns `token_kind`
    ///
    /// # Panics:
    /// - if `token_kind` is a keyword, identifier, number or string
    fn read_token(&mut self, token_kind: TokenKind) -> TokenKind {
        let len = token_kind.length_bytes().expect("Expected token to have a const known length");
        self.byte += len;
        self.rest = &self.rest[len..];
        token_kind
    }

    /// Get the next token from the input string.
    ///
    /// This will skip whitespace and return the next token.
    /// If the end of the input string is reached, next_token will return None
    pub fn next_token(&mut self) -> Option<Result<Token>> {
        self.skip_whitespace();

        if self.rest.is_empty() {
            return None;
        }

        let c = self.rest.chars().next()?;
        let c_start = self.byte;


        let token_kind = match c {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::CompareEqual)
                } else {
                    self.read_token(TokenKind::AssignEqual)
                }
            }
            ':' => self.read_token(TokenKind::Colon),
            ';' => self.read_token(TokenKind::Semicolon),
            '(' => self.read_token(TokenKind::LeftParen),
            ')' => self.read_token(TokenKind::RightParen),
            ',' => self.read_token(TokenKind::Comma),
            '+' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignPlus)
                } else {
                    self.read_token(TokenKind::Plus)
                }
            }
            '-' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignMinus)
                } else {
                    self.read_token(TokenKind::Minus)
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::CompareNotEqual)
                } else {
                    self.read_token(TokenKind::Bang)
                }
            }
            '~' => self.read_token(TokenKind::BitwiseNot),
            '/' => {
                // Support comments
                if self.peek_char() == '/' {
                    let line_end = self.rest.find('\n').unwrap_or(self.rest.len());
                    self.byte += line_end;
                    self.rest = &self.rest[line_end..];
                    return self.next_token();
                    // Support multiline comments
                } else if self.peek_char() == '*' {
                    let comment_end = self.rest.find("*/")
                        // Add 2 for the `*` and `/`
                        .map(|i| i + 2)
                        .unwrap_or(self.rest.len());
                    self.byte += comment_end;
                    self.rest = &self.rest[comment_end..];
                    return self.next_token();
                } else if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignDivide)
                } else {
                    self.read_token(TokenKind::Divide)
                }
            }
            '*' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignMultiply)
                } else {
                    self.read_token(TokenKind::Multiply)
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::CompareLessEqual)
                } else if self.peek_char() == '<' {
                    self.read_token(TokenKind::BitwiseLeftShift)
                } else {
                    self.read_token(TokenKind::CompareLess)
                }
            }
            '%' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignModulo)
                } else {
                    self.read_token(TokenKind::Modulo)
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::CompareGreaterEqual)
                } else if self.peek_char() == '>' {
                    self.read_token(TokenKind::BitwiseRightShift)
                } else {
                    self.read_token(TokenKind::CompareGreater)
                }
            }
            '&' => {
                if self.peek_char() == '&' {
                    self.read_token(TokenKind::LogicalAnd)
                } else if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignBitwiseAnd)
                } else {
                    self.read_token(TokenKind::Ampersand)
                }
            }
            '|' => {
                if self.peek_char() == '|' {
                    self.read_token(TokenKind::LogicalOr)
                } else if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignBitwiseOr)
                } else {
                    self.read_token(TokenKind::BitwiseOr)
                }
            }
            '^' => {
                if self.peek_char() == '=' {
                    self.read_token(TokenKind::AssignBitwiseXor)
                } else {
                    self.read_token(TokenKind::BitwiseXor)
                }
            }
            '.' => {
                if self.peek_char() == '.' && self.peek_second_char() == '.' {
                    self.read_token(TokenKind::Ellipsis)
                } else if self.peek_char() == '.' && self.peek_second_char() == '=' {
                    self.read_token(TokenKind::RangeInclusive)
                } else if self.peek_char() == '.' {
                    self.read_token(TokenKind::RangeExclusive)
                } else {
                    self.read_token(TokenKind::Dot)
                }
            }
            '{' => self.read_token(TokenKind::LeftSquirly),
            '}' => self.read_token(TokenKind::RightSquirly),
            '[' => self.read_token(TokenKind::LeftSquare),
            ']' => self.read_token(TokenKind::RightSquare),
            '\0' => return None,
            '\"' => {
                let literal = match self.read_string() {
                    Ok(literal) => literal,
                    Err(err) => {
                        return Some(Err(err));
                    }
                };
                TokenKind::String(literal)
            }
            c if c.is_alphabetic() || c == '_' => {
                let literal = self.read_identifier();
                if let Some(keyword_token) = TokenKind::is_keyword(&literal) {
                    keyword_token
                } else {
                    TokenKind::Identifier(literal)
                }
            }
            c if c.is_ascii_digit() => {
                let literal = self.read_number();
                TokenKind::Number(literal)
            }
            c => {
                self.byte += c.len_utf8();
                self.rest = &self.rest[c.len_utf8()..];
                return Some(Err(Error::IllegalCharacter {
                    span: Span {
                        bytes: c_start..c_start + c.len_utf8(),
                        file: self.file.clone(),
                    },
                    found: c,
                }));
            }
        };

        let token = Token {
            kind: token_kind,
            span: Span {
                bytes: c_start..self.byte,
                file: self.file.clone(),
            },
        };

        Some(Ok(token))
    }

    fn read_string(&mut self) -> Result<String> {
        const QUOTE_CHAR_LEN: usize = '"'.len_utf8();
        let string_start_byte = self.byte;
        let literal_start_byte = self.byte + QUOTE_CHAR_LEN;

        // Read the opening quote
        self.byte += QUOTE_CHAR_LEN;
        self.rest = &self.rest[QUOTE_CHAR_LEN..];

        // allow for escaped quotes
        let literal_length_bytes = {
            let mut chars = self.rest.char_indices();

            loop {
                let (c_at, c) = chars.next().ok_or_else(||
                    Error::UnterminatedString {
                        span: Span {
                            bytes: string_start_byte..self.whole.len(),
                            file: self.file.clone(),
                        }
                    })?;
                if c == '"' {
                    break c_at;
                }
                if c == '\\' {
                    // Make sure we don't count an escaped `"` as the end
                    chars.next();
                }
            }
        };

        let literal_end_byte = literal_start_byte + literal_length_bytes;

        // read rest of string
        self.byte += literal_length_bytes + QUOTE_CHAR_LEN;
        self.rest = &self.rest[(literal_length_bytes + QUOTE_CHAR_LEN)..];

        let escaped_literal = {
            let mut escaped_literal = String::with_capacity(literal_length_bytes);
            let mut literal = self.whole[literal_start_byte..literal_end_byte].char_indices();

            while let Some((c_at, c)) = literal.next() {
                if c != '\\' {
                    escaped_literal.push(c);
                    continue;
                }

                let (c2_at, c2) = literal.next().expect("literal should not end with a `\\`");
                match c2 {
                    '\\' => escaped_literal.push('\\'),
                    '"' => escaped_literal.push('"'),
                    'n' => escaped_literal.push('\n'),
                    'r' => escaped_literal.push('\r'),
                    't' => escaped_literal.push('\t'),
                    _ => return Err(Error::UnsupportedEscapeSequence {
                        span: Span {
                            file: self.file.clone(),
                            bytes: (literal_start_byte + c_at)..(literal_start_byte + c2_at + c2.len_utf8()),
                        }
                    })
                }
            }
            escaped_literal
        };

        Ok(escaped_literal)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::tokens::{TokenKind},
        Token,
    };

    use super::*;

    fn lex_token_kinds(input: &str) -> Result<Vec<TokenKind>> {
        Lexer::new(input).map(|token| token.map(|token| token.kind)).collect()
    }

    fn lex(input: &str) -> Result<Vec<Token>> {
        Lexer::new(input).collect()
    }

    #[test]
    fn test_number_token() {
        let num = lex_token_kinds("123").unwrap();
        assert_eq!(num.as_slice(), [TokenKind::Number("123".into())]);
    }


    #[test]
    fn test_multiple_char_tokens() {
        let tests = vec![
            ("+=", TokenKind::AssignPlus),
            ("-=", TokenKind::AssignMinus),
            ("==", TokenKind::CompareEqual),
            ("!=", TokenKind::CompareNotEqual),
            ("..", TokenKind::RangeExclusive),
            ("..=", TokenKind::RangeInclusive),
        ];

        for (input, expected) in tests {
            let tokens = lex_token_kinds(input).unwrap();
            assert_eq!(tokens.first(), Some(&expected));
        }
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
                ],
            ),
        ];

        for (input, expected_tokens) in tests {
            let tokens = lex_token_kinds(input).unwrap();
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
        ];

        let tokens = lex_token_kinds(input).unwrap();

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
        ];

        let tokens = lex_token_kinds(input).unwrap();
        assert_eq!(tokens, expected_tokens);

        let input = r#"
            assert_eq(1, 1, 1); // ok
        "#;

        let expected_tokens = vec![
            TokenKind::Identifier("assert_eq".to_string()),
            TokenKind::LeftParen,
            TokenKind::Number("1".to_string()),
            TokenKind::Comma,
            TokenKind::Number("1".to_string()),
            TokenKind::Comma,
            TokenKind::Number("1".to_string()),
            TokenKind::RightParen,
            TokenKind::Semicolon,
        ];

        let tokens = lex_token_kinds(input).unwrap();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_string_escape_characters() {
        let input = r#"
        "str with \""
        "\n\\\r\t\""
        "#;

        let expected_tokens = vec![
            TokenKind::String("str with \"".to_string()),
            TokenKind::String("\n\\\r\t\"".to_string()),
        ];

        let token_kinds = lex_token_kinds(input).unwrap();
        assert_eq!(token_kinds, expected_tokens);
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
        ];

        let tokens = lex_token_kinds(input).unwrap();
        assert_eq!(tokens, expected_tokens);
    }


    #[test]
    fn test_numbers() {
        let input = r#"2.6 > -2.9"#;

        let expected_tokens = vec![
            TokenKind::Number("2.6".to_string()),
            TokenKind::CompareGreater,
            TokenKind::Minus,
            TokenKind::Number("2.9".to_string()),
        ];

        let tokens = lex_token_kinds(input).unwrap();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_unicode_alphabetic_identifiers() {
        let input = r#"
            let にほへとちり = 5;
            let Γαζέες καὶ μυρτιὲς δὲν θὰ βρῶ πιὰ στὸ χρυσαφὶ ξέφωτο = 5;
        "#;

        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("にほへとちり".to_string()),
            TokenKind::AssignEqual,
            TokenKind::Number("5".to_string()),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Identifier("Γαζέες".to_string()),
            TokenKind::Identifier("καὶ".to_string()),
            TokenKind::Identifier("μυρτιὲς".to_string()),
            TokenKind::Identifier("δὲν".to_string()),
            TokenKind::Identifier("θὰ".to_string()),
            TokenKind::Identifier("βρῶ".to_string()),
            TokenKind::Identifier("πιὰ".to_string()),
            TokenKind::Identifier("στὸ".to_string()),
            TokenKind::Identifier("χρυσαφὶ".to_string()),
            TokenKind::Identifier("ξέφωτο".to_string()),
            TokenKind::AssignEqual,
            TokenKind::Number("5".to_string()),
            TokenKind::Semicolon,
        ];

        let tokens = lex_token_kinds(input).unwrap();
        assert_eq!(tokens, expected_tokens);
    }


    #[test]
    fn test_token_spans() {
        assert_eq!(lex("let five = 5"), Ok(vec![
            Token {
                kind: TokenKind::Let,
                span: Span {
                    bytes: 0..3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("five".to_string()),
                span: Span {
                    bytes: 4..8,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::AssignEqual,
                span: Span {
                    bytes: 9..10,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Number("5".to_string()),
                span: Span {
                    bytes: 11..12,
                    file: None,
                },
            },
        ]));

        assert_eq!(lex("let にほへとちり = 5"), Ok(vec![
            Token {
                kind: TokenKind::Let,
                span: Span {
                    bytes: 0..3,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Identifier("にほへとちり".to_string()),
                span: Span {
                    bytes: 4..22,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::AssignEqual,
                span: Span {
                    bytes: 23..24,
                    file: None,
                },
            },
            Token {
                kind: TokenKind::Number("5".to_string()),
                span: Span {
                    bytes: 25..26,
                    file: None,
                },
            }
        ]));
    }
}
