use std::rc::Rc;
use crate::{Error, Lexer, Token};
use crate::errors::InterpreterErrors;
use crate::lexer::tokens::TokenKind;
use crate::parser::program::Program;
use crate::tracer::reset_trace;

pub mod expressions;
pub mod statements;
pub mod program;
pub mod errors;
pub mod tokens;

/// The `Parser` struct is responsible for parsing the source code into an abstract syntax tree (AST).
/// It uses a lexer to tokenize the input and processes these tokens to produce the AST.
#[derive(Debug)]
pub struct Parser {
    /// The lexer used to tokenize the source code.
    lexer: Lexer,
    /// The current token being processed.
    pub cur_token: Token,
    /// The next token to be processed.
    pub peek_token: Token,
    /// A list of errors encountered during parsing.
    errors: Vec<Error>,
    /// The lines of source code being parsed, used for error reporting.
    lines: Rc<[String]>,
}

impl Parser {
    /// Creates a new `Parser` instance.
    ///
    /// This method initializes a new `Parser` with the given `Lexer`. It reads two tokens initially
    /// to set both `cur_token` and `peek_token`.
    ///
    /// # Arguments
    ///
    /// * `lexer` - The lexer instance used for tokenizing the input.
    ///
    /// # Returns
    ///
    /// A new `Parser` instance with the initial state set.
    ///
    /// # Example
    ///
    /// ```
    /// use mechylang::{Lexer, Parser};
    /// let source_code = r#"print("hello, world!")"#;
    /// let lexer = Lexer::new(source_code);
    /// let mut parser = Parser::new(lexer);
    /// let program = parser.parse().unwrap();
    /// ```
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lines: lexer.lines(),
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: vec![],
        };

        reset_trace();

        // Read two tokens, so cur_token and peek_token are both set
        parser.next_token();
        parser.next_token();
        parser
    }
    
    /// Creates a new `Parser` instance from the provided source code.
    ///
    /// This method initializes a new `Parser` with the given source code by first creating a
    /// `Lexer` from the source and then using it to create the `Parser`.
    ///
    /// # Arguments
    ///
    /// * `src` - The source code as a string reference that will be tokenized and parsed.
    ///
    /// # Returns
    ///
    /// A new `Parser` instance with the initial state set.
    /// 
    /// # Example
    /// ```
    /// use mechylang::Parser;
    /// let source_code = r#"print("hello, world!")"#;
    /// let mut parser = Parser::from_source(source_code);
    /// let program = parser.parse().unwrap();
    /// ```
    pub fn from_source(src: impl AsRef<str>) -> Self {
        Parser::new(Lexer::new(src))
    }
    
    /// Parses the input source code into a `Program` structure.
    ///
    /// This method repeatedly reads tokens from the lexer and attempts to parse them into statements
    /// until the end of the file (`EOF`) is reached. Each successfully parsed statement is collected
    /// into a `Program` instance.
    ///
    /// # Returns
    ///
    /// A `Result` which is:
    /// * `Ok(Program)` containing the list of parsed statements if successful.
    /// * `Err(InterpreterErrors)` containing any errors encountered during parsing.
    ///
    /// # Errors
    ///
    /// If any parsing error occurs, the method returns an `Err` variant with an `InterpreterErrors` 
    /// containing the encountered error(s).
    ///
    /// # Example
    ///
    /// ```
    /// use mechylang::{Lexer, Parser};
    /// let source_code = r#"print("hello, world!")"#;
    /// let mut parser = Parser::from_source(source_code);
    /// match parser.parse() {
    ///     Ok(program) => {
    ///         // Process the parsed program
    ///         println!("{}", program);
    ///     }
    ///     Err(errors) => {
    ///         // Handle parsing errors
    ///         for error in errors {
    ///             println!("Error: {}", error);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn parse(&mut self) -> Result<Program, InterpreterErrors> {
        let mut statements = Vec::new();

        while self.cur_token.kind != TokenKind::EOF {
            let statement = self.parse_statement()
                .map_err(|err| InterpreterErrors(vec![err]))?;
            statements.push(statement);
            self.next_token();
        }

        Ok(Program { statements })
    }
}

#[cfg(test)]
mod tests {
    use color_print::cprintln;
    use crate::errors::InterpreterErrors;
    use crate::parser::Parser;
    use crate::parser::statements::Statement;

    pub(super) fn parse(input: &str) -> Result<Vec<Statement>, InterpreterErrors> {
        let mut parser = Parser::from_source(input);

        let result = parser.parse();

        if let Err(ref err) = result {
            cprintln!("{}", err)
        }

        result.map(|program| program.statements)
    }
}



