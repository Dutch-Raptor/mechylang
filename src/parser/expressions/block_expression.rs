use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Error, Token, trace};
use crate::errors::ErrorKind;
use crate::lexer::tokens::TokenKind;
use crate::parser::Parser;
use crate::parser::statements::Statement;

/// Represents a block of code in Mechylang, which is a sequence of statements enclosed in curly braces.
///
/// A `BlockExpression` is used to group multiple statements together and is commonly used to
/// represent the body of functions, control structures (like `if`, `for`, etc.), and other
/// constructs that contain a sequence of statements. A block expression can however also be used
/// to group expressions, which is useful for expressions that return a value.
///
/// # Fields
///
/// * `token` - The token representing the opening curly brace `{` that starts the block. This token
///            is used for error reporting and debugging purposes.
/// * `statements` - A sequence of `Statement` instances contained within the block. The statements
///                  are stored in an `Rc<[Statement]>`, which is a reference-counted smart pointer
///                  to a slice, allowing multiple parts of the code to share the same statements
///                  without unnecessary copying.
///
/// # Notes
///
/// The `BlockExpression` struct is used internally by the parser to represent blocks of code. 
/// The `statements` field is a collection of `Statement` enums, which can represent different types
/// of statements such as variable declarations, expressions, and control flow commands.
///
/// It is crucial for understanding how the parser organizes and processes blocks of code in Mechylang.
/// 
/// # Example usages
/// 
/// ## Block expression as a function body
/// ```
/// use mechylang::test_utils::test_parse_ok;
/// test_parse_ok(r#"
/// fn add(a, b) {
///     return a + b;
/// }
/// "#);
/// ```
/// 
/// ## Block expression as an expression
/// ```
/// use mechylang::test_utils::test_parse_ok;
/// test_parse_ok(r#"
/// let result = {
///     let x = 5;
///     let y = 10;
///     x + y;
/// };
/// // result is now 15
/// "#);
/// ```
/// 
/// ## Block expression in a for loop
/// ```
/// use mechylang::test_utils::test_parse_ok;
/// test_parse_ok(r#"
/// for i in 1..5 {
///     sum += i;
/// }
/// "#);
/// ```
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockExpression {
    pub token: Token,
    pub statements: Rc<[Statement]>,
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for statement in self.statements.iter() {
            writeln!(f, "\t{}", statement)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl Parser {
    /// Parses a block expression in Mechylang.
    ///
    /// This function handles the parsing of a block expression, which is a sequence of statements enclosed
    /// in curly braces (`{}`). It reads statements from the lexer until it encounters a closing curly brace
    /// (`}`) or the end of the file (EOF). The block expression is represented by a `BlockExpression` structure.
    ///
    /// # Returns
    ///
    /// Returns a `Result` which contains:
    /// - `Ok(BlockExpression)` if the block expression was successfully parsed.
    /// - `Err(Error)` if there was an error during parsing, such as an unexpected token.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// - The current token is not a left curly brace (`{`) at the beginning.
    /// - An unexpected token is encountered while parsing the statements.
    /// - The closing curly brace (`}`) is missing or misplaced.
    pub(in crate::parser) fn parse_block_expression(&mut self) -> Result<BlockExpression, Error> {
        let _trace = trace!("parse_block_expression");
        debug_assert!(self.is_cur_token(TokenKind::LeftSquirly), "Expected current token to be `{{`");
        self.next_token();
        let token = self.cur_token.clone();

        let mut statements = Vec::new();

        while self.cur_token.kind != TokenKind::RightSquirly
            && self.cur_token.kind != TokenKind::EOF
        {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }

        if self.cur_token.kind != TokenKind::RightSquirly {
            return Err(self.error_current(
                ErrorKind::UnexpectedToken,
                format!("Expected `}}`, got {:?}", self.cur_token.kind),
            ));
        }

        Ok(BlockExpression { token, statements: statements.into() })
    }
}


#[cfg(test)]
mod tests {
    use crate::errors::ErrorKind;
    use crate::Parser;
    use crate::parser::expressions::block_expression::BlockExpression;
    use crate::parser::statements::expression_statement::ExpressionStatement;
    use crate::parser::statements::Statement;
    
   /// Test parsing a block expression with multiple statements
    #[test]
    fn test_parse_block_expression_multiple_statements() {
        let source_code = r#"
        {
            let x = 10;
            let y = 20;
            x + y;
        }
        "#;

        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_block_expression();
        assert!(result.is_ok());

        if let Ok(BlockExpression { statements, .. }) = result {
            assert_eq!(statements.len(), 3);
            assert!(matches!(statements[0], Statement::Let(_)));
            assert!(matches!(statements[1], Statement::Let(_)));
            assert!(matches!(statements[2], Statement::Expression(ExpressionStatement { .. })));
        } else {
            panic!("Expected BlockExpression but found {:?}", result);
        }
    } 

    /// Test parsing a block expression with a single statement
    #[test]
    fn test_parse_block_expression_single_statement() {
        let source_code = r#"
        {
            let x = 42;
        }
        "#;

        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_block_expression();
        assert!(result.is_ok());

        if let Ok(BlockExpression { statements, .. }) = result {
            assert_eq!(statements.len(), 1);
            assert!(matches!(statements[0], Statement::Let(_)));
        } else {
            panic!("Expected BlockExpression but found {:?}", result);
        }
    }
    
    /// Test parsing a block expression with no statements
    #[test]
    fn test_parse_block_expression_empty() {
        let source_code = r#"
        {}
        "#;

        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_block_expression();
        assert!(result.is_ok());

        if let Ok(BlockExpression { statements, .. }) = result {
            assert_eq!(statements.len(), 0);
        } else {
            panic!("Expected BlockExpression but found {:?}", result);
        }
    }
    
    /// Test parsing a block expression with missing closing brace
    #[test]
    fn test_parse_block_expression_missing_closing_brace() {
        let source_code = r#"
        {
            let x = 10;
        "#; // Missing closing brace

        let mut parser = Parser::from_source(source_code);
        let result = parser.parse_block_expression();
        assert!(result.is_err());

        if let Err(error) = result {
            assert_eq!(error.kind, ErrorKind::UnexpectedToken);
        } else {
            panic!("Expected error but found {:?}", result);
        }
    }
}
