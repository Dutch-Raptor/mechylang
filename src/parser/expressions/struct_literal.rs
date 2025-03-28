use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::{Expression, Parser, Span, TokenKind};
use crate::parser::expressions::Precedence;
use crate::parser::{Error, Result};
use crate::parser::error::Location;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StructLiteral {
    pub span: Span,
    pub entries: HashMap<String, Expression>,
}

impl Display for StructLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {{")?;
        for (key, value) in &self.entries {
            writeln!(f, "{}: {}", key, value)?;
        }
        writeln!(f, "}}")
    }
}

impl Parser<'_> {
    /// Parses a struct literal
    ///
    /// valid syntax is:
    /// ```text
    /// struct {
    ///     key: value,
    ///     otherKey: 3,
    /// }
    /// ```
    ///
    /// Trailing commas are optional
    pub(super) fn parse_struct_literal(&mut self) -> Result<StructLiteral> {
        debug_assert!(self.is_cur_token(TokenKind::Struct), "Expected current token to be `struct`");
        let start = self.cur_token.span.clone();

        self.expect_peek(TokenKind::LeftSquirly, Some(Location::StructLiteral))?;

        // move past the opening brace
        self.next_token()?;

        let mut entries = HashMap::new();

        while self.cur_token.kind != TokenKind::RightSquirly {
            let key = self.cur_token.kind
                .as_identifier()
                .ok_or(Error::InvalidStructKey {
                    span: self.cur_token.span.clone(),
                    found: self.cur_token.kind.clone(),
                })?
                .to_string();

            // ensure key is followed by a colon
            self.expect_peek(TokenKind::Colon, Some(Location::StructLiteral))?;
            // move past the colon
            self.next_token()?;

            let value = self.parse_expression(Precedence::Lowest)?;
            entries.insert(key, value);

            // move past the value
            self.next_token()?;

            if self.cur_token.kind != TokenKind::Comma && self.cur_token.kind != TokenKind::RightSquirly {
                return Err(Error::UnexpectedToken {
                    span: self.cur_token.span.clone(),
                    expected: vec![TokenKind::Comma, TokenKind::RightSquirly],
                    found: self.cur_token.kind.clone(),
                    location: Some(Location::StructLiteral),
                });
            }

            if self.cur_token.kind == TokenKind::Comma {
                self.next_token()?;
            }
        }

        self.expect_current(TokenKind::RightSquirly, Some(Location::StructLiteral))?;

        Ok(StructLiteral {
            span: self.span_with_start(&start),
            entries,
        })
    }
}
#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::expressions::identifier::Identifier;
    use crate::parser::expressions::number_expressions::IntegerLiteral;
    use crate::parser::expressions::string_literal::StringLiteral;
    use crate::parser::expressions::struct_literal::StructLiteral;
    use crate::parser::statements::{LetStatement, Statement};
    use crate::parser::tests::parse;

    #[test]
    fn test_struct_literal() {
        let input = r#"
            let object = struct {
                a: 1,
                b: "Some String"
            }
        "#;

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        if let Statement::Let(LetStatement {
                                  name: Identifier {
                                      value: name,
                                      ..
                                  },
                                  value: Expression::StructLiteral(StructLiteral { entries, .. }),
                                  ..
                              }) = &statements[0] {
            assert_eq!(name.as_ref(), "object");

            match entries.get("a") {
                Some(Expression::IntegerLiteral(IntegerLiteral { value: 1, .. })) => {}
                _ => panic!("Key 'a' should contain integer 1")
            }
            match entries.get("b") {
                Some(Expression::StringLiteral(StringLiteral { value, .. })) if value.as_ref() == "Some String" => {}
                _ => panic!("Key 'b' should contain string `Some String`")
            }
        }
    }
}
