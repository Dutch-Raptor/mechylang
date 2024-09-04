use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::{Expression, Parser, Span, TokenKind};
use crate::parser::{Result};
use crate::parser::expressions::{ExpressionSpanExt, Identifier};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct MemberExpression {
    pub span: Span,
    pub object: Rc<Expression>,
    pub property: Identifier,
}

impl Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.object, self.property)
    }
}
impl Parser {
    /// Parses a member access expression in Mechylang, where an object is accessed by its property.
    ///
    /// This function is used to parse member access expressions, where an object is followed by a dot `.` and then
    /// an identifier representing the property or method being accessed. For example, in the expression `object.property`,
    /// `object` is the left expression and `property` is the member being accessed.
    ///
    /// # Arguments
    ///
    /// * `left` - The left side of the member access expression, which is the object from which the property is being accessed.
    ///
    /// # Returns
    ///
    /// Returns a `Result` which is:
    /// * `Ok(MemberExpression)` if the member access was successfully parsed. This includes:
    ///   - `token`: The token representing the dot `.` in the member access expression.
    ///   - `object`: The left expression which is being accessed.
    ///   - `property`: The identifier of the property being accessed.
    /// * `Err(Error)` if there was an error during parsing, such as an unexpected token or missing identifier.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// * The next token is not an identifier, which indicates that the member access syntax is incorrect.
    /// * An unexpected token is encountered after the dot `.` symbol.
    pub(super) fn parse_member(&mut self, left: Expression) -> Result<MemberExpression> {
        debug_assert!(self.is_cur_token(TokenKind::Dot), "Expected current token to be `.`");
        let start = left.span().start.clone();
        self.next_token()?;

        let property = self.parse_identifier()?;

        Ok(MemberExpression {
            span: self.span_with_start(start),
            object: Rc::new(left),
            property,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::expressions::number_expressions::IntegerLiteral;
    use crate::parser::statements::{ExpressionStatement, Statement};
    use crate::parser::tests::parse;

    #[test]
    fn test_method_call() {
        let input = "foo.bar(1, 2, 3);";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        let call_expr = match statements[0] {
            Statement::Expression(
                ExpressionStatement {
                    expression:
                    Expression::Call(ref call_expr), ..
                }) => call_expr,
            _ => panic!("Expected a call expression")
        };

        let method = match call_expr.function.as_ref() {
            Expression::Member(ref member) => member,
            _ => panic!("Expected call on member function"),
        };

        match method.object.as_ref() {
            Expression::Identifier(ident) => assert_eq!(ident.value.as_ref(), "foo"),
            _ => panic!("foo should be an identifier")
        };

        assert_eq!(method.property.value.as_ref(), "bar");

        for i in 1..=3 {
            if let Expression::IntegerLiteral(IntegerLiteral { value, .. }) = call_expr.arguments[i - 1] {
                assert_eq!(value, i as i64);
            } else {
                panic!("Arguments should be integer literals");
            }
        }
    }
}