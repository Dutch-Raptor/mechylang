use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::parser::expressions::Expression;
use crate::parser::expressions::identifier::Identifier;
use crate::parser::Parser;
use crate::{Error, Token};
use crate::errors::ErrorKind;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct MemberExpression {
    pub token: Token,
    pub object: Rc<Expression>,
    pub property: Identifier,
}

impl Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.object, self.property)
    }
}
impl Parser {
    pub(super) fn parse_member(&mut self, left: Expression) -> Result<Expression, Error> {
        self.next_token();
        let token = self.cur_token.clone();

        let property = match self.parse_identifier()? {
            Expression::Identifier(ident) => ident,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a property (identifier), got {:?}", self.cur_token),
                ))
            }
        };

        Ok(Expression::Member(MemberExpression {
            token,
            object: Rc::new(left),
            property,
        }))
    }

}

#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::expressions::number_expressions::IntegerLiteral;
    use crate::parser::statements::expression_statement::ExpressionStatement;
    use crate::parser::statements::Statement;
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
            _ => panic!("Expected a call expresssion")
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