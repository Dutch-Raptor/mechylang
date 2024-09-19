use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use serde::Serialize;
use crate::parser::expressions::{Expression, ExpressionSpanExt};
use crate::parser::Parser;
use crate::{Span, TokenKind};
use crate::parser::{Result};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct CallExpression {
    pub span: Span,
    pub function: Rc<Expression>,
    pub arguments: Rc<[Expression]>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{}({})", self.function, args)
    }
}
impl<'a> Parser<'a> {
    pub(super) fn parse_call_expression(&mut self, left: Expression) -> Result<CallExpression> {
        let arguments = self.parse_expression_list(TokenKind::RightParen)?;

        Ok(CallExpression {
            span: self.span_with_start(left.span().clone()),
            function: Rc::new(left),
            arguments: arguments.into(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Lexer;
    use crate::parser::expressions::Expression;
    use crate::parser::Parser;
    use crate::parser::statements::Statement;

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Call(ref call) => {
                    match call.function.as_ref() {
                        Expression::Identifier(ref ident) => {
                            assert_eq!(ident.value, "add".into());
                        }
                        _ => panic!("expected identifier expression"),
                    };
                    assert_eq!(call.arguments.len(), 3);
                    assert_eq!(call.arguments[0].to_string(), "1");
                    assert_eq!(call.arguments[1].to_string(), "(2 * 3)");
                    assert_eq!(call.arguments[2].to_string(), "(4 + 5)");
                }
                _ => panic!("expected call expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}