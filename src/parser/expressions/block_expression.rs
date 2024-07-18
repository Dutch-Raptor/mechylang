use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use serde::Serialize;
use crate::{Error, Token, trace};
use crate::lexer::tokens::TokenKind;
use crate::parser::Parser;
use crate::parser::statements::Statement;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockExpression {
    pub token: Token,
    pub statements: Rc<[Statement]>,
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{\n")?;
        for statement in self.statements.iter() {
            write!(f, "\t{}\n", statement)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl Parser {
    pub(in crate::parser) fn parse_block_expression(&mut self) -> Result<BlockExpression, Error> {

        let _trace = trace!("parse_block_statement");
        // parse_block_statement handles opening and closing braces
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


        Ok(BlockExpression { token, statements: statements.into() })
    }
}


#[cfg(test)]
mod tests {
    use crate::parser::expressions::Expression;
    use crate::parser::statements::Statement;
    use crate::parser::tests::parse;

    #[test]
    fn test_block_expression() {
        let input = "{
            let x = 1;
            let y = 2;
            x + y;
        }";

        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Block(ref block) => {
                    assert_eq!(block.statements.len(), 3);
                    assert_eq!(block.statements[0].to_string(), "let x = 1;");
                    assert_eq!(block.statements[1].to_string(), "let y = 2;");
                    assert_eq!(block.statements[2].to_string(), "(x + y);");
                }
                _ => panic!("expected block expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }
}
