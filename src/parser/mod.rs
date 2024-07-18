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

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
    lines: Rc<[String]>,
}

impl Parser {
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
    use crate::Lexer;
    use crate::parser::Parser;
    use crate::parser::statements::Statement;

    pub(super) fn parse(input: &str) -> Result<Vec<Statement>, InterpreterErrors> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let result = parser.parse();

        if let Err(ref err) = result {
            cprintln!("{}", err)
        }

        result.map(|prgrm| prgrm.statements)
    }
}



