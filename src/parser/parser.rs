use std::fmt::{self, Display, Formatter};

use crate::lexer::tokens::Token;
use crate::lexer::{lexer::Lexer, tokens::TokenKind};
use crate::parser::expressions::{Expression, Identifier};
use crate::trace;
use color_print::cformat;

use super::errors::{Error, ErrorKind};
use super::expressions::{
    BooleanLiteral, FloatLiteral, IfExpression, InfixExpression, IntegerLiteral, PrecedenceTrait,
    PrefixExpression,
};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<Error>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}\n", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s),
            Statement::Return(s) => write!(f, "{}", s),
            Statement::Expression(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut len = self.statements.len();

        for statement in &self.statements {
            write!(f, "\t{}", statement)?;
            if len > 1 {
                write!(f, "\n")?;
            }
            len -= 1;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {} = {};", self.token, self.name, self.value)
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {};", self.token, self.return_value)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", self.expression)
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
    lines: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lines: lexer.lines(),
            lexer,
            cur_token: Token {
                kind: TokenKind::EOF,
                line: 1,
                column: 0,
            },
            peek_token: Token {
                kind: TokenKind::EOF,
                line: 1,
                column: 0,
            },
            errors: vec![],
        };

        // Read two tokens, so cur_token and peek_token are both set
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        let token = self.lexer.next_token();
        let next = match token {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                line: self.cur_token.line,
                column: self.cur_token.column + 1,
            },
        };
        self.cur_token = std::mem::replace(&mut self.peek_token, next);
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
            errors: Vec::new(),
        };

        while self.cur_token.kind != TokenKind::EOF {
            let statement = match self.parse_statement() {
                Ok(statement) => statement,
                Err(err) => {
                    self.error(
                        ErrorKind::ParseError,
                        err.to_string(),
                        self.cur_token.line,
                        self.cur_token.column,
                        Some("parse_statement".to_string()),
                    );

                    self.next_token();
                    continue;
                }
            };
            program.statements.push(statement);
            self.next_token();
        }

        program.errors.append(&mut self.errors);

        // match self.errors.len() {
        //     0 => cprintln!("<green>Successfully parsed</green>"),
        //     _ => {
        //         cprintln!(
        //             "<red>Failed to parse with <b>{}</> errors</red>\n",
        //             self.errors.len()
        //         );
        //         for err in &self.errors {
        //             cprintln!("<red>{}</red>", err);
        //         }
        //     }
        // }

        program
    }

    fn parse_prefix(&mut self, token: TokenKind) -> Result<Expression, String> {
        let _trace = trace!("parse_prefix");
        match token {
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Number(_) => self.parse_number(),
            TokenKind::True | TokenKind::False => self.parse_boolean(),

            TokenKind::If => self.parse_if_expression(),

            TokenKind::Bang => self.parse_prefix_expression(),
            TokenKind::Minus => self.parse_prefix_expression(),

            TokenKind::LeftParen => self.parse_grouped_expression(),
            _ => Err(format!(
                "no prefix parse function for {} found",
                self.cur_token
            )),
        }
    }

    fn has_prefix(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Identifier(_) => true,
            TokenKind::Number(_) => true,

            TokenKind::LeftParen => true,

            TokenKind::True | TokenKind::False => true,

            TokenKind::Bang => true,
            TokenKind::Minus => true,

            TokenKind::If => true,
            _ => false,
        }
    }

    fn parse_infix(&mut self, token: TokenKind, left: Expression) -> Result<Expression, String> {
        let _trace = trace!("parse_infix");
        match token {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Slash
            | TokenKind::Asterisk
            | TokenKind::Percent
            | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess
            | TokenKind::CompareLessEqual => self.parse_infix_expression(left),
            _ => Err(format!(
                "no infix parse function for {} found",
                self.cur_token
            )),
        }
    }

    fn has_infix(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Slash
            | TokenKind::Asterisk
            | TokenKind::Percent
            | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess
            | TokenKind::CompareLessEqual => true,
            _ => false,
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        let _trace = trace!("parse_statement");

        let statement = match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Semicolon => {
                self.next_token();
                self.parse_statement()
            }
            _ => self.parse_expression_statement(),
        };

        self.expect_peek(TokenKind::Semicolon);

        statement
    }

    /// Checks if the peek token is the expected token and advances it to be the current token if it is.
    ///
    /// Returns true if the peek token was the expected token, false otherwise.
    ///
    /// Adds an error to the parser if the peek token was not the expected token.
    fn expect_peek(&mut self, token: TokenKind) -> bool {
        if self.peek_token.kind == token {
            self.next_token();
            true
        } else {
            self.error(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected next token to be <i>{:?}</i>, got <i>{:?}</i> instead",
                    token,
                    self.peek_token.kind
                ),
                self.peek_token.line,
                self.peek_token.column,
                None,
            );

            false
        }
    }

    /// Checks if the current token is the expected token.
    fn expect_cur(&mut self, token: TokenKind) -> bool {
        if self.cur_token.kind == token {
            true
        } else {
            self.error(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected current token to be <i>{:?}</i>, got <i>{:?}</i> instead",
                    token,
                    self.cur_token.kind
                ),
                self.cur_token.line,
                self.cur_token.column,
                None,
            );
            false
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let _trace = trace!("parse_let_statement");
        let token = self.cur_token.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => name.clone(),
            _ => return Err(format!("expected identifier, got {:?}", self.peek_token)),
        };

        let name = Identifier {
            token: self.peek_token.clone(),
            value: name,
        };

        self.next_token();

        if !self.expect_peek(TokenKind::AssignEqual) {
            return Err(format!("expected assign, got {:?}", self.peek_token));
        }

        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        let statement = Statement::Let(LetStatement {
            token,
            name,
            value: expression,
        });
        Ok(statement)
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let _trace = trace!("parse_return_statement");
        let token = self.cur_token.clone();
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        let statement = Statement::Return(ReturnStatement {
            token,
            return_value: expression,
        });

        Ok(statement)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let _trace = trace!("parse_expression_statement");
        let statement = Statement::Expression(ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest)?,
        });

        if self.is_cur_token(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let _trace = trace!("parse_expression");
        let token = self.cur_token.clone();

        if !self.has_prefix(&token.kind) {
            return Err(format!("No prefix parse function for {:?}", token));
        }

        let mut left_exp = self.parse_prefix(token.kind)?;

        while !self.is_peek_token(TokenKind::Semicolon) && precedence < self.peek_precedence() {
            let peek_token = self.peek_token.clone();

            if !self.has_infix(&peek_token.kind) {
                return Ok(left_exp);
            }

            self.next_token();

            left_exp = self.parse_infix(peek_token.kind, left_exp)?;
        }

        return Ok(left_exp);
    }

    fn is_cur_token(&self, token: TokenKind) -> bool {
        self.cur_token.kind == token
    }

    fn is_peek_token(&self, token: TokenKind) -> bool {
        self.peek_token.kind == token
    }

    fn is_cur_eof(&self) -> bool {
        self.cur_token.kind == TokenKind::EOF
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        let _trace = trace!("parse_identifier");
        let token = self.cur_token.clone();

        let literal = match token.kind {
            TokenKind::Identifier(ref literal) => literal.clone(),
            _ => return Err(format!("expected identifier, got {:?}", token)),
        };

        Ok(Expression::Identifier(Identifier {
            token: token,
            value: literal,
        }))
    }

    fn parse_number(&mut self) -> Result<Expression, String> {
        let _trace = trace!("parse_number");
        let token = self.cur_token.clone();

        let literal = match token.kind {
            TokenKind::Number(ref literal) => literal.clone(),
            _ => return Err(format!("expected number, got {:?}", token)),
        };

        if let Ok(value) = literal.parse::<i64>() {
            Ok(Expression::IntegerLiteral(IntegerLiteral { token, value }))
        } else if let Ok(value) = literal.parse::<f64>() {
            Ok(Expression::FloatLiteral(FloatLiteral { token, value }))
        } else {
            Err(format!("invalid number literal: {}", literal))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        let _trace = trace!("parse_prefix_expression");
        let token = self.cur_token.clone();

        let operator = match token.kind {
            TokenKind::Bang => String::from("!"),
            TokenKind::Minus => String::from("-"),
            _ => return Err(format!("expected prefix operator, got {:?}", token)),
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let _trace = trace!("parse_infix_expression");
        let token = self.cur_token.clone();

        let operator = match token.kind {
            TokenKind::Plus => String::from("+"),
            TokenKind::Minus => String::from("-"),
            TokenKind::Asterisk => String::from("*"),
            TokenKind::Slash => String::from("/"),
            TokenKind::Percent => String::from("%"),
            TokenKind::CompareEqual => String::from("=="),
            TokenKind::CompareNotEqual => String::from("!="),
            TokenKind::CompareLess => String::from("<"),
            TokenKind::CompareGreater => String::from(">"),
            TokenKind::CompareLessEqual => String::from("<="),
            TokenKind::CompareGreaterEqual => String::from(">="),
            _ => return Err(format!("expected infix operator, got {:?}", token)),
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }

    fn peek_precedence(&mut self) -> Precedence {
        match self.peek_token.precedence() {
            Some(precedence) => precedence,
            None => {
                self.error(
                    ErrorKind::MissingPrecedence,
                    format!("No precedence found for {:?}", self.peek_token),
                    self.peek_token.line,
                    self.peek_token.column,
                    None,
                );

                Precedence::Lowest
            }
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match self.cur_token.precedence() {
            Some(precedence) => precedence,
            None => Precedence::Lowest,
        }
    }

    fn parse_boolean(&self) -> Result<Expression, String> {
        let _trace = trace!("parse_boolean");
        let token = self.cur_token.clone();
        let value = match token.kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => return Err(format!("expected boolean, got {:?}", token)),
        };

        Ok(Expression::Boolean(BooleanLiteral { token, value }))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        let _trace = trace!("parse_grouped_expression");
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenKind::RightParen) {
            return Err(format!("expected ) got {:?}", self.peek_token));
        }

        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        let token = self.cur_token.clone();
        if !self.expect_peek(TokenKind::LeftParen) {
            return Err(format!("expected ( got {:?}", self.peek_token));
        }

        self.next_token();
        let condition = self
            .parse_expression(Precedence::Lowest)
            .map_err(|err| format!("parse_if_expression: {}", err))?;

        if !self.expect_peek(TokenKind::RightParen) {
            return Err(format!("expected ) got {:?}", self.peek_token));
        }

        if !self.expect_peek(TokenKind::LeftSquirly) {
            return Err(format!("expected {{ got {:?}", self.peek_token));
        }

        // parse_block_statement handles opening and closing braces
        let consequence = self
            .parse_block_statement()
            .map_err(|err| format!("parse_if_expression: {}", err))?;

        let mut alternative = None;

        if self.peek_token.kind == TokenKind::Else {
            self.next_token();

            if !self.expect_peek(TokenKind::LeftSquirly) {
                return Err(format!("expected {{ got {:?}", self.peek_token));
            }

            alternative = Some(
                self.parse_block_statement()
                    .map_err(|err| format!("parse_if_expression: {}", err))?,
            );
        }

        Ok(Expression::If(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    /// Parses a block statement
    fn parse_block_statement(&mut self) -> Result<BlockStatement, String> {
        // parse_block_statement handles opening and closing braces
        self.next_token();
        let token = self.cur_token.clone();

        let mut statements = Vec::new();

        while self.cur_token.kind != TokenKind::RightSquirly
            && self.cur_token.kind != TokenKind::EOF
        {
            let stmt = self
                .parse_statement()
                .map_err(|err| format!("parse_block_statement: {}", err))?;
            statements.push(stmt);
            self.next_token();
        }

        Ok(BlockStatement { token, statements })
    }

    fn error(
        &mut self,
        kind: ErrorKind,
        msg: String,
        line: usize,
        col: usize,
        context: Option<String>,
    ) {
        self.errors.push(Error {
            kind,
            message: msg,
            line_nr: line,
            column: col,
            context,
            line: self
                .lines
                // line is 1-indexed
                .get(line - 1)
                .unwrap_or(&"Failed to get line".to_string())
                .clone(),
        });
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_presedence() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::Sum);
        assert!(Precedence::Sum < Precedence::Product);
        assert!(Precedence::Product < Precedence::Prefix);
        assert!(Precedence::Prefix < Precedence::Call);
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.errors.len(), 0);

        let stmt = &program.statements[0];

        match stmt {
            Statement::Expression(ref expr) => {
                assert_eq!(expr.token.kind, TokenKind::Identifier("foobar".to_string()));
                match expr.expression {
                    Expression::Identifier(ref ident) => {
                        assert_eq!(ident.value, "foobar".to_string());
                        assert_eq!(
                            ident.token.kind,
                            TokenKind::Identifier("foobar".to_string())
                        );
                    }
                    _ => panic!("expected identifier expression"),
                };
            }
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b);\n"),
            ("!-a", "(!(-a));\n"),
            ("a + b + c", "((a + b) + c);\n"),
            ("a + b - c", "((a + b) - c);\n"),
            ("a * b * c", "((a * b) * c);\n"),
            ("a * b / c", "((a * b) / c);\n"),
            ("a + b / c", "(a + (b / c));\n"),
            (
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f);\n",
            ),
            ("3 + 4; -5 * 5", "(3 + 4);\n((-5) * 5);\n"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));\n"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));\n"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n",
            ),
            ("true", "true;\n"),
            ("false", "false;\n"),
            ("3 > 5 == false", "((3 > 5) == false);\n"),
            ("3 < 5 == true", "((3 < 5) == true);\n"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);\n"),
            ("(5 + 5) * 2", "((5 + 5) * 2);\n"),
            ("2 / (5 + 5)", "(2 / (5 + 5));\n"),
            ("-(5 + 5)", "(-(5 + 5));\n"),
            ("!(true == true)", "(!(true == true));\n"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d);\n"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();

            let actual = program.to_string();
            println!("{:?}", parser.errors);
            println!("Found {}, expected {}", actual, expected);
            assert_eq!(actual, expected);
            assert_eq!(parser.errors.len(), 0);
        }
    }
}
