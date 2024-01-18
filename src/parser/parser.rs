use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use crate::errors::{Error, ErrorKind, InterpreterErrors};
use crate::lexer::tokens::{Token, Position};
use crate::lexer::{lexer::Lexer, tokens::TokenKind};
use crate::parser::expressions::{Expression, Identifier};
use crate::trace;
use crate::tracer::reset_trace;
use color_print::cformat;

use super::expressions::{
    BooleanLiteral, FloatLiteral, FunctionLiteral, IfExpression, InfixExpression, IntegerLiteral,
    PrecedenceTrait, PrefixExpression, CallExpression, InfixOperator, PrefixOperator, StringLiteral, ArrayLiteral, IndexExpression, Precedence, RangeToExpression, RangeExpression, RangeFromExpression, RangeFullExpression, ForExpression, WhileExpression, MemberExpression, StructLiteral,
};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}\n", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Function(FunctionStatement),
}

#[derive(Debug, PartialEq)]
pub struct FunctionStatement {
    pub token: Token,
    pub name: Identifier,
    pub parameters: Rc<[Identifier]>,
    pub body: BlockStatement,
}

impl Display for FunctionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let params = self.parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", ");
        write!(
            f,
            "{} {}({}) {}",
            self.token, self.name, params, self.body
        )
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s),
            Statement::Return(s) => write!(f, "{}", s),
            Statement::Expression(s) => write!(f, "{}", s),
            Statement::Break(s) => write!(f, "{}", s),
            Statement::Continue(s) => write!(f, "{}", s),
            Statement::Function(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Rc<[Statement]>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for statement in self.statements.iter() {
            write!(f, "\t{}\n", statement)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.return_value {
            Some(value) => write!(f, "{} {};", self.token, value),
            None => write!(f, "{};", self.token),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", self.expression)
    }
}

#[derive(Debug, PartialEq)]
pub struct BreakStatement {
    pub token: Token,
    pub value: Option<Expression>,
}

impl Display for BreakStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{} {};", self.token, value),
            None => write!(f, "{};", self.token),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ContinueStatement {
    pub token: Token,
}

impl Display for ContinueStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", self.token)
    }
}

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

    fn next_token(&mut self) {
        let token = self.lexer.next_token();
        let next = match token {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                position: Position {
                    line: self.peek_token.position.line,
                    column: self.peek_token.position.column + self.peek_token.position.length + 1,
                    length: 1,
                    file: self.peek_token.position.file.clone(),
                }
            },
        };
        self.cur_token = std::mem::replace(&mut self.peek_token, next);
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

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_statement");

        let statement = match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement()?,
            TokenKind::Return => self.parse_return_statement()?,
            TokenKind::Semicolon => {
                self.next_token();
                self.parse_statement()?
            }
            TokenKind::Break => self.parse_break_statement()?,
            TokenKind::Continue => Statement::Continue(ContinueStatement { token: self.cur_token.clone() }),
            TokenKind::Fn => self.parse_function_statement()?,
            _ => self.parse_expression_statement()?,
        };

        // next statement must be seperated by a semicolon or on a new line
        if self.peek_token.kind == TokenKind::Semicolon {
            self.next_token();
        } else {
            // determine if the stament was ended properly
            // ie. the next token is on a new line or EOF
            let mut ended_properly = false;
            if self.cur_token.position.line != self.peek_token.position.line {
                ended_properly = true;
            } else if self.peek_token.kind == TokenKind::EOF {
                ended_properly = true;
            }

            if !ended_properly {
                return Err(self.error_peek(
                    ErrorKind::UnexpectedToken,
                    cformat!(
                        "Expected end of statement, got <i>{:?}</i> instead",
                        self.peek_token.kind
                    ),
                ));
            }
        }

        Ok(statement)
    }

    /// Checks if the current token is `token`, returns an error if not
    fn expect_current(&self, token: TokenKind) -> Result<(), Error> {
        if self.cur_token.kind == token {
            Ok(())
        } else {
            Err(self.error(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected current token to be <i>{:?}</i>, got <i>{:?}</i> instead",
                    token,
                    self.cur_token.kind
                ),
                Some(&self.cur_token),
                None,
            ))
        }
    }

    /// Checks if the peek token is the expected token and advances it to be the current token if it is.
    ///
    /// If the peek token is not the expected token `expect_peek` returns an Err
    fn expect_peek(&mut self, token: TokenKind) -> Result<(), Error> {
        if self.peek_token.kind == token {
            self.next_token();
            Ok(())
        } else {
            Err(self.error_peek(
                ErrorKind::UnexpectedToken,
                cformat!(
                    "Expected next token to be <i>{:?}</i>, got <i>{:?}</i> instead",
                    token,
                    self.peek_token.kind
                ),
            ))
        }
    }


    fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_let_statement");
        let token = self.cur_token.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => name.clone(),
            _ => {
                return Err(self.error_peek(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an identifier, got {:?}", self.peek_token.kind),
                ))
            }
        };

        let name = Identifier {
            token: self.peek_token.clone(),
            value: name.into(),
        };

        self.next_token();

        self.expect_peek(TokenKind::AssignEqual)?;

        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        let statement = Statement::Let(LetStatement {
            token,
            name,
            value: expression,
        });
        Ok(statement)
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_return_statement");
        let token = self.cur_token.clone();
        self.next_token();

        if let TokenKind::Semicolon = self.cur_token.kind {
            let statement = Statement::Return(ReturnStatement {
                token,
                return_value: None,
            });
            return Ok(statement);
        }

        let expression = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            // If there is no expression, let the user know that they need to return something or
            // use a semicolon
            Err(Error {
                kind: ErrorKind::MissingPrefix, ..
            }) => {
                return Err(self.error(
                    ErrorKind::UnexpectedToken,
                    cformat!(
                        "Expected an expression or semicolon after <i>`return`</i>, got <i>{:?}</i> instead",
                        self.cur_token.kind
                    ),
                    Some(&self.cur_token),
                    None,
                ));
            }
            Err(err) => return Err(err),
        };

        let statement = Statement::Return(ReturnStatement {
            token,
            return_value: Some(expression),
        });

        Ok(statement)
    }

    fn parse_function_statement(&mut self) -> Result<Statement, Error> {
        let _trace = trace!("parse_function_statement");

        let token = self.cur_token.clone();

        let name = match self.peek_token.kind {
            TokenKind::Identifier(ref name) => Identifier {
                token: self.cur_token.clone(),
                value: name.clone().into(),
            },
            TokenKind::LeftParen => {
                // Looks like this statement is actually an anonymous function
                // So we'll just parse it as such
                return self.parse_expression_statement();
            }
            _ => {
                return Err(self.error_peek(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an identifier, got {:?}", self.peek_token.kind),
                ))
            }
        };

        self.next_token();

        self.expect_peek(TokenKind::LeftParen)?;

        let parameters = self.parse_function_parameters()?.into();

        self.expect_peek(TokenKind::LeftSquirly)?;

        let body = self.parse_block_statement()?;

        let statement = Statement::Function(FunctionStatement {
            name,
            token,
            parameters,
            body,
        });

        Ok(statement)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
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

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let _trace = trace!("parse_expression");
        let token = self.cur_token.clone();

        if !self.has_prefix(&token.kind) {
            return Err(self.error_current_with_context(
                ErrorKind::MissingPrefix,
                format!("Expected a value, got {:?}", token.kind),
                "parse_expression".to_string(),
            ));
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

    fn has_prefix(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Identifier(_) => true,
            TokenKind::Number(_) => true,

            TokenKind::Fn => true,

            TokenKind::LeftParen => true,

            TokenKind::True | TokenKind::False => true,

            TokenKind::Bang => true,
            TokenKind::Minus => true,
            TokenKind::BitwiseNot => true,
            TokenKind::Ampersand => true,

            // Control flow expressions
            TokenKind::If => true,
            TokenKind::For => true,
            TokenKind::While => true,

            TokenKind::String(_) => true,
            TokenKind::Struct => true,

            // Block expressions
            TokenKind::LeftSquirly => true,

            // Array expressions
            TokenKind::LeftSquare => true,

            // Range expressions
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => true,
            _ => false,
        }
    }

    fn parse_prefix(&mut self, token: TokenKind) -> Result<Expression, Error> {
        let _trace = trace!("parse_prefix");
        match token {
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Number(_) => self.parse_number(),
            TokenKind::True | TokenKind::False => self.parse_boolean(),

            TokenKind::Fn => self.parse_function_literal(),

            TokenKind::If => self.parse_if_expression(),

            TokenKind::Bang => self.parse_prefix_expression(),
            TokenKind::Minus => self.parse_prefix_expression(),
            TokenKind::BitwiseNot => self.parse_prefix_expression(),
            TokenKind::Ampersand => self.parse_prefix_expression(),

            // Block expression
            TokenKind::LeftSquirly => self.parse_block_expression(),

            TokenKind::Struct => self.parse_struct_literal(),
            TokenKind::String(_) => self.parse_string(),

            TokenKind::LeftParen => self.parse_grouped_expression(),

            TokenKind::LeftSquare => self.parse_array_expression(),

            TokenKind::For => self.parse_for_expression(),
            TokenKind::While => self.parse_while_expression(),

            TokenKind::RangeExclusive | TokenKind::RangeInclusive => self.parse_range_prefix_expression(),
            _ => Err(self.error_current(
                ErrorKind::MissingPrefix,
                format!("No registered prefix function for {:?}", token),
            )),
        }
    }

    fn has_infix(&self, token: &TokenKind) -> bool {
        match token {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Divide
            | TokenKind::Multiply
            | TokenKind::Modulo
            | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess
            | TokenKind::CompareLessEqual 
            // Open parenthesis for function calls
            | TokenKind::LeftParen
            // Open square bracket for array indexing
            | TokenKind::LeftSquare

            // logical operators
            | TokenKind::LogicalAnd
            | TokenKind::LogicalOr

                // bitwise operators
            | TokenKind::Ampersand
            | TokenKind::BitwiseOr
            | TokenKind::BitwiseXor
            | TokenKind::BitwiseLeftShift
            | TokenKind::BitwiseRightShift

            // Assignment operators
            | TokenKind::AssignEqual
            | TokenKind::AssignPlus
            | TokenKind::AssignMinus
            | TokenKind::AssignMultiply
            | TokenKind::AssignDivide
            | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseAnd
            | TokenKind::AssignBitwiseOr
            | TokenKind::AssignBitwiseXor

            // Range operator
            | TokenKind::RangeExclusive
            | TokenKind::RangeInclusive

            // Method expressions
            | TokenKind::Dot 

                => true,
            _ => false,
        }
    }

    fn parse_infix(&mut self, token: TokenKind, left: Expression) -> Result<Expression, Error> {
        let _trace = trace!("parse_infix");
        match token {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Divide
            | TokenKind::Multiply
            | TokenKind::Modulo
            | TokenKind::CompareEqual
            | TokenKind::CompareNotEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual
            | TokenKind::CompareLess
            | TokenKind::CompareLessEqual 
            | TokenKind::LogicalAnd
            | TokenKind::LogicalOr
            | TokenKind::Ampersand
            | TokenKind::BitwiseOr
            | TokenKind::BitwiseXor
            | TokenKind::BitwiseLeftShift
            | TokenKind::BitwiseRightShift

            // Assignments
            | TokenKind::AssignEqual
            | TokenKind::AssignPlus
            | TokenKind::AssignMinus
            | TokenKind::AssignMultiply
            | TokenKind::AssignDivide
            | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseAnd
            | TokenKind::AssignBitwiseOr
            | TokenKind::AssignBitwiseXor

                => self.parse_infix_expression(left),
            TokenKind::LeftParen => self.parse_call_expression(left),
            TokenKind::LeftSquare => self.parse_index_expression(left),
            TokenKind::Dot => self.parse_member(left),
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => self.parse_range_infix_expression(token, left),
            _ => Err(self.error_current(
                ErrorKind::MissingInfix,
                format!("No registered infix function for {:?}", token),
            )),
        }
    }

    fn is_cur_token(&self, token: TokenKind) -> bool {
        self.cur_token.kind == token
    }

    fn is_peek_token(&self, token: TokenKind) -> bool {
        self.peek_token.kind == token
    }


    fn parse_identifier(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_identifier");
        let token = self.cur_token.clone();

        let literal = match token.kind {
            TokenKind::Identifier(ref literal) => literal.clone(),
            _ => {
                return Err(self.error_current(
                    ErrorKind::InvalidIdentifier,
                    "Expected an identifier".to_string(),
                ))
            }
        };

        Ok(Expression::Identifier(Identifier {
            token,
            value: literal.into(),
        }))
    }

    fn parse_number(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_number");
        let token = self.cur_token.clone();

        let literal = match token.kind {
            TokenKind::Number(ref literal) => literal.clone(),
            _ => {
                return Err(
                    self.error_current(ErrorKind::UnexpectedToken, "Expected a number".to_string())
                )
            }
        };

        if let Ok(value) = literal.parse::<i64>() {
            Ok(Expression::IntegerLiteral(IntegerLiteral { token, value }))
        } else if let Ok(value) = literal.parse::<f64>() {
            Ok(Expression::FloatLiteral(FloatLiteral { token, value }))
        } else {
            Err(self.error_current(
                ErrorKind::InvalidNumber,
                "Invalid number literal".to_string(),
            ))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_prefix_expression");
        let token = self.cur_token.clone();

        let operator = match token.kind {
            TokenKind::Bang => PrefixOperator::Bang,
            TokenKind::Minus => PrefixOperator::Minus,
            TokenKind::BitwiseNot => PrefixOperator::BitwiseNot,
            TokenKind::Ampersand => PrefixOperator::Ampersand,
            _ => {
                return Err(self.error_current(
                    ErrorKind::MissingPrefix,
                    format!("Expected a prefix operator, got {:?}", token),
                ))
            }
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right: Rc::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, Error> {
        let _trace = trace!("parse_infix_expression");
        let token = self.cur_token.clone();

        let operator = match token.kind {
            TokenKind::Plus => InfixOperator::Plus,
            TokenKind::Minus => InfixOperator::Minus,
            TokenKind::Multiply => InfixOperator::Asterisk,
            TokenKind::Divide => InfixOperator::Slash,
            TokenKind::Modulo => InfixOperator::Percent,
            TokenKind::CompareEqual => InfixOperator::CompareEqual,
            TokenKind::CompareNotEqual => InfixOperator::CompareNotEqual,
            TokenKind::CompareLess => InfixOperator::CompareLess,
            TokenKind::CompareGreater => InfixOperator::CompareGreater,
            TokenKind::CompareLessEqual => InfixOperator::CompareLessEqual,
            TokenKind::CompareGreaterEqual => InfixOperator::CompareGreaterEqual,

            TokenKind::BitwiseXor => InfixOperator::BitwiseXor,
            TokenKind::Ampersand => InfixOperator::BitwiseAnd,
            TokenKind::BitwiseOr => InfixOperator::BitwiseOr,
            TokenKind::BitwiseLeftShift => InfixOperator::BitwiseLeftShift,
            TokenKind::BitwiseRightShift => InfixOperator::BitwiseRightShift,

            TokenKind::LogicalAnd => InfixOperator::LogicalAnd,
            TokenKind::LogicalOr => InfixOperator::LogicalOr,

            // Assignment
            TokenKind::AssignEqual => InfixOperator::AssignEqual,
            TokenKind::AssignPlus => InfixOperator::AssignPlus,
            TokenKind::AssignMinus => InfixOperator::AssignMinus,
            TokenKind::AssignMultiply => InfixOperator::AssignAsterisk,
            TokenKind::AssignDivide => InfixOperator::AssignSlash,
            TokenKind::AssignModulo => InfixOperator::AssignPercent,
            TokenKind::AssignBitwiseXor => InfixOperator::AssignBitwiseXor,
            TokenKind::AssignBitwiseAnd => InfixOperator::AssignBitwiseAnd,
            TokenKind::AssignBitwiseOr => InfixOperator::AssignBitwiseOr,

            _ => {
                return Err(self.error_current(
                    ErrorKind::MissingInfix,
                    format!("expected infix operator, got {:?}", token),
                ))
            }
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left: Rc::new(left),
            right: Rc::new(right),
        }))
    }

    fn peek_precedence(&mut self) -> Precedence {
        
        match self.peek_token.precedence() {
            Some(precedence) => precedence,
            None => {
                self.errors.push(self.error_peek(
                    ErrorKind::MissingPrecedence,
                    format!("No precedence found for {:?}", self.peek_token),
                ));

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

    fn parse_boolean(&self) -> Result<Expression, Error> {
        let _trace = trace!("parse_boolean");
        let token = self.cur_token.clone();
        let value = match token.kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected boolean literal, got {:?}", token.kind),
                ))
            }
        };

        Ok(Expression::Boolean(BooleanLiteral { token, value }))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_grouped_expression");
        self.next_token();

        if self.cur_token.kind == TokenKind::RightParen {
            return Ok(Expression::Unit(
                self.cur_token.clone()
            ));
        }

        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RightParen)?;

        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();


        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::LeftSquirly)?;

        // parse_block_statement handles opening and closing braces
        let consequence = self.parse_block_statement()?;

        let mut alternative = None;

        if self.peek_token.kind == TokenKind::Else {
            self.next_token();

            self.expect_peek(TokenKind::LeftSquirly)?;

            alternative = Some(self.parse_block_statement()?);
        }

        Ok(Expression::If(IfExpression {
            token,
            condition: Rc::new(condition),
            consequence,
            alternative,
        }))
    }

    /// Parses a block statement
    fn parse_block_statement(&mut self) -> Result<BlockStatement, Error> {
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

        Ok(BlockStatement { token, statements: statements.into() })
    }

    fn error(
        &self,
        kind: ErrorKind,
        msg: impl ToString,
        token: Option<&Token>,
        context: Option<String>,
    ) -> Error {
        let error = Error::new(kind, msg, token, &self.lines, context);
        trace!(format!("Error: {:?}", error).as_str());
        error
    }

    fn error_current(&self, kind: ErrorKind, msg: impl ToString) -> Error {
        self.error(kind, msg, Some(&self.cur_token), None)
    }

    fn error_current_with_context(&self, kind: ErrorKind, msg: impl ToString, context: String) -> Error {
        self.error(
            kind,
            msg,
            Some(&self.cur_token),
            Some(context),
        )
    }

    fn error_peek(&self, kind: ErrorKind, msg: impl ToString) -> Error {
        self.error(
            kind,
            msg,
            Some(&self.peek_token),
            None,
        )
    }

    #[allow(dead_code)]
    fn error_peek_with_context(&self, kind: ErrorKind, msg: impl ToString, context: String) -> Error {
        self.error(
            kind,
            msg,
            Some(&self.peek_token),
            Some(context),
        )
    }

    fn parse_function_literal(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.expect_peek(TokenKind::LeftParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenKind::LeftSquirly)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::Function(FunctionLiteral {
            token,
            parameters: parameters.into(),
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, Error> {
        let mut identifiers = Vec::new();

        if self.peek_token.kind == TokenKind::RightParen {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        let ident = match self.parse_identifier()? {
            Expression::Identifier(ident) => ident,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an identifier, got {:?}", self.cur_token),
                ))
            }
        };

        identifiers.push(ident);

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();
            self.next_token();

            let ident = match self.parse_identifier()? {
                Expression::Identifier(ident) => ident,
                _ => {
                    return Err(self.error_current(
                        ErrorKind::UnexpectedToken,
                        format!("Expected an identifier, got {:?}", self.cur_token),
                    ))
                }
            };

            identifiers.push(ident);
        }

        self.expect_peek(TokenKind::RightParen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, Error> {
        let arguments = self.parse_expression_list(TokenKind::RightParen)?;

        Ok(Expression::Call(CallExpression {
            token: self.cur_token.clone(),
            function: Rc::new(left),
            arguments: arguments.into(),
        }))
    }

    fn parse_expression_list(&mut self, end: TokenKind) -> Result<Vec<Expression>, Error> {
        let mut arguments = Vec::new();

        if self.peek_token.kind == end {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();

        arguments.push(match self.parse_expression(Precedence::Lowest) {
            Err(Error { kind: ErrorKind::MissingPrefix, .. }) => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an expression or a {:?}, got {:?} instead", end, self.cur_token.kind),
                ))
            }
            Err(e) => return Err(e),
            Ok(e) => e,
        });

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();

            // allow trailing commas
            if self.peek_token.kind == end {
                break;
            }

            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(arguments)
    }

    fn parse_block_expression(&mut self) -> Result<Expression, Error> {
        Ok(Expression::Block(self.parse_block_statement()?))
    }

    fn parse_string(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let value = match self.cur_token.kind {
            TokenKind::String(ref s) => s.clone(),
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a string, got {:?}", self.cur_token),
                ))
            }
        };

        Ok(Expression::StringLiteral(StringLiteral { token, value: value.into() }))
    }

    fn parse_array_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(TokenKind::RightSquare)?;

        Ok(Expression::ArrayLiteral(ArrayLiteral { token, elements: elements.into() }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RightSquare)?;

        Ok(Expression::Index(IndexExpression {
            token,
            left: Rc::new(left),
            index: Rc::new(index),
    }))
}

    /// Parses a RangeTo expression
    /// e.g. `..5` or `..=5`
    fn parse_range_prefix_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        // check if we are dealing with a FullRange expression
        // e.g. `..`
        // if so, we would have a `..` token followed by a ')' or a ']' token
        if self.peek_token.kind == TokenKind::RightParen || self.peek_token.kind == TokenKind::RightSquare {
            return Ok(Expression::RangeFull(RangeFullExpression { token }));
        }

        self.next_token();


        let right = self.parse_expression(Precedence::Prefix)?;

        let inclusive = match token.kind {
            TokenKind::RangeInclusive => true,
            TokenKind::RangeExclusive => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a range operator, got {:?}", self.cur_token),
                ))
            }
        };

        Ok(Expression::RangeTo(RangeToExpression {
            token,
            right: Rc::new(right),
            inclusive,
        }))
    }

    fn parse_range_infix_expression(&mut self, token_kind: TokenKind, left: Expression) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        let precedence = self.cur_precedence();
        let inclusive = match token_kind {
            TokenKind::RangeInclusive => true,
            TokenKind::RangeExclusive => false,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected a range operator, got {:?}", self.cur_token),
                ))
            }
        };

        // check if we have a RangeFrom expression
        // RangeFrom expressions are bounded by either square brackets or a parenthesis
        // 
        // ```text
        // (1..) // RangeFrom
        // [1..] // RangeFrom
        if self.peek_token.kind == TokenKind::RightParen || self.peek_token.kind == TokenKind::RightSquare {
            return Ok(Expression::RangeFrom(RangeFromExpression {
                token,
                left: Rc::new(left),
                inclusive,
            }));
        }

        self.next_token();


        // if the next token is not a closing parenthesis or square bracket, we assume that we have a Range expression

        let right = self.parse_expression(precedence)?;

        Ok(Expression::Range(RangeExpression {
            token,
            left: Rc::new(left),
            right: Rc::new(right),
            inclusive,
        }))
    }

    /// Parses a for expression
    ///
    /// valid syntax:
    /// ```text
    /// for <iterator> in <iterable> { <body> }
    /// for (<index>, <iterator>) in <iterable> { <body> }
    /// ```
    fn parse_for_expression(&mut self) -> Result<Expression, Error> {
        let _trace = trace!("parse_for_expression");
        let token = self.cur_token.clone();

        self.next_token();

        // check if we have a for expression with an index
        let index = match self.cur_token.kind {
            TokenKind::LeftParen => {
                self.next_token();
                let index = match self.parse_identifier()? {
                    Expression::Identifier(ident) => ident,
                    _ => {
                        return Err(self.error_current(
                            ErrorKind::UnexpectedToken,
                            format!("Expected an identifier, got {:?}", self.cur_token),
                        ))
                    }
                };
                self.expect_peek(TokenKind::Comma)?;
                self.next_token();
                Some(index)
            }
            _ => None,
        };

        let iterator = match self.parse_identifier()? {
            Expression::Identifier(ident) => ident,
            _ => {
                return Err(self.error_current(
                    ErrorKind::UnexpectedToken,
                    format!("Expected an identifier, got {:?}", self.cur_token),
                ))
            }
        };

        if index.is_some() {
            self.expect_peek(TokenKind::RightParen)?;
        }

        self.expect_peek(TokenKind::In)?;
        self.next_token();

        let iterable = self.parse_expression(Precedence::Lowest)?;

        // set current token to the '{' token
        self.next_token();

        let body = self.parse_block_statement()?;

        let else_block = if self.peek_token.kind == TokenKind::Else {
            self.next_token();
            self.expect_peek(TokenKind::LeftSquirly)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::For(ForExpression {
            token,
            iterator,
            iterable: Rc::new(iterable),
            body,
            index,
            else_block,
        }))

    }

    fn parse_break_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();

        self.next_token();

        // check if we have a value to return
        let value = if self.cur_token.kind != TokenKind::Semicolon {
            let value = self.parse_expression(Precedence::Lowest)?;
            self.next_token();
            Some(value)
        } else {
            None
        };

        Ok(Statement::Break(BreakStatement { token, value }))
    }

    /// Parses a while expression
    ///
    /// valid syntax:
    ///
    /// ```text
    /// while <condition> { <body> }
    fn parse_while_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::LeftSquirly)?;

        let body = self.parse_block_statement()?;

        let else_block = if self.peek_token.kind == TokenKind::Else {
            self.next_token();
            self.expect_peek(TokenKind::LeftSquirly)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::While(WhileExpression {
            token,
            condition: Rc::new(condition),
            body,
            else_block,
        }))
    }

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
    fn parse_struct_literal(&mut self) -> Result<Expression, Error> {
        // Cur token is `struct`
        let token = self.cur_token.clone();

        self.expect_peek(TokenKind::LeftSquirly)?;

        self.next_token();

        let mut entries = HashMap::new();

        while self.cur_token.kind != TokenKind::RightSquirly {
            let key = match self.cur_token.kind {
                TokenKind::Identifier(ref ident) => ident.clone(),
                _ => return Err(self.error_current(ErrorKind::TypeError, "Struct keys must be identifiers.")),
            };

            self.expect_peek(TokenKind::Colon)?;

            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;
            entries.insert(key, value);

            self.next_token();

            if self.cur_token.kind != TokenKind::Comma && self.cur_token.kind != TokenKind::RightSquirly {
                return Err(self.error_current(ErrorKind::UnexpectedToken, format!("Expected a `,` or `}}`")))
            }

            if self.cur_token.kind == TokenKind::Comma {
                self.next_token();
            }
        }

        self.expect_current(TokenKind::RightSquirly)?;

        Ok(Expression::StructLiteral(StructLiteral {
            token,
            entries
        }))
    }

    fn parse_member(&mut self, left: Expression) -> Result<Expression, Error> {

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

use core::panic;

use color_print::cprintln;

use crate::parser::expressions::Precedence;

use super::*;

    fn parse(input: &str) -> Result<Vec<Statement>, InterpreterErrors> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let result = parser.parse();

        if let Err(ref err) = result {
            cprintln!("{}", err)
        }

        result.map(|prgrm| prgrm.statements)
    }

#[test]
fn test_presedence() {
    assert!(Precedence::Lowest < Precedence::Assign);
    assert!(Precedence::Assign < Precedence::LogicalOr);
    assert!(Precedence::LogicalOr < Precedence::LogicalAnd);
    assert!(Precedence::LogicalAnd < Precedence::Equals);
    assert!(Precedence::Equals < Precedence::LessGreater);
    assert!(Precedence::LessGreater < Precedence::BitwiseOr);
    assert!(Precedence::BitwiseOr < Precedence::BitwiseXor);
    assert!(Precedence::BitwiseXor < Precedence::BitwiseAnd);
    assert!(Precedence::BitShift < Precedence::Sum);
    assert!(Precedence::Sum < Precedence::Product);
    assert!(Precedence::Product < Precedence::Prefix);
    assert!(Precedence::Prefix < Precedence::Index);
    assert!(Precedence::Index < Precedence::Call);
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let statements = parse(input).unwrap();


    let stmt = &statements[0];

    match stmt {
        Statement::Expression(ref expr) => {
            assert_eq!(expr.token.kind, TokenKind::Identifier("foobar".to_string()));
            match expr.expression {
                Expression::Identifier(ref ident) => {
                    assert_eq!(ident.value, "foobar".into());
                    assert_eq!(
                        ident.token.kind,
                        TokenKind::Identifier("foobar".into())
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
        ("-a * b;", "((-a) * b);"),
        ("!-a;", "(!(-a));"),
        ("a + b + c;", "((a + b) + c);"),
        ("a + b - c;", "((a + b) - c);"),
        ("a * b * c;", "((a * b) * c);"),
        ("a * b / c;", "((a * b) / c);"),
        ("a + b / c;", "(a + (b / c));"),
        (
            "a + b * c + d / e - f;",
            "(((a + (b * c)) + (d / e)) - f);",
        ),
        ("3 + 4; -5 * 5;", "(3 + 4);\n((-5) * 5);"),
        ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
        ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5;",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
        ),
        ("true;", "true;"),
            ("false;", "false;"),
            ("3 > 5 == false;", "((3 > 5) == false);"),
            ("3 < 5 == true;", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("-(5 + 5);", "(-(5 + 5));"),
            ("!(true == true);", "(!(true == true));"),
            ("a + add(b * c) + d;", "((a + add((b * c))) + d);"),

            ("1 && 0 == 0;", "(1 && (0 == 0));"),
            ("1 << 1 == 0;", "((1 << 1) == 0);"),

            
            ("a + b * c;", "(a + (b * c));"),
            ("a == b && c != d;", "((a == b) && (c != d));"),
            ("a | b ^ c & d;", "(a | (b ^ (c & d)));"),
            ("a << b >> c;", "((a << b) >> c);"),
            ("a <= b && c < d || e >= f && g > h;", "(((a <= b) && (c < d)) || ((e >= f) && (g > h)));"),

            // Index expressions
            ("myArray[1 + 1];", "(myArray[(1 + 1)]);"),
            ("myArray[1][2];", "((myArray[1])[2]);"),
            ("a * [1, 2, 3, 4][b * c] * d;", "((a * ([1, 2, 3, 4][(b * c)])) * d);"),
            ("add(a * b[2], b[1], 2 * [1, 2][1]);", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));"),


        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let actual = program.to_string();
            assert_eq!(actual.trim(), expected.trim());
            assert_eq!(parser.errors.len(), 0);
        }
    }

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
                            assert_eq!(
                                ident.token.kind,
                                TokenKind::Identifier("add".into())
                            );
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

    #[test]
    fn test_if_expression() {
        let input = "if x < y { x; }";


        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::If(ref if_expr) => {
                    assert_eq!(if_expr.condition.to_string(), "(x < y)");
                    assert_eq!(if_expr.consequence.to_string().split_whitespace().collect::<Vec<&str>>().join(" "), "{ x; }");
                    assert_eq!(if_expr.alternative.is_none(), true);
                }
                _ => panic!("expected if expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::StringLiteral(ref literal) => {
                    assert_eq!(literal.value, "hello world".into());
                    assert_eq!(
                        literal.token.kind,
                        TokenKind::String("hello world".into())
                    );
                }
                _ => panic!("expected string literal expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_array_literal_expression() {
        let input = "[1, 2 * 2, 3 + 3]";

        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::ArrayLiteral(ref literal) => {
                    assert_eq!(literal.elements.len(), 3);
                    assert_eq!(literal.elements[0].to_string(), "1");
                    assert_eq!(literal.elements[1].to_string(), "(2 * 2)");
                    assert_eq!(literal.elements[2].to_string(), "(3 + 3)");
                }
                _ => panic!("expected array literal expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_index_expression() {
        let input = "myArray[1 + 1]";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        let stmt = &statements[0];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Index(ref index) => {
                    assert_eq!(index.left.to_string(), "myArray");
                    assert_eq!(index.index.to_string(), "(1 + 1)");
                }
                _ => panic!("expected index expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_mutating_values() {
        let input = "let a = 1; a = 2;";

        let statements = parse(input).unwrap();


        assert_eq!(statements.len(), 2);

        let stmt = &statements[1];

        match stmt {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignEqual);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "2");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_special_mutation_operators() {
        let input = "let a = 1; a += 2; a -= 3; a *= 4; a /= 5; a %= 6;";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 6);

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignPlus);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "2");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[2] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignMinus);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "3");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[3] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignAsterisk);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "4");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[4] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignSlash);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "5");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[5] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Infix(ref infix) => {
                    assert_eq!(infix.operator, InfixOperator::AssignPercent);
                    assert_eq!(infix.left.to_string(), "a");
                    assert_eq!(infix.right.to_string(), "6");
                }
                _ => panic!("expected assign expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_expression() {
        let input = "1..5; 1..=5;";

        let statements = parse(input).unwrap();
        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Range(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, false);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Range(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, true);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_from_expressions() {
        let input = "(1..); (1..=);";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeFrom(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.inclusive, false);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeFrom(ref range) => {
                    assert_eq!(range.left.to_string(), "1");
                    assert_eq!(range.inclusive, true);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_to_expressions() {
        let input = "..5; ..=5;";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeTo(ref range) => {
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, false);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeTo(ref range) => {
                    assert_eq!(range.right.to_string(), "5");
                    assert_eq!(range.inclusive, true);
                }
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_range_full_expression() {
        let input = "(..); array[..];";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 2);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::RangeFull(_) => {}
                _ => panic!("expected range expression"),
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::Index(ref index) => {
                    assert_eq!(index.left.to_string(), "array");
                    assert_eq!(index.index.to_string(), "(..)");
                }
                _ => panic!("expected index expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_for_loop() {
        let input = "for i in 1..5 { i; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::For(ref for_loop) => {
                    assert_eq!(for_loop.iterator.to_string(), "i");
                    assert_eq!(for_loop.iterable.to_string(), "(1..5)");
                    assert_eq!(for_loop.body.statements.len(), 1);
                }
                _ => panic!("expected for loop expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_for_loop_with_index() {
        let input = "for (i, j) in 1..5 { i; j; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::For(ref for_loop) => {
                    println!("{:#?}", for_loop);
                    assert_eq!(for_loop.iterator.to_string(), "j");
                    assert_eq!(for_loop.iterable.to_string(), "(1..5)");
                    assert_eq!(for_loop.body.statements.len(), 2);

                    match for_loop.index {
                        Some(ref index) => {
                            assert_eq!(index.to_string(), "i");
                        }
                        None => panic!("expected index"),
                    }
                }
                _ => panic!("expected for loop expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_break_continue() {
        let input = r#"
        break;
        continue;
        break 1;
        "#;

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 3);

        match &statements[0] {
            Statement::Break(ref break_stmt) => {
                assert_eq!(break_stmt.value, None);
            },
            _ => panic!("expected expression statement"),
        };

        match &statements[1] {
            Statement::Continue(_) => {},
            _ => panic!("expected expression statement"),
        };

        match &statements[2] {
            Statement::Break(ref break_stmt) => {
                assert_eq!(break_stmt.value.is_some(), true);
                assert_eq!(break_stmt.value.as_ref().unwrap().to_string(), "1");
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_while_expression() {
        let input = "while true { 1; }";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        match &statements[0] {
            Statement::Expression(ref expr) => match expr.expression {
                Expression::While(ref while_expr) => {
                    assert_eq!(while_expr.condition.to_string(), "true");
                    assert_eq!(while_expr.body.statements.len(), 1);
                }
                _ => panic!("expected while expression"),
            },
            _ => panic!("expected expression statement"),
        };
    }

    #[test]
    fn test_method_call() {
        let input = "foo.bar(1, 2, 3);";

        let statements = parse(input).unwrap();

        assert_eq!(statements.len(), 1);

        let call_expr = match statements[0] {
            Statement::Expression(
                ExpressionStatement { expression: 
                Expression::Call(ref call_expr), ..}) => call_expr,
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

    #[test]
    fn test_function_statement() {
        let input = "fn foo() { 1; }";


    }
    

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
            value: Expression::StructLiteral(StructLiteral { entries,.. }),
            ..
        }) = &statements[0] {
            assert_eq!(name.as_ref(), "object");

            match entries.get("a") {
                Some(Expression::IntegerLiteral(IntegerLiteral { value: 1, .. })) => {},
                _ => panic!("Key 'a' should contain integer 1")
            }
            match entries.get("b") {
                Some(Expression::StringLiteral(StringLiteral { value,.. })) if value.as_ref() == "Some String" => {},
                _ => panic!("Key 'b' should contain string `Some String`")
            }
        }
    }

}

