use std::rc::Rc;

use crate::{
    lexer::{lexer::Lexer, tokens::Token},
    parser::{
        errors::{Error, ErrorKind},
        expressions::{
            CallExpression, Expression, Identifier, IfExpression, InfixExpression, InfixOperator,
            PrefixOperator,
        },
        parser::{BlockStatement, LetStatement, Parser, Statement},
    },
};

use super::{
    environment::Environment,
    objects::{Function, Object, UnwrapReturnValue},
};

pub type EvalResult = Result<Object, Rc<[Error]>>;

pub struct Evaluator {
    lines: Vec<String>,
    program: Rc<[Statement]>,
    current_token: Option<Token>,
}

impl Evaluator {
    pub fn eval(input: impl Into<Rc<str>>, env: &mut Environment) -> EvalResult {
        let input: Rc<str> = input.into();
        let lexer = Lexer::new(&input);
        let lines = lexer.lines();
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        if !program.errors.is_empty() {
            return Err(program.errors.into());
        }

        let evaluator = Evaluator {
            lines,
            program: program.statements.into(),
            current_token: None,
        };

        evaluator.eval_program(env).map_err(|err| vec![err].into())
    }

    pub fn eval_program(mut self, env: &mut Environment) -> Result<Object, Error> {
        let mut result = Object::Null;

        let program = self.program.clone();

        for statement in program.iter() {
            result = self.eval_statement(statement, env)?;

            if let Object::ReturnValue(val) = result {
                return Ok(*val);
            }
        }

        Ok(result)
    }

    fn eval_statement(
        &mut self,
        statement: &Statement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        match statement {
            Statement::Expression(stmt) => self.eval_expression(&stmt.expression, env),
            Statement::Return(stmt) => {
                let val = self.eval_expression(&stmt.return_value, env)?;
                Ok(Object::ReturnValue(Box::new(val)))
            }
            Statement::Let(let_statement) => {
                let val = self.eval_let_statement(let_statement, env)?;
                Ok(Object::Let(let_statement.name.clone(), Box::new(val)))
            }
        }
    }

    fn eval_let_statement(
        &mut self,
        let_statement: &LetStatement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let val = self.eval_expression(&let_statement.value, env)?;
        env.set(let_statement.name.value.clone(), val.clone());
        Ok(val)
    }

    fn eval_expression(
        &mut self,
        expression: &Expression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        dbg!(&env);

        match expression {
            Expression::IntegerLiteral(lit) => Ok(Object::Integer(lit.value)),
            Expression::Boolean(boolean) => Ok(boolean.value.into()),
            Expression::Prefix(prefix) => {
                let right = self.eval_expression(&prefix.right, env)?;
                self.current_token = Some(prefix.token.clone());

                match prefix.operator {
                    PrefixOperator::Bang => Ok(self.eval_bang_operator_expression(right)),
                    PrefixOperator::Minus => Ok(self.eval_minus_prefix_operator_expression(right)?),
                }
            }
            Expression::Infix(infix) => self.eval_infix_expression(infix, env),
            Expression::If(if_expr) => self.eval_if_expression(if_expr, env),
            Expression::Block(block) => self.eval_block_statement(block, env),
            Expression::Identifier(ident) => self.eval_identifier(ident, env),
            Expression::Function(func) => Ok(Object::Function(Function {
                params: func.parameters.clone(),
                body: func.body.clone(),
                env: env.clone(),
            })),
            Expression::Call(call) => self.eval_call_expression(call, env),
            _ => todo!(),
        }
    }

    fn eval_infix_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let left = self.eval_expression(&infix.left, env)?;
        let right = self.eval_expression(&infix.right, env)?;

        self.current_token = Some(infix.token.clone());

        if let Object::Integer(left) = left {
            if let Object::Integer(right) = right {
                return self.eval_integer_infix_expression(&infix.operator, left, right);
            }
        }

        if let Object::Boolean(left) = left {
            if let Object::Boolean(right) = right {
                return self.eval_boolean_infix_expression(&infix.operator, left, right);
            }
        }

        Err(self.error(
            self.current_token.as_ref(),
            format!("Type mismatch: {:?} {} {:?}", left, infix.operator, right).as_str(),
            ErrorKind::TypeMismatch,
        ))
    }

    fn eval_block_statement(
        &mut self,
        block: &BlockStatement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let mut result = Object::Null;

        for statement in block.statements.iter() {
            result = self.eval_statement(statement, env)?;

            // if the result is a return value, bubble it up and stop evaluating the block
            if let Object::ReturnValue(_) = result {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_if_expression(
        &mut self,
        if_expr: &IfExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let condition = self.eval_expression(&if_expr.condition, env)?;

        if Self::is_truthy(condition) {
            self.eval_block_statement(&if_expr.consequence, env)
        } else if let Some(alternative) = &if_expr.alternative {
            self.eval_block_statement(alternative, env)
        } else {
            Ok(Object::Null)
        }
    }

    fn is_truthy(condition: Object) -> bool {
        match condition {
            Object::Null => false,
            Object::Boolean(boolean) => boolean,
            _ => true,
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &InfixOperator,
        left: bool,
        right: bool,
    ) -> Result<Object, Error> {
        let invalid = || {
            self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: Boolean({:?}) {} Boolean({:?})",
                    left, operator, right
                )
                .as_str(),
                ErrorKind::InvalidOperator,
            )
        };

        match operator {
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),
            InfixOperator::LogicalAnd => Ok((left && right).into()),
            InfixOperator::LogicalOr => Ok((left || right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::Plus
            | InfixOperator::Minus
            | InfixOperator::Asterisk
            | InfixOperator::Slash
            | InfixOperator::Percent
            | InfixOperator::CompareGreater
            | InfixOperator::CompareLess
            | InfixOperator::CompareGreaterEqual
            | InfixOperator::CompareLessEqual
            | InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseLeftShift
            | InfixOperator::BitwiseRightShift => Err(invalid()),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &InfixOperator,
        left: i64,
        right: i64,
    ) -> Result<Object, Error> {
        let invalid = || {
            self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: Integer({:?}) {} Integer({:?})",
                    left, operator, right
                )
                .as_str(),
                ErrorKind::InvalidOperator,
            )
        };

        match operator {
            InfixOperator::Plus => Ok((left + right).into()),
            InfixOperator::Minus => Ok((left - right).into()),
            InfixOperator::Asterisk => Ok((left * right).into()),
            InfixOperator::Slash => Ok((left / right).into()),
            InfixOperator::Percent => Ok((left % right).into()),
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),
            InfixOperator::CompareGreater => Ok((left > right).into()),
            InfixOperator::CompareLess => Ok((left < right).into()),
            InfixOperator::CompareGreaterEqual => Ok((left >= right).into()),
            InfixOperator::CompareLessEqual => Ok((left <= right).into()),

            InfixOperator::BitwiseOr => Ok((left | right).into()),
            InfixOperator::BitwiseAnd => Ok((left & right).into()),
            InfixOperator::BitwiseXor => Ok((left ^ right).into()),
            InfixOperator::BitwiseLeftShift => Ok((left << right).into()),
            InfixOperator::BitwiseRightShift => Ok((left >> right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::LogicalAnd => Err(invalid()),
            InfixOperator::LogicalOr => Err(invalid()),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Result<Object, Error> {
        match right {
            Object::Integer(integer) => Ok((-integer).into()),
            _ => Err(self.error(
                self.current_token.as_ref(),
                format!("Unknown operator: -{:?}", right).as_str(),
                ErrorKind::UnknownOperator,
            )),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(boolean) => (!boolean).into(),
            Object::Null => true.into(),
            _ => false.into(),
        }
    }

    fn error(&self, token: Option<&Token>, message: &str, error_kind: ErrorKind) -> Error {
        let (line_nr, line, column) = if let Some(token) = token {
            (
                token.line,
                self.lines
                    // line is 1-indexed
                    .get(token.line - 1)
                    .unwrap_or(&"Failed to get line".to_string())
                    .clone(),
                token.column,
            )
        } else {
            (0, "".to_string(), 0)
        };
        Error {
            line,
            line_nr,
            column,
            kind: error_kind,
            message: message.to_string(),
            context: None,
        }
    }

    fn eval_identifier(
        &mut self,
        ident: &Identifier,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        if let Some(object) = env.get(ident.value.clone()) {
            Ok(object)
        } else {
            Err(self.error(
                Some(&ident.token),
                format!("Identifier not found: {}", ident.value).as_str(),
                ErrorKind::IdentifierNotFound,
            ))
        }
    }

    fn eval_call_expression(
        &mut self,
        call: &CallExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let function = self.eval_expression(&call.function, env)?;

        let arguments = self.eval_expressions(&call.arguments, env)?;

        self.apply_function(function, arguments)
    }

    fn apply_function(
        &mut self,
        function: Object,
        arguments: Vec<Object>,
    ) -> Result<Object, Error> {
        let function = match function {
            Object::Function(function) => function,
            _ => {
                return Err(self.error(
                    None,
                    format!("Not a function: {:?}", function).as_str(),
                    ErrorKind::TypeError,
                ))
            }
        };

        let mut extended_env = self.extend_function_env(&function, arguments)?;

        let evaluated = self.eval_block_statement(&function.body, &mut extended_env)?;

        return Ok(evaluated.unwrap_return_value());
    }

    fn extend_function_env(
        &mut self,
        function: &Function,
        arguments: Vec<Object>,
    ) -> Result<Environment, Error> {
        println!("Extending function env");
        println!("Function env: {:#?}", function.env);
        let mut env = Environment::new_enclosed(function.env.clone());

        println!("Extended env: {:#?}", env);
        println!("rc count {}", env.strong_count());
        println!("outer rc count {}", env.outer_strong_count());

        for (parameter, argument) in function.params.iter().zip(arguments) {
            env.set(parameter.value.clone(), argument);
        }

        Ok(env)
    }

    fn eval_expressions(
        &mut self,
        arguments: &[Expression],
        env: &mut Environment,
    ) -> Result<Vec<Object>, Error> {
        let mut result = Vec::with_capacity(arguments.len());

        for argument in arguments {
            let evaluated = self.eval_expression(argument, env)?;
            result.push(evaluated);
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        evaluator::{environment::Environment, objects::Object},
        parser::errors::ErrorKind,
    };

    use super::{EvalResult, Evaluator};

    fn test_eval(input: &str) -> Object {
        let mut env = Environment::new();

        let result = Evaluator::eval(input, &mut env);

        match result {
            Ok(object) => object,
            Err(errors) => {
                for error in errors.iter() {
                    println!("{}", error);
                }

                println!("env: {:#?}", env);
                panic!("Error occured while evaluating input: {}", input);
            }
        }
    }

    fn test_eval_fallible(input: &str) -> EvalResult {
        let mut env = Environment::new();

        Evaluator::eval(input, &mut env)
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
            ("4 % 2", 0),
            ("5 % 2", 1),
            // bitwise
            ("1 | 2", 3),
            ("1 & 2", 0),
            ("1 ^ 2", 3),
            ("1 << 2", 4),
            ("4 >> 2", 1),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_integer_object(obj: Object, expected: i64) {
        match obj {
            Object::Integer(integer) => assert_eq!(integer, expected),
            _ => panic!("Object is not an Integer. Got: {:?}", obj),
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        match obj {
            Object::Boolean(boolean) => assert_eq!(boolean, expected),
            _ => panic!("Object is not a Boolean. Got: {:?}", obj),
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_block_expressions() {
        let tests = vec![
            ("{ 1; 2; 3; }", Object::Integer(3)),
            ("4 == { 4 }", Object::Boolean(true)),
            ("4 == { 5 }", Object::Boolean(false)),
            ("{ 4 } == 4", Object::Boolean(true)),
            ("{ 5 } == 4", Object::Boolean(false)),
            ("{ 4 } == { 4 }", Object::Boolean(true)),
            ("{ 4 } == { 5 }", Object::Boolean(false)),
            ("{ 4; 5 } == { 4; 5 }", Object::Boolean(true)),
            ("{ 4; 5 } == { 5; 4 }", Object::Boolean(false)),
            ("{ 4; 5; 6 } == { 4; 5 }", Object::Boolean(false)),
            ("{ 4; 5 } == { 4; 5; 6 }", Object::Boolean(false)),
            ("{ 4; 5; 6 } == { 4; 6 }", Object::Boolean(true)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                Object::Integer(10),
            ),
            (
                "9; if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                Object::Integer(10),
            ),
            (
                "if (10 > 1) { if (10 < 1) { return 10; } return 1; } return 9;",
                Object::Integer(1),
            ),
            (
                "if (10 < 1) { if (10 < 1) { return 10; } return 1; } return 9;",
                Object::Integer(9),
            ),
            // (
            //     "let f = fn(x) { return x; x + 10; }; f(10);",
            //     Object::Integer(10),
            // ),
            // (
            //     "let f = fn(x) { let result = x + 10; return result; return 10; }; f(10);",
            //     Object::Integer(20),
            // ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true;",
                "Type mismatch: Integer(5) + Boolean(true)",
                ErrorKind::TypeMismatch,
                "5 + true;",
                1,
            ),
            (
                "5 + true;
                 5;",
                "Type mismatch: Integer(5) + Boolean(true)",
                ErrorKind::TypeMismatch,
                "5 + true;",
                1,
            ),
            (
                "-true",
                "Unknown operator: -Boolean(true)",
                ErrorKind::UnknownOperator,
                "-true",
                1,
            ),
            (
                "true + false;",
                "Invalid operator: Boolean(true) + Boolean(false)",
                ErrorKind::InvalidOperator,
                "true + false;",
                1,
            ),
            (
                "5; true + false; 5",
                "Invalid operator: Boolean(true) + Boolean(false)",
                ErrorKind::InvalidOperator,
                "5; true + false; 5",
                1,
            ),
            (
                "if (10 > 1) {
                    true + false; 
                }",
                "Invalid operator: Boolean(true) + Boolean(false)",
                ErrorKind::InvalidOperator,
                "true + false;",
                2,
            ),
            (
                "if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }
                        return 1;
                    }",
                "Invalid operator: Boolean(true) + Boolean(false)",
                ErrorKind::InvalidOperator,
                "return true + false;",
                3,
            ),
            (
                "foobar",
                "Identifier not found: foobar",
                ErrorKind::IdentifierNotFound,
                "foobar",
                1,
            ),
            // (
            //     "\"Hello\" - \"World\"",
            //     "Unknown operator: String(\"Hello\") - String(\"World\")",
            //     ErrorKind::UnknownOperator,
            //     "\"Hello\" - \"World\"",
            //     1,
            // ),
            (
                "if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }
                        return 1;
                    }",
                "Invalid operator: Boolean(true) + Boolean(false)",
                ErrorKind::InvalidOperator,
                "return true + false;",
                3,
            ),
            // (
            //     "if (10 > 1) {
            //             if (10 > 1) {
            //                 return true + false;
            //             }
            //             return 1;
            //         }",
            //     "Unknown operator: Boolean(true) + Boolean(false)",
            //     ErrorKind::UnknownOperator,
            //     "true + false;",
            //     1,
            // ),
            // (
            //     "if (10 > 1) {
            //             if (10 > 1) {
            //                 return true + false;
            //             }
            //             return 1;
            //         }",
            //     "Unknown operator: Boolean(true) + Boolean(false)",
            //     ErrorKind::UnknownOperator,
            //     "true + false;",
            //     1,
            // ),
        ];

        for (input, message, error_kind, line_with_err, line_nr) in tests {
            let evaluated = test_eval_fallible(input);
            match evaluated {
                Err(errors) => {
                    assert_eq!(errors.len(), 1);
                    let error = errors.first().unwrap();
                    assert_eq!(error.message, message);
                    assert_eq!(error.kind, error_kind);
                    assert_eq!(error.line.trim(), line_with_err.trim());
                    assert_eq!(error.line_nr, line_nr);
                }
                _ => panic!("No error object returned. Got: {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        match evaluated {
            Object::Function(function) => {
                assert_eq!(function.params.len(), 1);

                let params: Vec<String> = function
                    .params
                    .iter()
                    .map(|p| p.value.to_string())
                    .collect();

                assert_eq!(params, vec!["x".to_string()]);
                assert_eq!(function.body.to_string().trim(), "(x + 2);");
            }
            _ => panic!("Object is not a function. Got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            // (
            //     "let identity = fn(x) { x; }; identity(5);",
            //     Object::Integer(5),
            // ),
            // (
            //     "let identity = fn(x) { return x; }; identity(5);",
            //     Object::Integer(5),
            // ),
            // (
            //     "let double = fn(x) { x * 2; }; double(5);",
            //     Object::Integer(10),
            // ),
            // (
            //     "let add = fn(x, y) { x + y; }; add(5, 5);",
            //     Object::Integer(10),
            // ),
            // (
            //     "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            //     Object::Integer(20),
            // ),
            // ("fn(x) { x; }(5)", Object::Integer(5)),
            ("let x = 5;
            let factorial = fn(n) { if (n == 0) { return 1; } return n * factorial(n - 1); }; factorial(5);", Object::Integer(120)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }
}
