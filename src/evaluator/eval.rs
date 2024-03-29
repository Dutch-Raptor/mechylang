use crate::evaluator::objects::function::Function;
use crate::evaluator::objects::iterators::IteratorObject;
use crate::evaluator::runtime::builtins::BuiltinFunction;
use crate::Environment;
use std::{collections::HashMap, rc::Rc};

use crate::{
    errors::{Error, ErrorKind, InterpreterErrors},
    evaluator::methods::ObjectMethods,
    lexer::{lexer::Lexer, tokens::Token},
    parser::{
        expressions::{
            ArrayLiteral, CallExpression, Expression, ExpressionToken, ForExpression, Identifier,
            IfExpression, IndexExpression, InfixExpression, InfixOperator, MemberExpression,
            PrefixExpression, PrefixOperator, StructLiteral, WhileExpression,
        },
        parser::{BlockStatement, LetStatement, Parser, Program, Statement},
    },
    trace, Object,
};

use super::{
    methods::{Method, MethodError},
    objects::traits::UnwrapReturnValue,
};

pub type EvalResult = Result<Object, InterpreterErrors>;

pub struct Evaluator {
    lines: Rc<[String]>,
    current_token: Option<Token>,
    globals: HashMap<Rc<str>, Object>,
    eval_config: Rc<EvalConfig>,
}

// We have to provide a special trait for our clonable iterator,
// since Clone requires a Sized type (so we can't call it on a trait object).
// pub trait CloneWriter: Write {
//     fn clone_box(&self) -> Box<dyn CloneWriter>;
// }
//
// // Implement our special trait for all Cloneable Iterators
// impl<T> CloneWriter for T
// where
//     T: 'static + Write + Clone,
// {
//     fn clone_box(&self) -> Box<dyn CloneWriter> {
//         Box::new(self.clone())
//     }
// }

pub struct EvalConfig {
    pub output_fn: Box<dyn Fn(String) + Send>,
}

impl Default for EvalConfig {
    fn default() -> Self {
        Self {
            output_fn: Box::new(|string| print!("{}", string)),
        }
    }
}

impl EvalConfig {
    pub fn new() -> Self {
        Self::default()
    }

    /// If you want to capture the output of the evaluator, you can use this function.
    ///
    /// This closure will be called every time the evaluator prints something.
    /// The string passed to the closure is the string that would be printed.
    ///
    /// # Examples
    /// ```
    /// use mechylang::EvalConfig;
    /// use mechylang::Evaluator;
    /// use std::sync::Arc;
    /// use std::sync::Mutex;
    ///
    /// let (sender, receiver) = std::sync::mpsc::channel();
    ///
    /// let config = EvalConfig::with_output(move |string| {
    ///    sender.send(string).unwrap();
    ///    // You can also do other things with the string here, like write it to a file.
    /// });
    ///
    /// Evaluator::eval("print(\"Hello, world!\")", &mut Default::default(), config).unwrap();
    ///
    /// assert_eq!(receiver.recv().unwrap(), "Hello, world!");
    /// ```
    pub fn with_output(output_fn: impl Fn(String) + Send + 'static) -> Self {
        Self {
            output_fn: Box::new(output_fn),
        }
    }
}

impl Evaluator {
    pub fn eval(
        input: impl Into<Rc<str>>,
        env: &mut Environment,
        config: EvalConfig,
    ) -> EvalResult {
        let input: Rc<str> = input.into();
        let lexer = Lexer::new(input);
        let lines = lexer.lines();
        let mut parser = Parser::new(lexer);

        let Program { statements } = parser.parse()?;

        let evaluator = Evaluator {
            lines,
            current_token: None,
            globals: HashMap::new(),
            eval_config: config.into(),
        };

        evaluator
            .eval_program(statements, env)
            .map_err(|err| InterpreterErrors(vec![err]))
    }

    pub(super) fn print(&self, string: String) {
        (self.eval_config.output_fn)(string);
    }

    pub fn eval_program(
        mut self,
        program: Vec<Statement>,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!("eval_program");
        let mut result = Object::Unit;

        self.eval_initial_pass(&program, env)?;

        for statement in program.into_iter() {
            result = self.eval_statement(&statement, env)?;

            if let Object::ReturnValue(val) = result {
                return Ok(*val);
            }
        }

        Ok(result)
    }

    /// Executes the initial pass of the program.
    ///
    /// This pass is used to declare functions before they are used.
    /// This is necessary because Mechyl does not have hoisting.
    fn eval_initial_pass(
        &mut self,
        program: &[Statement],
        env: &mut Environment,
    ) -> Result<(), Error> {
        for statement in program.iter() {
            if let Statement::Function(function) = statement {
                let func = Function {
                    params: function.parameters.clone(),
                    body: function.body.clone(),
                    env: env.clone(),
                };
                env.set(function.name.value.clone(), Object::Function(func));
            }
        }
        Ok(())
    }

    fn eval_statement(
        &mut self,
        statement: &Statement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("Evaluating statement: {}", statement).to_string());
        match statement {
            Statement::Expression(stmt) => self.eval_expression(&stmt.expression, env),
            Statement::Return(stmt) => {
                let val = match stmt.return_value {
                    Some(ref expr) => self.eval_expression(expr, env)?,
                    None => Object::Unit,
                };
                Ok(Object::ReturnValue(Box::new(val)))
            }
            Statement::Let(let_statement) => self.eval_let_statement(let_statement, env),
            Statement::Break(break_statement) => match &break_statement.value {
                None => Ok(Object::Break(None)),
                Some(val) => Ok(Object::Break(Some(Box::new(
                    self.eval_expression(val, env)?,
                )))),
            },
            Statement::Continue(_) => Ok(Object::Continue),
            Statement::Function(_) => {
                // Ignore function declarations after initial pass
                Ok(Object::Unit)
            }
        }
    }

    /// Evaluates a let statement.
    ///
    /// Assigns a value to a variable in the current environment.
    ///
    /// Returns `Object::Unit`.
    ///
    /// # Examples
    /// ```
    /// use mechylang::{Evaluator, Environment, Object};
    /// assert_eq!(
    ///    Evaluator::eval("let x = 5", &mut Default::default(), Default::default()).unwrap(),
    ///    Object::Unit
    /// );
    /// assert_eq!(
    ///    Evaluator::eval("let x = 5; x", &mut Default::default(), Default::default()).unwrap(),
    ///    Object::Integer(5)
    /// );
    /// ```
    fn eval_let_statement(
        &mut self,
        let_statement: &LetStatement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_let_statement: {}", let_statement));
        let val = self.eval_expression(&let_statement.value, env)?;
        env.set(let_statement.name.value.clone(), val);
        Ok(Object::Unit)
    }

    fn eval_expression(
        &mut self,
        expression: &Expression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("Evaluating expression: {}", expression).to_string());
        match expression {
            Expression::IntegerLiteral(lit) => Ok(Object::Integer(lit.value)),
            Expression::FloatLiteral(lit) => Ok(Object::Float(lit.value)),
            Expression::Boolean(boolean) => Ok(boolean.value.into()),
            Expression::Prefix(prefix) => {
                let right = self.eval_expression(&prefix.right, env)?;
                self.current_token = Some(prefix.token.clone());

                match prefix.operator {
                    PrefixOperator::Bang => Ok(self.eval_bang_operator_expression(right)),
                    PrefixOperator::Minus => Ok(self.eval_minus_prefix_operator_expression(right)?),
                    PrefixOperator::BitwiseNot => {
                        Ok(self.eval_bitwise_not_operator_expression(right)?)
                    }
                    PrefixOperator::Ampersand => {
                        Ok(self.eval_reference_operator_expression(prefix.right.clone(), env)?)
                    }
                    PrefixOperator::Asterisk => {
                        Ok(self.eval_dereference_operator_expression(right)?)
                    }
                }
            }
            Expression::Infix(infix) => self.eval_infix_expression(infix, env),
            Expression::If(if_expr) => self.eval_if_expression(if_expr, env),
            Expression::Block(block) => self.eval_scoped_block_statement(block, env),
            Expression::Identifier(ident) => self.eval_identifier(ident, env),
            Expression::Function(func) => Ok(Object::Function(Function {
                params: func.parameters.clone(),
                body: func.body.clone(),
                env: env.clone(),
            })),
            Expression::Call(call) => self.eval_call_expression(call, env),
            Expression::StringLiteral(lit) => Ok(Object::String(lit.value.clone())),
            Expression::Unit(_) => Ok(Object::Unit),
            Expression::ArrayLiteral(array) => self.eval_array_expression(array, env),
            Expression::Index(index) => self.eval_index_expression(index, env),
            Expression::For(for_expr) => self.eval_for_expression(for_expr, env),
            Expression::While(while_expr) => self.eval_while_expression(while_expr, env),
            Expression::Range(_)
            | Expression::RangeFrom(_)
            | Expression::RangeTo(_)
            | Expression::RangeFull(_) => self.eval_range_expression(expression, env),
            Expression::Member(member) => self.eval_member_expression(member, env),
            Expression::StructLiteral(lit) => self.eval_struct_expression(lit, env),
        }
    }

    fn eval_infix_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("Evaluating infix expression: {}", infix).to_string());
        // check if the infix operator is an assignment operator
        match infix.operator {
            InfixOperator::AssignEqual
            | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => {
                return self.eval_assignment_expression(infix, env);
            }

            _ => {}
        }

        let left = self.eval_expression(&infix.left, env)?;
        let right = self.eval_expression(&infix.right, env)?;

        self.current_token = Some(infix.token.clone());

        match (&left, &right) {
            (Object::Unit, _) | (_, Object::Unit) => {
                self.eval_unit_infix_expression(&infix.operator, &left, &right)
            }
            (Object::Integer(left), Object::Integer(right)) => {
                self.eval_integer_infix_expression(&infix.operator, *left, *right)
            }
            (Object::Float(left), Object::Float(right)) => {
                self.eval_float_infix_expression(&infix.operator, *left, *right)
            }
            (Object::Float(left), Object::Integer(right)) => {
                self.eval_float_infix_expression(&infix.operator, *left, *right as f64)
            }
            (Object::Integer(left), Object::Float(right)) => {
                self.eval_float_infix_expression(&infix.operator, *left as f64, *right)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.eval_boolean_infix_expression(&infix.operator, *left, *right)
            }
            (Object::String(left), Object::String(right)) => {
                self.eval_string_infix_expression(&infix.operator, left.clone(), right.clone())
            }
            _ => Err(self.error(
                self.current_token.as_ref(),
                format!("Type mismatch: {:?} {} {:?}", left, infix.operator, right).as_str(),
                ErrorKind::TypeMismatch,
            )),
        }
    }

    fn eval_scoped_block_statement(
        &mut self,
        block: &BlockStatement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let mut result = Object::Unit;

        // a block is a new scope, so create a new environment with the current environment as its parent
        let env = &mut Environment::new_enclosed(env);

        // evaluate the initial pass of the block
        self.eval_initial_pass(&block.statements, env)?;

        for statement in block.statements.iter() {
            result = self.eval_statement(statement, env)?;

            // if the result is a return value, bubble it up and stop evaluating the block
            if let Object::ReturnValue(_) = result {
                return Ok(result);
            }

            // if the result is a break, bubble it up and stop evaluating the block
            if let Object::Break(_) = result {
                return Ok(result);
            }

            // if the result is a continue, bubble it up and stop evaluating the block
            if let Object::Continue = result {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_block_statement(
        &mut self,
        block: &BlockStatement,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let mut result = Object::Unit;

        // evaluate the initial pass of the block
        self.eval_initial_pass(&block.statements, env)?;

        for statement in block.statements.iter() {
            result = self.eval_statement(statement, env)?;

            // if the result is a return value, bubble it up and stop evaluating the block
            if let Object::ReturnValue(_) = result {
                return Ok(result);
            }

            // if the result is a break, bubble it up and stop evaluating the block
            if let Object::Break(_) = result {
                return Ok(result);
            }

            // if the result is a continue, bubble it up and stop evaluating the block
            if let Object::Continue = result {
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

        if Self::is_truthy(&condition) {
            self.eval_block_statement(&if_expr.consequence, env)
        } else if let Some(alternative) = &if_expr.alternative {
            self.eval_block_statement(alternative, env)
        } else {
            Ok(Object::Unit)
        }
    }

    pub fn is_truthy(condition: &Object) -> bool {
        match condition {
            Object::Unit => false,
            Object::Boolean(boolean) => *boolean,
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
            | InfixOperator::BitwiseRightShift
            | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => Err(invalid()),
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
            InfixOperator::LogicalAnd
            | InfixOperator::LogicalOr
            | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => Err(invalid()),
        }
    }

    fn eval_float_infix_expression(
        &self,
        operator: &InfixOperator,
        left: f64,
        right: f64,
    ) -> Result<Object, Error> {
        let invalid = || {
            self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: Float({:?}) {} Float({:?})",
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

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::LogicalAnd
            | InfixOperator::LogicalOr
            | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseLeftShift
            | InfixOperator::BitwiseRightShift
            | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => Err(invalid()),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_minus_prefix_operator_expression({})", right));
        match right {
            Object::Integer(integer) => Ok((-integer).into()),
            Object::Float(float) => Ok((-float).into()),
            _ => Err(self.error(
                self.current_token.as_ref(),
                format!("Unknown operator: -{:?}", right).as_str(),
                ErrorKind::UnknownOperator,
            )),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        let _trace = trace!(&format!("eval_bang_operator_expression({})", right).to_string());
        match right {
            Object::Boolean(boolean) => (!boolean).into(),
            Object::Unit => true.into(),
            _ => false.into(),
        }
    }

    fn error(&self, token: Option<&Token>, message: &str, error_kind: ErrorKind) -> Error {
        let _trace =
            trace!(&format!("error({:?}, {}, {:?})", token, message, error_kind).to_string());

        Error::new(error_kind, message, token, &self.lines, None)
    }

    fn eval_identifier(
        &mut self,
        ident: &Identifier,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_identifier({})", ident));
        if let Some(object) = env.get(ident.value.clone()) {
            // If the value is a return value, unwrap it
            Ok(object.unwrap_return_value())
        } else if let Some(object) = self.globals.get(&ident.value) {
            Ok(object.clone())
        } else if let Ok(builtin) = BuiltinFunction::try_from(ident) {
            Ok(builtin.into())
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
        let _trace = trace!(&format!("eval_call_expression({})", call));
        let function = self.eval_expression(&call.function, env)?;

        // set current token so error messages are more helpful
        self.current_token = Some(call.function.token().clone());

        if let Object::BuiltinFunction(builtin) = function {
            return self.apply_builtin_function(&builtin, &call.arguments, env);
        }

        if let Object::Method(method) = function {
            return self.apply_method(method, &call.arguments, env);
        }

        if let Object::Unit = function {
            return Err(self.error(
                Some(call.function.token()),
                "Cannot call unit value",
                ErrorKind::TypeError,
            ));
        }

        let arguments = self.eval_expressions(&call.arguments, env)?;

        self.apply_function(function, arguments, None)
            .map_err(|err| {
                // only apply current token if there is no token set
                if err.token.is_some() {
                    return err;
                }

                return self.error(Some(call.function.token()), err.message.as_str(), err.kind);
            })
    }

    pub fn eval_function(
        function: Object,
        arguments: Vec<Object>,
        env: Option<Environment>,
        config: Rc<EvalConfig>,
    ) -> Result<Object, String> {
        let mut evaluator = Evaluator {
            globals: HashMap::new(),
            current_token: None,
            lines: vec![].into(),
            eval_config: config,
        };

        evaluator
            .apply_function(function, arguments, env)
            .map(|object| object.unwrap_return_value())
            .map_err(|err| err.message)
    }

    fn apply_function(
        &mut self,
        function: Object,
        arguments: Vec<Object>,
        _extra_env: Option<Environment>,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("apply_function({}, {:?})", function, arguments));
        let function = match function {
            Object::Function(function) => function,
            _ => {
                return Err(self.error(
                    None,
                    format!("Not a function: {}", function).as_str(),
                    ErrorKind::TypeError,
                ))
            }
        };

        let mut extended_env = self.extend_function_env(&function, arguments)?;

        // TODO: figure out if this is actually needed
        // // if any external environment was passed, extend the function environment with it
        // // this is used to pass mutable variables to functions in method calls
        // if let Some(mut env) = extra_env {
        //     let new_outer = Environment::new_enclosed(&extended_env);
        //     env.set_outer(&new_outer);
        //     extended_env = env;
        // }

        let evaluated = self.eval_block_statement(&function.body, &mut extended_env)?;

        Ok(evaluated.unwrap_return_value())
    }

    fn extend_function_env(
        &mut self,
        function: &Function,
        arguments: Vec<Object>,
    ) -> Result<Environment, Error> {
        let _trace = trace!(&format!(
            "extend_function_env({}, {:?})",
            function, arguments
        ));
        let mut env = Environment::new_enclosed(&function.env);

        if function.params.len() != arguments.len() {
            return Err(self.error(
                None,
                format!(
                    "Wrong number of arguments: expected {}, got {}",
                    function.params.len(),
                    arguments.len()
                )
                .as_str(),
                ErrorKind::WrongNumberOfArguments,
            ));
        }

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
        let _trace = trace!("eval_expressions");
        let mut result = Vec::with_capacity(arguments.len());

        for argument in arguments.iter() {
            let evaluated = self.eval_expression(argument, env)?;
            result.push(evaluated);
        }

        Ok(result)
    }

    fn eval_string_infix_expression(
        &self,
        operator: &InfixOperator,
        left: Rc<str>,
        right: Rc<str>,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!(
            "eval_string_infix_expression({:?}, {}, {})",
            operator, left, right
        ));
        match operator {
            InfixOperator::Plus => Ok((left.to_string() + &right).into()),
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::Minus
            | InfixOperator::Asterisk
            | InfixOperator::Slash
            | InfixOperator::Percent
            | InfixOperator::CompareGreater
            | InfixOperator::CompareLess
            | InfixOperator::CompareGreaterEqual
            | InfixOperator::CompareLessEqual
            | InfixOperator::LogicalAnd
            | InfixOperator::LogicalOr
            | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseLeftShift
            | InfixOperator::BitwiseRightShift
            | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => Err(self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: String({:?}) {} String({:?})",
                    left, operator, right
                )
                .as_str(),
                ErrorKind::InvalidOperator,
            )),
        }
    }

    fn apply_builtin_function(
        &mut self,
        function: &BuiltinFunction,
        arguments: &[Expression],
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!(
            "apply_builtin_function({}, {:?})",
            function.name, arguments
        ));

        if !function.args_len.contains(&arguments.len()) {
            return Err(self.error(
                self.current_token.as_ref(),
                format!(
                    "Wrong number of arguments. Expected {}, got {}",
                    match function.args_len.start() - function.args_len.end() {
                        0 => format!("{} argument(s)", function.args_len.start()),
                        _ => format!(
                            "{} to {} arguments",
                            function.args_len.start(),
                            function.args_len.end()
                        ),
                    },
                    arguments.len()
                )
                .as_str(),
                ErrorKind::WrongNumberOfArguments,
            ));
        };

        let arg_objects = self.eval_expressions(arguments, env)?;

        (function.function)(&arg_objects, env, self).map_err(|(msg, err_type)| {
            self.error(
                self.current_token.as_ref(),
                msg.as_str(),
                ErrorKind::BuiltInError(err_type),
            )
        })
    }

    fn eval_array_expression(
        &mut self,
        array: &ArrayLiteral,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let elements = self.eval_expressions(&array.elements, env)?;

        Ok(Object::Array(elements))
    }

    fn eval_index_expression(
        &mut self,
        index: &IndexExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let left = self.eval_expression(&index.left, env)?;

        let array = match left {
            Object::Array(ref arr) => arr,
            _ => {
                return Err(self.error(
                    Some(index.left.token()),
                    &format!("Expected an array. Got {:?}", left).to_string(),
                    ErrorKind::TypeError,
                ))
            }
        };

        let evaluated_index = self.eval_expression(&index.index, env)?;

        match evaluated_index {
            Object::Integer(i) => match array.get(i as usize) {
                Some(item) => Ok(item.clone()),
                None => Err(self.error(
                    Some(index.index.token()),
                    &format!(
                        "Index out of bounds: {}, {} has len({})",
                        i,
                        left,
                        array.len()
                    )
                    .to_string(),
                    ErrorKind::IndexOutOfBounds,
                )),
            },
            _ => {
                return Err(self.error(
                    Some(index.index.token()),
                    &format!(
                        "Index operator not supported for {:?}[{:?}]",
                        left, evaluated_index
                    )
                    .to_string(),
                    ErrorKind::IndexOperatorNotSupported,
                ))
            }
        }
    }

    fn eval_assignment_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_assignment_expression: {}", infix));

        // set current token to the identifier token
        self.current_token = Some(infix.left.token().clone());

        let new_value = {
            if infix.operator == InfixOperator::AssignEqual {
                self.eval_expression(&infix.right, env)?
            } else {
                self.eval_infix_expression(
                    &InfixExpression {
                        left: infix.left.clone(),
                        operator: match infix.operator {
                            InfixOperator::AssignPlus => InfixOperator::Plus,
                            InfixOperator::AssignMinus => InfixOperator::Minus,
                            InfixOperator::AssignAsterisk => InfixOperator::Asterisk,
                            InfixOperator::AssignSlash => InfixOperator::Slash,
                            InfixOperator::AssignPercent => InfixOperator::Percent,
                            InfixOperator::AssignBitwiseOr => InfixOperator::BitwiseOr,
                            InfixOperator::AssignBitwiseAnd => InfixOperator::BitwiseAnd,
                            InfixOperator::AssignBitwiseXor => InfixOperator::BitwiseXor,

                            _ => {
                                return Err(self.error(
                                    Some(&infix.token),
                                    &format!("Invalid operator: {:?}", infix.operator).to_string(),
                                    ErrorKind::InvalidOperator,
                                ))
                            }
                        },
                        right: infix.right.clone(),
                        token: infix.token.clone(),
                    },
                    env,
                )?
            }
        };

        match infix.left.as_ref() {
            Expression::Identifier(ident) => {
                env.update(ident.value.clone(), new_value).map_err(|err| {
                    self.error(
                        Some(&infix.token),
                        &format!("Error mutating variable: {}", err).to_string(),
                        ErrorKind::MutateError,
                    )
                })?;
            }
            Expression::Index(index_expr) => {
                let index = self.eval_expression(&index_expr.index, env)?;

                let mutate_fn = |ident: String| {
                    move |obj: &mut Object| {
                        let array = match obj {
                            Object::Array(arr) => arr,
                            _ => {
                                return Err(format!("Cannot index non-array: {:?}", obj).to_string())
                            }
                        };

                        let index = match index {
                            Object::Integer(i) => i as usize,
                            _ => {
                                return Err(
                                    format!("Cannot index array with {:?}", index).to_string()
                                )
                            }
                        };

                        array
                            .get_mut(index)
                            .map(|item| {
                                *item = new_value.clone();
                                Ok(Object::Unit)
                            })
                            .unwrap_or_else(|| {
                                Err(format!(
                                    "Index out of bounds: {}, {} has len({})",
                                    index,
                                    ident,
                                    array.len()
                                ))
                            })
                    }
                };

                match index_expr.left.as_ref() {
                    Expression::Identifier(ident) => {
                        env.mutate(ident.clone(), mutate_fn(ident.value.to_string()))
                    }
                    _ => {
                        return Err(self.error(
                            Some(&infix.token),
                            &format!("Cannot index non-identifier: {:?}", index_expr.left)
                                .to_string(),
                            ErrorKind::MutateError,
                        ))
                    }
                }
                .map_err(|err| {
                    self.error(
                        Some(&infix.token),
                        &format!("Error mutating variable: {}", err).to_string(),
                        ErrorKind::MutateError,
                    )
                })?;
            }

            Expression::Prefix(PrefixExpression {
                token: _,
                operator: PrefixOperator::Asterisk,
                right: _,
            }) => {
                todo!("Dereference operator not implemented yet");
                // let mut reference = match self.eval_expression(right, env)? {
                //     Object::Reference(reference) => reference,
                //     _ => {
                //         return Err(self.error(
                //             Some(token),
                //             &format!("Cannot dereference non-reference: {:?}", right).to_string(),
                //             ErrorKind::InvalidDereference,
                //         ))
                //     }
                // };
                //
                // reference.update(new_value).map_err(|err| {
                //     self.error(
                //         Some(&infix.token),
                //         &format!("Error mutating variable: {}", err).to_string(),
                //         ErrorKind::MutateError,
                //     )
                // })?;
            }

            _ => {}
        }

        Ok(Object::Unit)
    }

    fn eval_for_expression(
        &mut self,
        for_expr: &ForExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_for_expression: {}", for_expr));

        let mut result = Object::Unit;

        let iterable = self.eval_expression(&for_expr.iterable, env)?;

        let iterator = IteratorObject::try_from(iterable).map_err(|err| {
            self.error(
                Some(for_expr.iterable.token()),
                &format!("Error iterating over object: {}", err).to_string(),
                ErrorKind::TypeError,
            )
        })?;

        let mut index = 0;

        for item in iterator {
            let mut new_env = Environment::new_enclosed(env);

            new_env.set(for_expr.iterator.value.clone(), item.clone());

            if let Some(ref index_ident) = for_expr.index {
                new_env.set(index_ident.value.clone(), Object::Integer(index));
            }

            result = self.eval_block_statement(&for_expr.body, &mut new_env)?;

            index += 1;

            match result {
                Object::ReturnValue(_) => return Ok(result),
                Object::Break(Some(value)) => return Ok(*value),
                Object::Break(None) => return Ok(Object::Unit),
                // continue is handled in eval_block_statement
                _ => {}
            }
        }

        if let Some(ref else_block) = for_expr.else_block {
            result = self.eval_block_statement(else_block, env)?;
        }

        Ok(result)
    }

    fn eval_range_expression(
        &mut self,
        expression: &Expression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_range_expression: {}", expression));

        Ok(match expression {
            Expression::Range(range) => {
                if range.inclusive {
                    Object::RangeInclusive(
                        self.eval_expression(&range.left, env)?.into(),
                        self.eval_expression(&range.right, env)?.into(),
                    )
                } else {
                    Object::Range(
                        self.eval_expression(&range.left, env)?.into(),
                        self.eval_expression(&range.right, env)?.into(),
                    )
                }
            }
            Expression::RangeTo(range) => {
                if range.inclusive {
                    Object::RangeToInclusive(self.eval_expression(&range.right, env)?.into())
                } else {
                    Object::RangeTo(self.eval_expression(&range.right, env)?.into())
                }
            }
            Expression::RangeFrom(range) => {
                Object::RangeFrom(self.eval_expression(&range.left, env)?.into())
            }
            Expression::RangeFull(_) => Object::RangeFull,
            _ => {
                return Err(self.error(
                    Some(&expression.token()),
                    &format!("Invalid range expression: {:?}", expression).to_string(),
                    ErrorKind::UnexpectedToken,
                ));
            }
        })
    }

    fn eval_while_expression(
        &mut self,
        while_expr: &WhileExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_while_expression: {}", while_expr));

        let mut result = Object::Unit;

        while Self::is_truthy(&self.eval_expression(&while_expr.condition, env)?) {
            let mut new_env = Environment::new_enclosed(env);

            result = self.eval_block_statement(&while_expr.body, &mut new_env)?;

            match result {
                Object::ReturnValue(_) => return Ok(result),
                Object::Break(Some(value)) => return Ok(*value),
                Object::Break(None) => return Ok(Object::Unit),
                // continue is handled in eval_block_statement
                _ => {}
            }
        }

        if let Some(ref else_block) = while_expr.else_block {
            result = self.eval_block_statement(else_block, env)?;
        }

        Ok(result)
    }

    fn eval_member_expression(
        &mut self,
        member: &MemberExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_member_expression: {}", member));

        let object = self.eval_expression(&member.object, env)?;

        let property = member.property.value.clone();

        let ident = match *member.object {
            Expression::Identifier(ref ident) => Some(ident.value.clone()),
            _ => None,
        };

        match object.get_method(&property, ident) {
            Ok(method) => return Ok(Object::Method(method)),
            Err(MethodError::NotFound) => {}
            Err(err @ MethodError::IterMethodOnIterable(_)) => {
                return Err(self.error(
                    Some(&member.property.token),
                    &err.to_string(),
                    ErrorKind::TypeError,
                ))
            }
        };

        // try to read property from object
        Err(self.error(
            Some(&member.property.token),
            &format!(
                "Property or method '{}' not found on object: {:?}",
                property, object
            )
            .to_string(),
            ErrorKind::PropertyNotFound,
        ))
    }

    fn apply_method(
        &mut self,
        method: Method,
        arguments: &[Expression],
        env: &mut Environment,
    ) -> Result<Object, Error> {
        if !method.args_len.contains(&arguments.len()) {
            return Err(self.error(
                self.current_token.as_ref(),
                &format!(
                    "Wrong number of arguments: expected {}, got {}",
                    match method.args_len.start() - method.args_len.end() {
                        0 => format!(
                            "{} argument{}",
                            method.args_len.start(),
                            match method.args_len.start() {
                                1 => "",
                                _ => "s",
                            }
                        ),
                        _ => format!(
                            "{} to {} arguments",
                            method.args_len.start(),
                            method.args_len.end()
                        ),
                    },
                    arguments.len()
                ),
                ErrorKind::WrongNumberOfArguments,
            ));
        };

        let args = self.eval_expressions(arguments, env)?;

        (method.function)(
            *method.obj,
            method.ident.as_deref(),
            args,
            env,
            self.eval_config.clone(),
        )
        .map_err(|err| {
            self.error(
                self.current_token.as_ref(),
                &format!("Error evaluating method: {}", err),
                ErrorKind::MethodError,
            )
        })
    }

    fn eval_bitwise_not_operator_expression(&self, right: Object) -> Result<Object, Error> {
        match right {
            Object::Integer(integer) => Ok(Object::Integer(!integer)),
            _ => Err(self.error(
                self.current_token.as_ref(),
                &format!("Invalid operator: ~{:?}", right).to_string(),
                ErrorKind::InvalidOperator,
            )),
        }
    }

    fn eval_unit_infix_expression(
        &self,
        operator: &InfixOperator,
        left: &Object,
        right: &Object,
    ) -> Result<Object, Error> {
        match operator {
            InfixOperator::CompareNotEqual => Ok(Object::Boolean(left != right)),
            InfixOperator::CompareEqual => Ok(Object::Boolean(left == right)),
            _ => Err(self.error(
                self.current_token.as_ref(),
                &format!("Invalid operator: {:?} {:?} {:?}", left, operator, right).to_string(),
                ErrorKind::InvalidOperator,
            )),
        }
    }

    fn eval_reference_operator_expression(
        &self,
        right: Rc<Expression>,
        _env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_reference_operator_expression: {}", right));
        todo!()
    }

    fn eval_dereference_operator_expression(&self, right: Object) -> Result<Object, Error> {
        let _trace = trace!(&format!(
            "eval_dereference_operator_expression: {:?}",
            right
        ));
        todo!()
    }

    fn eval_struct_expression(
        &mut self,
        lit: &StructLiteral,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!("Eval struct expression");
        let mut map = HashMap::new();

        for (key, expression) in &lit.entries {
            let object = self.eval_expression(&expression, env)?;
            map.insert(key.to_string(), object);
        }

        Ok(Object::Struct(map))
    }
}

#[cfg(test)]
mod tests {

    use crate::evaluator::runtime::builtins::BuiltinError;
    use crate::test_utils::test_eval_err;
    use crate::Object;
    use crate::{errors::ErrorKind, test_utils::test_eval_ok};

    use super::{EvalResult, Evaluator};

    fn test_eval(input: &str) -> Object {
        match Evaluator::eval(input, &mut Default::default(), Default::default()) {
            Ok(object) => object,
            Err(errors) => {
                for error in errors.iter() {
                    println!("{}", error);
                    println!("{:#?}", error);
                }

                panic!("Error occured while evaluating input: {}", input);
            }
        }
    }

    fn test_eval_fallible(input: &str) -> EvalResult {
        Evaluator::eval(input, &mut Default::default(), Default::default())
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
            ("2.6 > -2.9", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    fn test_float_object(obj: Object, expected: f64) {
        match obj {
            Object::Float(float) => assert_eq!(float, expected),
            _ => panic!("Object is not a Float. Got: {:?}", obj),
        };
    }

    #[test]
    fn test_eval_float_expression() {
        let tests = vec![("3.0", 3.0), ("3.0 + 1.5", 4.5), ("-29.6", -29.6)];

        for (input, evaluated) in tests {
            test_float_object(test_eval(input), evaluated);
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
    fn test_bitwise_operations() {
        let tests = vec![
            ("~0", -1),
            ("~1", -2),
            ("~-5", 4),
            ("~5", -6),
            ("~~5", 5),
            ("~~~5", -6),
            ("~~~~5", 5),
            ("~~~~~5", -6),
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

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10; }", Object::Integer(10)),
            ("if (false) { 10; }", Object::Unit),
            ("if (1) { 10; }", Object::Integer(10)),
            ("if (1 < 2) { 10; }", Object::Integer(10)),
            ("if (1 > 2) { 10; }", Object::Unit),
            ("if (1 > 2) { 10; } else { 20; }", Object::Integer(20)),
            ("if (1 < 2) { 10; } else { 20; }", Object::Integer(10)),
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
            ("4 == { 4; }", Object::Boolean(true)),
            ("4 == { 5; }", Object::Boolean(false)),
            ("{ 4; } == 4", Object::Boolean(true)),
            ("{ 5; } == 4", Object::Boolean(false)),
            ("{ 4; } == { 4; }", Object::Boolean(true)),
            ("{ 4; } == { 5; }", Object::Boolean(false)),
            ("{ 4; 5; } == { 4; 5; }", Object::Boolean(true)),
            ("{ 4; 5; } == { 5; 4; }", Object::Boolean(false)),
            ("{ 4; 5; 6; } == { 4; 5; }", Object::Boolean(false)),
            ("{ 4; 5; } == { 4; 5; 6; }", Object::Boolean(false)),
            ("{ 4; 5; 6; } == { 4; 6; }", Object::Boolean(true)),
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
                "if (10 > 1) { if (10 > 1) { return 10; }; return 1; }",
                Object::Integer(10),
            ),
            (
                "9; if (10 > 1) { if (10 > 1) { return 10; }; return 1; }",
                Object::Integer(10),
            ),
            (
                "if (10 > 1) { if (10 < 1) { return 10; }; return 1; }; return 9;",
                Object::Integer(1),
            ),
            (
                "if (10 < 1) { if (10 < 1) { return 10; }; return 1; }; return 9;",
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
            (
                "\"Hello\" - \"World\"",
                "Invalid operator: String(\"Hello\") - String(\"World\")",
                ErrorKind::InvalidOperator,
                "\"Hello\" - \"World\"",
                1,
            ),
            (
                "len(1)",
                "Argument to `len` not supported, got Integer(1)",
                ErrorKind::BuiltInError(BuiltinError::WrongArgumentType),
                "len(1)",
                1,
            ),
            (
                "len(\"one\", \"two\")",
                "Wrong number of arguments. Expected 1 argument(s), got 2",
                ErrorKind::WrongNumberOfArguments,
                "len(\"one\", \"two\")",
                1,
            ),
            (
                "[1, 2, 3][\"hi\"]",
                "Index operator not supported for Array([Integer(1), Integer(2), Integer(3)])[String(\"hi\")]",
                ErrorKind::IndexOperatorNotSupported,
                "[1, 2, 3][\"hi\"]",
                1,
            ),
            (
                "[1, 2, 3][-1]",
                "Index out of bounds: -1, [1, 2, 3] has len(3)",
                ErrorKind::IndexOutOfBounds,
                "[1, 2, 3][-1]",
                1,
            ),
            (
                "fn(x) { x + 1; }(1, 2)",
                "Wrong number of arguments: expected 1, got 2",
                ErrorKind::WrongNumberOfArguments,
                "fn(x) { x + 1; }(1, 2)",
                1,
            ),
        ];

        for (input, message, error_kind, line_with_err, line_nr) in tests {
            let evaluated = test_eval_fallible(input);
            match evaluated {
                Err(errors) => {
                    println!("{}", errors);
                    let error = errors.0.first().unwrap();
                    assert_eq!(error.message, message);
                    assert_eq!(error.kind, error_kind);
                    assert_eq!(error.line.as_ref().unwrap().trim(), line_with_err);
                    assert_eq!(error.token.as_ref().unwrap().position.line, line_nr);
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
        let input = "(fn(x) { x + 2; });";
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
                assert_eq!(
                    function
                        .body
                        .to_string()
                        .split_whitespace()
                        .collect::<Vec<&str>>()
                        .join(" "),
                    "{ (x + 2); }".to_string()
                );
            }
            _ => panic!("Object is not a function. Got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("(fn(x) { x; })(5)", Object::Integer(5)),
            (
                "
            let x = 5; 
            let factorial = fn(n) { if (n == 0) { return 1; }; return n * factorial(n - 1); }; factorial(5);",
                Object::Integer(120),
            ),
            (
                "
            let x = 5; 
            let factorial = fn(n) { if (n == 0) { return 1; }; return n * factorial(n - 1); }; factorial(5);",
                Object::Integer(120),
            ),
            (
                "let x = 5;
            let inc_x = fn() { x = x + 1; }; inc_x(); x;",
                Object::Integer(6),
            ),
            (
                r#"
                let adder = fn(x) {
                  return fn(y) {
                    return x + y;
                  };
                };

                let add5 = adder(5);
                let result = add5(10); // result = 15
                result;
                "#,
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            println!("Testing: {}", input);
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let tests = vec![
            (r#""Hello World!""#, Object::String("Hello World!".into())),
            (
                r#""Hello" + " " + "World!""#,
                Object::String("Hello World!".to_string().into()),
            ),
            // test escape sequences
            (r#""Hello\nWorld!""#, Object::String("Hello\nWorld!".into())),
            (r#""Hello\tWorld!""#, Object::String("Hello\tWorld!".into())),
            (r#""Hello\rWorld!""#, Object::String("Hello\rWorld!".into())),
            // test string comparison
            (r#""Hello" == "Hello""#, Object::Boolean(true)),
            (r#""Hello" == "World""#, Object::Boolean(false)),
            (r#""Hello" != "Hello""#, Object::Boolean(false)),
            (r#""Hello" != "World""#, Object::Boolean(true)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            ("len(\"this is a long string, where I can't quickly count the amount of characters.\")"
            , Object::Integer(76)),

            (r#"len([1, 2, 3])"#, Object::Integer(3)),

            // test passing a builtin to a function
            (r#"let apply_fn = fn(fun, x) { fun(x); }; apply_fn(len, "hello");"#, Object::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        match evaluated {
            Object::Array(array) => {
                assert_eq!(array.len(), 3);
                assert_eq!(array[0], Object::Integer(1));
                assert_eq!(array[1], Object::Integer(4));
                assert_eq!(array[2], Object::Integer(6));
            }
            _ => panic!("Object is not an array. Got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            (
                "let f = fn() { return [1, 2, 3]; }; f()[0]",
                Object::Integer(1),
            ),
            (
                "let f = fn() { return [1, 2, 3]; }; f()[1]",
                Object::Integer(2),
            ),
            (
                "let f = fn() { return [1, 2, 3]; }; f()[2]",
                Object::Integer(3),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_mutable_values() {
        let tests = vec![
            ("let one = 1; one = 2; one;", Object::Integer(2)),
            (
                "let one = 1; one = 2; let two = one; two;",
                Object::Integer(2),
            ),
            (
                "let one = 1; one = 2; let two = one; let three = one + two + 5; three;",
                Object::Integer(9),
            ),
            (
                r#"
            let one = 1;
            one += 2;
            one;
            "#,
                Object::Integer(3),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_range_expressions() {
        let tests = vec![(
            "1..5",
            Object::Range(Object::Integer(1).into(), Object::Integer(5).into()),
        )];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_for_expressions() {
        let tests = vec![
            (
                r#"let sum = 0;
            for i in [1, 2, 3, 4, 5] {
                sum += i;
            }
            sum;"#,
                Object::Integer(15),
            ),
            (
                r#"let sum = 0;
            for i in 1..5 {
                sum += i;
            }
            sum;"#,
                Object::Integer(10),
            ),
            (
                r#"let product = 1;
            for i in 1..5 {
                product *= i;
            }
            product;"#,
                Object::Integer(24),
            ),
            (
                r#"let fib = fn(n) {
                if (n == 0) {
                    return 0;
                }
                if (n == 1) {
                    return 1;
                }
                let a = 0;
                let b = 1;

                for i in 2..=n {
                    let c = a + b;
                    a = b;
                    b = c;
                }

                return b;
            };
            fib(10);"#,
                Object::Integer(55),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    // Deprecated in favor of method calls
    // #[test]
    // fn test_push_builtin() {
    //     let tests = vec![
    //         ("let a = [1, 2, 3]; push(a, 4); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //             Object::Integer(3),
    //             Object::Integer(4),
    //         ])),
    //         ("let a = []; push(a, 1); a;", Object::Array(vec![
    //             Object::Integer(1),
    //         ])),
    //         ("let a = []; push(a, 1); push(a, 2); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //         ])),
    //         ("let a = []; push(a, 1); push(a, 2); push(a, 3); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //             Object::Integer(3),
    //         ])),
    //         ("let a = []; push(a, 1); push(a, 2); push(a, 3); push(a, 4); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //             Object::Integer(3),
    //             Object::Integer(4),
    //         ])),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let evaluated = test_eval(input);
    //         assert_eq!(evaluated, expected);
    //     }
    // }

    #[test]
    fn test_eval_while_expression() {
        let tests = vec![
            (
                "let sum = 0;
            let i = 0;
            while i < 5 {
                sum += i;
                i += 1;
            }
            sum;",
                Object::Integer(10),
            ),
            (
                "let n = 10;
            let answer = while true {
                if n == 0 {
                    break 42;
                }
                n -= 1;
            };
            answer;",
                Object::Integer(42),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_break_continue() {
        test_eval_ok(
            r#"
            let sum = 0
            for i in 1..5 {
                if (i == 3) {
                    break
                }
                sum += i
            }

            assert_eq(sum, 3)
        "#,
        );

        test_eval_ok(
            r#"
            let sum = 0
            for i in 1..5 {
                if (i == 3) {
                    continue
                }
                sum += i
            }

            assert_eq(sum, 7)
            "#,
        );

        test_eval_ok(
            r#"
            let sum = for i in 1..5 {
                if (i == 3) {
                    break 17
                }
            }
            assert_eq(sum, 17)
            "#,
        );

        test_eval_ok(
            r#"
            let running_sum = 0
            let sum = for i in 1..5 {
                if (i == 3) {
                    continue
                }
                running_sum += i
                i
            }
            assert_eq(sum, 4)
            assert_eq(running_sum, 7)
            "#,
        );
    }

    #[test]
    fn test_eval_basic_program() {
        let tests = vec![
            (
                r#"
                let fib = fn(n) {
                  if n == 0 {
                    return 0;
                  }

                  if n == 1 {
                    return 1;
                  }

                  return fib(n - 1) + fib(n - 2);
                };

                fib(10);
            "#,
                Object::Integer(55),
            ),
            (
                r#"
                // Iterative Fibonacci
                let fib = fn(n) {
                  let a = 0;
                  let b = 1;

                  for i in 0..n {
                    b = a + b;
                    a = b - a;
                  }

                  return a;
                };

                fib(10);
                "#,
                Object::Integer(55),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_method_call_expression() {
        let tests = vec![
            (
                r#"let a = [1, 2, 3]; a.push(4); a;"#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                ]),
            ),
            (
                r#"let a = [1, 2, 3]; let b = a.pop(); [b, a];"#,
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
                ]),
            ),
            (r#"let a = [1, 2, 3]; a.len()"#, Object::Integer(3)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_iter_methods() {
        let tests = vec![
            (
                r#"let a = [1, 2, 3]; a.iter().map(fn(x) { x * 2; }).collect()"#,
                Object::Array(vec![
                    Object::Integer(2),
                    Object::Integer(4),
                    Object::Integer(6),
                ]),
            ),
            (
                r#"let a = [1, 2, 3]; a.iter().filter(fn(x) { x % 2 == 0; }).collect()"#,
                Object::Array(vec![Object::Integer(2)]),
            ),
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).take(6).collect();
                a;
                "#,
                Object::Array(vec![
                    Object::Integer(9),
                    Object::Integer(18),
                    Object::Integer(27),
                    Object::Integer(36),
                    Object::Integer(45),
                    Object::Integer(54),
                ]),
            ),
            // sum of first 6 multiples of 9
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).take(6).sum();
                a;
                "#,
                Object::Integer(189),
            ),
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).take(6).fold("", fn(acc, x) { acc + x.to_string(); });
                a;
                "#,
                Object::String("91827364554".into()),
            ),
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).step_by(2).skip(2).take(4).collect();
                a;
                "#,
                Object::Array(vec![
                    Object::Integer(45),
                    Object::Integer(63),
                    Object::Integer(81),
                    Object::Integer(99),
                ]),
            ),
            (
                r#"
                let a = (1..).iter();
                a.next();
                a.next();
                a.next();
                a.next();
                a.next();
                "#,
                Object::Integer(5),
            ),
            (
                r#"
                let iter = (1..).iter();
                let iter_skip = iter.skip(5);
                iter_skip.next();
                iter_skip.next();
                "#,
                Object::Integer(7),
            ),
            (
                r#"
                let iter = (1..).iter();
                let iter_take = iter.take(5);
                iter_take.next();
                iter_take.next();
                "#,
                Object::Integer(2),
            ),
            (
                r#"
                let iter = (1..).iter();
                let iter_stepped = iter.step_by(2);
                iter_stepped.next();
                iter_stepped.next();
                "#,
                Object::Integer(3),
            ),
            (
                r#"
                let n = 1000;

                let sum_primes = fn(n) {
                    if n < 2 {
                        return 0;
                    };
                    if n == 2 {
                        return 2;
                    };
                    if n <= 4 {
                        return 5;
                    };

                    (5..=n).iter()
                      .step_by(6)
                      .fold([2, 3], fn(primes, i) {
                          if primes.iter().filter(fn(p) { i % p == 0; }).count() == 0 {
                              primes.push(i);
                          }

                          if i + 2 <= n && primes.iter().filter(fn(p) { (i + 2) % p == 0; }).count() == 0 {
                              primes.push(i + 2);
                          }
                          primes;
                      })
                      .iter()
                      .sum();
                };
                sum_primes(n);

                "#,
                Object::Integer(76127),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_array_assignment() {
        let tests = vec![
            (
                r#"
            let a = [1, 2, 3];
            a[0] = 4;
            a;
            "#,
                Object::Array(vec![
                    Object::Integer(4),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            (
                r#"
            let a = [1, 2, 3];
            a[1] = 4;
            a;
            "#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(4),
                    Object::Integer(3),
                ]),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_iife_functions() {
        let tests = vec![
            (
                r#"
            (fn() { 1 + 2; })();
            "#,
                Object::Integer(3),
            ),
            (
                r#"
            (fn(x) { x * 2; })(2);
            "#,
                Object::Integer(4),
            ),
            (
                r#"
            (fn(x) { x * 2; })(2) + (fn(x) { x * 3; })(3);
            "#,
                Object::Integer(13),
            ),
            (
                r#"
            let a = 1;
            (fn() { a = 2; })();
            a;
            "#,
                Object::Integer(2),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    /// Test that function declarations are hoisted properly
    fn test_function_declaration_hoisting() {
        test_eval_ok(
            r#"
                    main();
                    fn main() {
                        assert_eq(fib(6), 8);
                    }

                    fn fib(n) {
                        let a = 0;
                        let b = 1;

                        for i in 0..n {
                            b = add(a, b);
                            a = b - a;
                        }

                        return a;
                    }

                    fn add(a, b) {
                        a + b
                    }

            "#,
        );

        // test that functions within blocks are hoisted properly
        test_eval_ok(
            r#"
                    fn main() {
                        assert_eq(fib(4), 3);
                    }

                    let fib = {
                        fn fib(n) {
                            let a = 0;
                            let b = 1;

                            for i in 0..n {
                                b = add(a, b);
                                a = b - a;
                            }

                            return a;
                        }

                        fn add(a, b) {
                            a + b
                        }

                        fib
                    }

                    main();
            "#,
        );
    }

    #[test]
    fn test_eval_struct_literals() {
        test_eval_ok(
            r#"
                let a = struct {
                    a: 1,
                    b: 2,
                };
            "#,
        );
    }

    #[test]
    fn test_function_declaration_in_block() {
        test_eval_ok(
            r#"
                let a = {
                    fn add(a, b) {
                        a + b
                    }

                    add(1, 2)
                };

                assert_eq(a, 3);
            "#,
        );

        // cannot access function outside of block as it does not exist
        test_eval_err(
            r#"
                let a = {
                    fn add(a, b) {
                        a + b
                    }
                };

                assert_eq(a(1, 2), 3);
            "#,
            &[ErrorKind::TypeError],
        );
    }
}
