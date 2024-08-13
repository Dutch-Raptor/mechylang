use std::collections::HashMap;
use std::rc::Rc;
use crate::{Environment, Error, EvalConfig, Evaluator, Object, Span, trace};
use crate::error::ErrorKind;
use crate::evaluator::methods::Method;
use crate::evaluator::objects::function::Function;
use crate::evaluator::objects::traits::UnwrapReturnValue;
use crate::evaluator::runtime::builtins::BuiltinFunction;
use crate::parser::expressions::{CallExpression, ExpressionSpanExt};

impl Evaluator {
    pub(super) fn eval_call_expression(
        &mut self,
        call: &CallExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_call_expression({})", call));
        let function = self.eval_expression(&call.function, env)?;

        // set current token so error messages are more helpful
        self.current_span = call.function.span().clone();
        
        let arguments = call.arguments
            .iter()
            .map(|arg| self.eval_expression(arg, env))
            .collect::<Result<Vec<Object>, Error>>()?;

        if let Object::BuiltinFunction(builtin) = function {
            return self.apply_builtin_function(&builtin, arguments, env);
        }

        if let Object::Method(method) = function {
            return self.apply_method(method, arguments, env);
        }
        
        if let Object::Function(function) = function {
            return self.apply_function(function, arguments, None)
        }
        
        return Err(self.error(
            call.function.span().clone(),
            format!("Cannot call {}", function).as_str(),
            ErrorKind::TypeError,
        ))
    }


    fn apply_builtin_function(
        &mut self,
        function: &BuiltinFunction,
        arguments: Vec<Object>,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!(
            "apply_builtin_function({}, {:?})",
            function.name, arguments
        ));

        if !function.args_len.contains(&arguments.len()) {
            return Err(self.error(
                self.current_span.clone(),
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

        (function.function)(&arguments, env, &self.eval_config).map_err(|(msg, err_type)| {
            self.error(
                self.current_span.clone(),
                msg.as_str(),
                ErrorKind::BuiltInError(err_type),
            )
        })
    }

    fn apply_method(
        &mut self,
        method: Method,
        arguments: Vec<Object>,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        if !method.args_len.contains(&arguments.len()) {
            return Err(self.error(
                self.current_span.clone(),
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

        (method.function)(
            *method.obj,
            method.ident.as_deref(),
            arguments,
            env,
            self.eval_config.clone(),
        )
            .map_err(|err| {
                self.error(
                    self.current_span.clone(),
                    &format!("Error evaluating method: {}", err),
                    ErrorKind::MethodError,
                )
            })
    }

    fn apply_function(
        &mut self,
        function: Function,
        arguments: Vec<Object>,
        _extra_env: Option<Environment>,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("apply_function({}, {:?})", function, arguments));

        let mut extended_env = self.extend_function_env(&function, arguments)?;

        // TODO: figure out if this is actually needed
        // // if any external environment was passed, extend the function environment with it
        // // this is used to pass mutable variables to functions in method calls
        // if let Some(mut env) = extra_env {
        //     let new_outer = Environment::new_enclosed(&extended_env);
        //     env.set_outer(&new_outer);
        //     extended_env = env;
        // }

        let evaluated = self.eval_block_expression(&function.body, &mut extended_env)?;

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
                self.current_span.clone(),
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


    pub(in crate::evaluator) fn eval_function(
        function: Function,
        arguments: Vec<Object>,
        env: Option<Environment>,
        config: Rc<EvalConfig>,
        span: Span,
    ) -> Result<Object, String> {
        let mut evaluator = Evaluator {
            globals: HashMap::new(),
            current_span: span,
            lines: vec![].into(),
            eval_config: config,
        };

        evaluator
            .apply_function(function, arguments, env)
            .map(|object| object.unwrap_return_value())
            .map_err(|err| err.message)
    }
}