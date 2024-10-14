use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::rc::Rc;
use crate::{Environment, EvalConfig, Evaluator, Object, Span, trace};
use crate::evaluator::methods::{Method, MethodArgs};
use crate::evaluator::objects::function::{Callable, Function};
use crate::evaluator::objects::traits::UnwrapReturnValue;
use crate::evaluator::runtime::builtins::BuiltinFunction;
use crate::evaluator::{Error, Result};
use crate::evaluator::objects::Argument;
use crate::parser::expressions::{CallExpression, ExpressionSpanExt};


impl Evaluator {
    pub(super) fn eval_call_expression(
        &mut self,
        call: &CallExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_call_expression({})", call));
        let function = self.eval_expression(&call.function, env)?;

        // set current token so error messages are more helpful
        self.current_span = call.function.span().clone();
        let call_span = call.span.clone();

        let arguments = call.arguments
            .iter()
            .map(|arg| Ok(Argument {
                span: Some(arg.span().clone()),
                value: self.eval_expression(arg, env)?,
            }))
            .collect::<Result<Vec<Argument>>>()?;

        if let Object::BuiltinFunction(builtin) = function {
            return self.apply_builtin_function(&builtin, arguments, env, call_span);
        }

        if let Object::Method(method) = function {
            return self.apply_method(method, arguments, env, call_span);
        }

        if let Object::Function(function) = function {
            return self.apply_function(function, arguments, call_span);
        }

        Err(Error::CannotCall {
            function,
            span: call.function.span().clone(),
        }.into())
    }


    fn apply_builtin_function(
        &mut self,
        function: &BuiltinFunction,
        arguments: Vec<Argument>,
        env: &mut Environment,
        call_span: Span,
    ) -> Result<Object> {
        let _trace = trace!(&format!(
            "apply_builtin_function({}, {:?})",
            function.ty.name, arguments
        ));

        Self::validate_argument_len(function.args_len(), &arguments, call_span.clone())?;

        (function.function)(&arguments, env, &self.eval_config, call_span)
    }


    pub(in crate::evaluator) fn apply_method(
        &mut self,
        method: Method,
        arguments: Vec<Argument>,
        env: &mut Environment,
        call_span: Span,
    ) -> Result<Object> {
        Self::validate_argument_len(method.args_len(), &arguments, call_span.clone())?;

        (method.function)(
            MethodArgs {
                obj_span: method.obj_span.clone(),
                method_span: method.method_span.clone(),
                obj: *method.obj,
                obj_identifier: method.ident,
                args: arguments,
                env,
                config: self.eval_config.clone(),
                call_span,
            }
        )
    }

    fn apply_function(
        &mut self,
        function: Function,
        arguments: Vec<Argument>,
        call_span: Span,
    ) -> Result<Object> {
        let _trace = trace!(&format!("apply_function({}, {:?})", function, arguments));

        let mut extended_env = self.extend_function_env(
            &function,
            arguments,
            call_span)?;

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
        arguments: Vec<Argument>,
        call_span: Span,
    ) -> Result<Environment> {
        let _trace = trace!(&format!(
            "extend_function_env({}, {:?})",
            function, arguments
        ));
        let mut env = Environment::new_enclosed(&function.env);

        Self::validate_argument_len(function.args_len(), &arguments, call_span)?;

        for (parameter, argument) in function.params.iter().zip(arguments) {
            env.set(parameter.value.clone(), argument.value);
        }

        Ok(env)
    }

    fn validate_argument_len(
        args_len: RangeInclusive<usize>,
        arguments: &[Argument],
        call_span: Span
    ) -> Result<()> {
        let unexpected_arg = if arguments.len() > *args_len.end() {
            Some((*args_len.end() + 1, arguments[*args_len.end()].clone()))
        } else {
            None
        };

        if !args_len.contains(&arguments.len()) {
            return Err(
                Error::WrongNumberOfArguments {
                    span: call_span,
                    unexpected_arg,
                    expected: args_len,
                    found: arguments.len(),
                }.into()
            );
        }
        Ok(())
    }

    pub(in crate::evaluator) fn eval_function(
        function: Function,
        arguments: Vec<Argument>,
        config: Rc<EvalConfig>,
        span: Span,
        call_span: Span,
    ) -> Result<Object> {
        let mut evaluator = Evaluator {
            globals: HashMap::new(),
            current_span: span,
            eval_config: config,
        };

        evaluator
            .apply_function(function, arguments, call_span)
            .map(|object| object.unwrap_return_value())
    }
}