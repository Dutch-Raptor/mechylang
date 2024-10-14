//! Builtins for `mechylang`
//!
//! #### `print`
//!
//! The `print` function can be used to print to stdout.
//! It prints the string representation of the arguments passed to it, separated by spaces.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! print("Hello", "World!"); // prints "Hello World!"
//! # "#);
//! ```
//!
//! #### `println`
//!
//! The `println` function can be used to print to stdout with a newline at the end.
//! It prints the string representation of the arguments passed to it, separated by spaces.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! println("Hello", "World!"); // prints "Hello World!\n"
//! # "#);
//! ```
//!
//! #### `assert`
//! The `assert` function can be used to assert that a boolean is true.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert(true); // ok
//! # "#);
//! ```
//!
//! ```should_panic
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert(false); // panics
//! # "#);
//! ```
//!
//! #### `assert_eq`
//! The `assert_eq` function can be used to assert that all arguments are equal.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert_eq(1, 1, 1); // ok
//! # "#);
//! ```
//!
//! ```should_panic
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert_eq(1, 2, 1); // panics
//! # "#);
//! ```
//!
//! #### `input_read_all`
//! The `input_read_all` function can be used to read all input from stdin.
//! It keeps reading until EOF is reached.
//!
//! ```ignore
//! # mechylang::test_utils::test_eval_ok(r#"
//! let input = input_read_all();
//! assert_eq(input, "Hello\nWorld\n"); // ok
//! # "#);
//! ```
//!
//! #### `input_read_line`
//!
//! The `input_read_line` function can be used to read a single line from stdin.
//! It reads until a newline is reached.
//!
//! ```ignore
//! # mechylang::test_utils::test_eval_ok(r#"
//! let input = input_read_line();
//! assert_eq(input, "Hello\n"); // ok
//! # "#);
//! ```
//!
//! #### `parse_int`
//! The `parse_int` function can be used to parse a string into an integer.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let num = parse_int("123");
//! assert_eq(num, 123); // ok
//! # "#);
//! ```
//!
//! It throws an error if the string is not a valid integer.
//! ```should_panic
//! # mechylang::test_utils::test_eval_ok(r#"
//! let num = parse_int("123a");
//! # "#);
//! ```
//!
//! #### `parse_float`
//! The `parse_float` function can be used to parse a string into a float.
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let num = parse_float("123.45");
//! assert_eq(num, 123.45); // ok
//! # "#);
//! ```
//!
//! It throws an error if the string is not a valid float.
//! ```should_panic
//! # mechylang::test_utils::test_eval_ok(r#"
//! let num = parse_float("123.45a");
//! # "#);
//! ```

use crate::{EvalConfig, Span};
use crate::Object;
use std::{
    io::{stdin, BufRead},
    ops::RangeInclusive,
};
use std::rc::Rc;
use std::sync::Arc;
use lazy_static::lazy_static;
use crate::evaluator::objects::function::{Callable};
use crate::evaluator::objects::{Argument, ArgumentList, ArgumentType, BuiltinFunctionTy, FunctionTy, ObjectTy};
use crate::parser::expressions::Identifier;
use crate::evaluator::Error;
use crate::evaluator::Result;

use super::environment::Environment;

/// A builtin function that can be called from `mechylang`
#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinFunction {
    pub ty: BuiltinFunctionTy,
    /// The function that will be called when the builtin is called
    ///
    /// # Arguments
    /// - `args`: The arguments passed to the function
    /// - `env`: The environment the function is being called in
    /// - `eval`: The evaluator, used for configuration (e.g. printing)
    pub function: fn(&Vec<Argument>, &mut Environment, &EvalConfig, call_span: Span) -> Result<Object>,
}

impl BuiltinFunction {
    pub fn name(&self) -> &str {
        self.ty.name
    }
}

impl Callable for BuiltinFunction {
    fn call(&self, _obj: Option<Object>, args: Vec<Argument>, env: &mut Environment, config: Rc<EvalConfig>, call_span: Span) -> Result<Object> {
        (self.function)(&args, env, &config, call_span)
    }

    fn args_len(&self) -> RangeInclusive<usize> {
        self.ty.function_ty.arguments.args_len()
    }

    fn argument_list(&self) -> Option<ArgumentList> {
        Some(self.ty.function_ty.arguments.clone())
    }
}



lazy_static! {
/// A list of all the builtin functions in `mechylang`
    pub static ref BUILTINS: Arc<[BuiltinFunction]> = Arc::new([
    // The `len` function
    //
    // `Mechylang` arguments:
    // - item: The item to get the length of
    //     - Type: `String` or `Array`
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "len",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "item".into(), ty: ObjectTy::AnyOf { types: vec![ObjectTy::String, ObjectTy::Array { expected_item_types: None }] } }]),
                expected_return_type: Some(Box::new(ObjectTy::Integer)),
            },
        },
        function: |args, _, _, call_span| match args[0].value {
            Object::String(ref s) => Ok(Object::Integer(s.len() as i64)),
            Object::Array(ref a) => Ok(Object::Integer(a.len() as i64)),
            _ => Err(
                Error::TypeError {
                    span: args[0].span.clone().unwrap_or_default(),
                    expected: vec![ObjectTy::String, ObjectTy::Array { expected_item_types: None }],
                    found: args[0].get_type(),
                        context: Some(call_span.clone()),
                }.into(),
            ),
        },
    },
    // The `print` function
    //
    // `Mechylang` arguments:
    // - items: The items to print
    //    - Type: `Any`
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "print",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_bounded(0..=usize::MAX, vec![ArgumentType { name: "items".into(), ty: ObjectTy::Any }]),
                expected_return_type: Some(Box::new(ObjectTy::Unit)),
            },
        },
        function: |args, _, eval_config, _| {
            (eval_config.output_fn)(
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
            );
            Ok(Object::Unit)
        },
    }
    ,
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "println",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_bounded(0..=usize::MAX, vec![ArgumentType { name: "items".into(), ty: ObjectTy::Any }]),
                expected_return_type: Some(Box::new(ObjectTy::Unit)),
            },
        },
        function: |args, _, eval_config, _| {
            (eval_config.output_fn)(format!(
                "{}\n",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ));
            Ok(Object::Unit)
        },
    },
    // Assert
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "assert",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "condition".into(), ty: ObjectTy::Boolean }]),
                expected_return_type: Some(Box::new(ObjectTy::Unit)),
            },
        },
        function: |args, _, _, call_span| {
            let value = args[0].value.clone();
            if value == Object::Boolean(true) {
                Ok(Object::Unit)
            } else {
                Err(Error::AssertionFailed {
                    span: args[0].span.clone().unwrap_or_default(),
                    assert_span: call_span.clone(),
                    value: value.clone(),
                }.into())
            }
        },
    },
    // AssertEq
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "assert_eq",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_bounded(2..=usize::MAX, vec![
                    ArgumentType { name: "first".into(), ty: ObjectTy::Any },
                    ArgumentType { name: "second".into(), ty: ObjectTy::Any },
                    ArgumentType { name: "rest".into(), ty: ObjectTy::Any },
                ]),
                expected_return_type: Some(Box::new(ObjectTy::Unit)),
            },
        },
        function: |args, _, _, call_span| {
            let first = args[0].clone();
            for arg in args.iter().skip(1) {
                if first.value != arg.value {
                    return Err(Error::AssertionEqualFailed {
                        assert_span: call_span.clone(),
                        first_span: first.span.clone().unwrap_or_default(),
                        second_span: arg.span.clone().unwrap_or_default(),
                        first_value: first.value.clone(),
                        second_value: arg.value.clone(),
                    }.into());
                }
            }
            Ok(Object::Unit)
        },
    },
    // input_read_all
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "input_read_all",
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::String)),
            },
        },
        function: |_, _, _, call_span| {
            stdin()
                .lock()
                .lines()
                .collect::<std::result::Result<Vec<String>, std::io::Error>>()
                .map(|val| Object::String(val.join("\n").into()))
                .map_err(|e| Error::IOError {
                    span: Some(call_span.clone()),
                    error: e.into(),
                }.into())
        },
    },
    // input_read_line
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "input_read_line",
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::String)),
            },
        },
        function: |_, _, _, call_span| {
            stdin()
                .lock()
                .lines()
                .next()
                .map(|val| Object::String(val.unwrap_or_default().into()))
                .ok_or(Error::IOError {
                    span: Some(call_span.clone()),
                    error: std::io::Error::new(std::io::ErrorKind::Other, "Failed to get input").into(),
                }.into())
        },
    },
    // parse_int
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "parse_int",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "string".into(), ty: ObjectTy::String }]),
                expected_return_type: Some(Box::new(ObjectTy::Integer)),
            },
        },
        
        function: |args, _, _, call_span| {
            let value = args[0].value.clone();
            match value {
                Object::String(s) => s.parse::<i64>().map(Object::Integer).map_err(|e| {
                    Error::BuiltInError {
                        span: args[0].span.clone().unwrap_or_default(),
                        message: format!("Failed to parse int: {}", e),
                        error_type: BuiltinError::RuntimeError,
                    }
                }.into()),
                _ => Err(Error::TypeError {
                    span: args[0].span.clone().unwrap_or_default(),
                    expected: vec![ObjectTy::String],
                    found: value.get_type(),
                    context: Some(call_span.clone()),
                }.into()),
            }
        },
    },
    // parse_float
    BuiltinFunction {
        ty: BuiltinFunctionTy {
            name: "parse_float",
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "string".into(), ty: ObjectTy::String }]),
                expected_return_type: Some(Box::new(ObjectTy::Float)),
            },
        },
        function: |args, _, _, call_span| match &args[0].value {
            Object::String(s) => s.parse::<f64>().map(Object::Float).map_err(|e| {
                Error::BuiltInError {
                    span: args[0].span.clone().unwrap_or_default(),
                    message: format!("Failed to parse float: {}", e),
                    error_type: BuiltinError::RuntimeError,
                }
            }.into()),
            _ => Err(Error::TypeError {
                span: args[0].span.clone().unwrap_or_default(),
                expected: vec![ObjectTy::String],
                found: args[0].get_type(),
                    context: Some(call_span.clone()),
            }.into()),
        },
    },
]);
    }

#[derive(Debug, PartialEq)]
pub enum BuiltinError {
    WrongArgumentType,
    AssertionFailed,
    IOError,
    RuntimeError,
    RangeError,
}

impl TryFrom<&Identifier> for BuiltinFunction {
    type Error = ();
    fn try_from(ident: &Identifier) -> std::result::Result<Self, ()> {
        BUILTINS
            .iter()
            .find(|b| *b.name() == *ident.value)
            .cloned()
            .ok_or(())
    }
}

impl From<BuiltinFunction> for Object {
    fn from(builtin: BuiltinFunction) -> Self {
        Object::BuiltinFunction(builtin)
    }
}
