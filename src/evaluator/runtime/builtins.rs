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

use crate::EvalConfig;
use crate::Object;
use std::{
    io::{stdin, BufRead, Error},
    ops::RangeInclusive,
};
use std::rc::Rc;
use itertools::Itertools;
use crate::evaluator::objects::function::Callable;
use crate::parser::expressions::Identifier;

use super::environment::Environment;

/// A builtin function that can be called from `mechylang`
#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinFunction {
    /// The name of the builtin function
    ///
    /// This is the name that will be used to call the function
    pub name: &'static str,
    /// The number of arguments the function takes
    pub args_len: RangeInclusive<usize>,
    /// The function that will be called when the builtin is called
    ///
    /// # Arguments
    /// - `args`: The arguments passed to the function
    /// - `env`: The environment the function is being called in
    /// - `eval`: The evaluator, used for configuration (e.g. printing)
    pub function: fn(&Vec<Object>, &mut Environment, &EvalConfig) -> BuiltinResult,
}

impl Callable for BuiltinFunction {
    fn call(&self, args: Vec<Object>, env: Option<Environment>, config: Rc<EvalConfig>) -> Result<Object, String> {
        let mut env = env.unwrap_or_default();
        (self.function)(&args, &mut env, &config).map_err(|(e, b)| format!("{:?}: {}", b, e))
    }

    fn args_len(&self) -> RangeInclusive<usize> {
        self.args_len.clone()
    }
}

type BuiltinResult = Result<Object, (String, BuiltinError)>;

/// A list of all the builtin functions in `mechylang`
pub const BUILTINS: [BuiltinFunction; 9] = [
    // The `len` function
    //
    // `Mechylang` arguments:
    // - item: The item to get the length of
    //     - Type: `String` or `Array`
    BuiltinFunction {
        name: "len",
        args_len: 1..=1,
        function: |args, _, _| match args[0] {
            Object::String(ref s) => Ok(Object::Integer(s.len() as i64)),
            Object::Array(ref a) => Ok(Object::Integer(a.len() as i64)),
            _ => Err((
                format!("Argument to `len` not supported, got {:?}", args[0]),
                BuiltinError::WrongArgumentType,
            )),
        },
    },
    // The `print` function
    //
    // `Mechylang` arguments:
    // - items: The items to print
    //    - Type: `Any`
    BuiltinFunction {
        name: "print",
        args_len: 1..=usize::MAX,
        function: |args, _, eval_config| {
            (eval_config.output_fn)(
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
            );
            Ok(Object::Unit)
        },
    },
    BuiltinFunction {
        name: "println",
        args_len: 1..=usize::MAX,
        function: |args, _, eval_config| {
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
        name: "assert",
        args_len: 1..=1,
        function: |args, _, eval_config| {
            if args[0] == Object::Boolean(true) {
                Ok(Object::Unit)
            } else {
                (eval_config.output_fn)(format!("Assertion failed: {} is not true", args[0]));
                Err((
                    format!("Assertion failed: {} is not true", args[0]),
                    BuiltinError::AssertionFailed,
                ))
            }
        },
    },
    // AssertEq
    BuiltinFunction {
        name: "assert_eq",
        args_len: 2..=usize::MAX,
        function: |args, _, _| {
            let first = args[0].clone();
            for arg in args.iter().skip(1) {
                if first != *arg {
                    return Err((
                        format!("Assertion failed: {:?} != {:?}", first, arg),
                        BuiltinError::AssertionFailed,
                    ));
                }
            }
            Ok(Object::Unit)
        },
    },
    // input_read_all
    BuiltinFunction {
        name: "input_read_all",
        args_len: 0..=0,
        function: |_, _, _| {
            stdin()
                .lock()
                .lines()
                .collect::<Result<Vec<String>, Error>>()
                .map(|val| Object::String(val.iter().join("\n").into()))
                .map_err(|_| ("Failed to get input".to_string(), BuiltinError::IOError))
        },
    },
    // input_read_line
    BuiltinFunction {
        name: "input_read_line",
        args_len: 0..=0,
        function: |_, _, _| {
            stdin()
                .lock()
                .lines()
                .next()
                .map(|val| Object::String(val.unwrap_or_default().into()))
                .ok_or(("Failed to get input".to_string(), BuiltinError::IOError))
        },
    },
    // parse_int
    BuiltinFunction {
        name: "parse_int",
        args_len: 1..=1,
        function: |args, _, _| match &args[0] {
            Object::String(s) => s.parse::<i64>().map(Object::Integer).map_err(|e| {
                (
                    format!("Failed to parse int: {}", e),
                    BuiltinError::WrongArgumentType,
                )
            }),
            _ => Err((
                format!("Argument to `parse_int` not supported, got {:?}", args[0]),
                BuiltinError::WrongArgumentType,
            )),
        },
    },
    // parse_float
    BuiltinFunction {
        name: "parse_float",
        args_len: 1..=1,
        function: |args, _, _| match &args[0] {
            Object::String(s) => s.parse::<f64>().map(Object::Float).map_err(|e| {
                (
                    format!("Failed to parse float: {}", e),
                    BuiltinError::WrongArgumentType,
                )
            }),
            _ => Err((
                format!("Argument to `parse_float` not supported, got {:?}", args[0]),
                BuiltinError::WrongArgumentType,
            )),
        },
    },
];

#[derive(Debug, PartialEq)]
pub enum BuiltinError {
    WrongArgumentType,
    AssertionFailed,
    IOError,
}

impl TryFrom<&Identifier> for BuiltinFunction {
    type Error = ();
    fn try_from(ident: &Identifier) -> Result<Self, ()> {
        BUILTINS
            .iter()
            .find(|b| *b.name == *ident.value)
            .cloned()
            .ok_or(())
    }
}

impl From<BuiltinFunction> for Object {
    fn from(builtin: BuiltinFunction) -> Self {
        Object::BuiltinFunction(builtin)
    }
}
