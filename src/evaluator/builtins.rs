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

use std::ops::RangeInclusive;

use crate::parser::expressions::Identifier;

use super::{environment::Environment, eval::Evaluator, objects::Object};

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
    pub function: fn(&Vec<Object>, &mut Environment, &Evaluator) -> BuiltinResult,
}

type BuiltinResult = Result<Object, (String, BuiltinError)>;

/// A list of all the builtin functions in `mechylang`
pub const BUILTINS: [BuiltinFunction; 5] = [
    // The `len` function
    //
    // `Mechylang` arguments:
    // - item: The item to get the length of
    //     - Type: `String` or `Array`
    BuiltinFunction {
        name: "len",
        args_len: (1..=1),
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
        args_len: (1..=usize::MAX),
        function: |args, _, eval| {
            eval.print(format!(
                "{}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ));
            Ok(Object::Unit)
        },
    },
    BuiltinFunction {
        name: "println",
        args_len: (1..=usize::MAX),
        function: |args, _, eval| {
            eval.print(format!(
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
        args_len: (1..=1),
        function: |args, _, eval| {
            if args[0] == Object::Boolean(true) {
                Ok(Object::Unit)
            } else {
                eval.print(format!(
                    "Assertion failed: {} is not true",
                    args[0].to_string()
                ));
                Err((
                    format!("Assertion failed: {} is not true", args[0].to_string()),
                    BuiltinError::AssertionFailed,
                ))
            }
        },
    },
    // AssertEq
    BuiltinFunction {
        name: "assert_eq",
        args_len: (2..=usize::MAX),
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
];

#[derive(Debug, PartialEq)]
pub enum BuiltinError {
    WrongArgumentType,
    AssertionFailed,
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
