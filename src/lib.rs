// #![warn(missing_docs)]
//! A dynamically typed, interpreted programming language.
//! This is the documentation for the `mechylang` crate.
//!
//! ## What is `mechylang`?
//!
//! `mechylang` is a programming language that was created for the purpose of learning how to create a programming language.
//!
//! It is a dynamically typed, interpreted language that is heavily inspired by Rust.
//!
//! It supports the following features:
//! - Variables
//! - Functions (with support for closures, recursion, and higher order functions)
//! - Control flow (if/else, while, for)
//! - Comments (single line and multi line)
//! - Arithmetic operations
//! - Comparison operations
//! - Logical operations
//! - String operations (concatenation)
//! - Printing to the console (or using a custom function passed to the interpreter)
//! - Iterators and some of their methods:
//!     - `map`
//!     - `filter`
//!     - `fold`
//!     - `step_by`
//!     - `sum`
//!     - `take`
//!
//! Special thanks to [Thorsten Ball](https://thorstenball.com/) for his book [Writing An Interpreter In Go](https://interpreterbook.com/), it was a great read and a lot of help.
//!
//! ## How do I use `mechylang`?
//!
//! Your currently looking at the documentation for the `mechylang` library crate. There is also a binary crate that can be used to run `mechylang` programs. It supports running a file, or opening a `repl`.
//!
//! If you want to use the library crate, you can add it to your `Cargo.toml` file like so:
//!
//! ```toml
//! [dependencies]
//! mechylang = "0.1.0"
//! ```
//!
//! Then, you can use it in your code like so:
//!
//! ```rust
//! use mechylang::{Evaluator, Environment, EvalConfig, Object};
//!
//! # fn main() {
//! let mut env = Environment::new();
//!
//! let code = "let x = 5; x + 5";
//!
//! let result = Evaluator::eval(code, &mut env, EvalConfig::default());
//! assert_eq!(result, Ok(Object::Integer(10)));
//! # }
//! ```
//!
//! ## Mechylang Syntax
//!
//! ### Hello World
//!
//! No programming language is complete without a hello world example.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! // This is a comment
//! // With the print function you can print to stdout
//! print("Hello World!");
//! # "#);
//! ```
//!
//! #### Note about code examples:
//!
//! All code examples in this documentation are automatically tested using the [test_eval_ok] function.
//!
//! [test_eval_ok]: crate::test_utils::test_eval_ok
//! ```rust
//! mechylang::test_utils::test_eval_ok(r#"
//! assert_eq(2 + 3, 5);
//! "#);
//! ```
//! Only the mechylang executed code is shown in the examples, not the rust code that runs the tests.
//!
//! ### Variables
//! Variables can be declared using the `let` keyword.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = 5;
//! let y = 10;
//! assert_eq(x, 5);
//! assert_eq(y, 10);
//! # "#);
//! ```
//!
//! Variables can be reassigned using the `=` operator.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = 5;
//! x = 10;
//! assert_eq(x, 10);
//! # "#);
//! ```
//!
//! Valid variable names are any combination of alphabetic characters, numbers, and underscores, as long as they don't start with a number.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = 5;
//! let Y = 10;
//! let _ = 15;
//! let _z = 15;
//! let _Z_1 = 35;
//! let Οι_χαρακτήρες_που_δεν_είναι_ASCII_λειτουργούν_επίσης =
//! "non-ASCII characters also work";
//! # "#);
//! ```
//!
//!
//! ### To semi-colon or not to semi-colon
//!
//! Semi-colons are optional in `mechylang`. If you don't want to use them, you usually don't have to.
//!
//! Sometimes, code without semi-colons can be ambiguous, so you can use a semi-colon to make it clear what you want.
//!
//! Cases where a semi-colon is required:
//! - When putting multiple statements on the same line
//! - When using an empty `return` statement
//!     ```rust
//!     # mechylang::test_utils::test_eval_ok(r#"
//!     // This function returns unit
//!     let foo = fn() {
//!         return;
//!         5
//!     }
//!
//!     assert_eq(foo(), ());
//!
//!     // This function returns 5
//!     let bar = fn() {
//!         return
//!         5
//!     }
//!
//!     assert_eq(bar(), 5);
//!    # "#);
//!    ```
//!
//!
//! ### Types
//!
//! `mechylang` has the following types:
//! - Integer
//! - Float
//! - Boolean
//! - String
//! - Array
//! - Function
//!
//!
//! ### Arithmetic Operations
//! `mechylang` supports the following arithmetic operations:
//! - Addition (`+`)
//! - Subtraction (`-`)
//! - Multiplication (`*`)
//! - Division (`/`)
//! - Remainder (`%`)
//! - Negation (`-`)
//! - Bitwise Or (`|`)
//! - Bitwise And (`&`)
//! - Bitwise Xor (`^`)
//! - Bitwise Not (`~`)
//! - Bitwise Left Shift (`<<`)
//! - Bitwise Right Shift (`>>`)
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let a = 5;
//! let b = 10;
//! assert_eq(a + b, 15);
//! assert_eq(a - b, -5);
//! assert_eq(a * b, 50);
//! assert_eq(a / b, 0);
//! assert_eq(a % b, 5);
//! assert_eq(-a, -5);
//! assert_eq(10 | 3, 11);
//! assert_eq(10 & 3, 2);
//! assert_eq(10 ^ 3, 9);
//! assert_eq(~10, -11);
//! assert_eq(10 << 1, 20);
//! assert_eq(10 >> 1, 5);
//! # "#);
//! ```
//!
//! ### Comparison Operations
//!
//! `mechylang` supports the following comparison operations:
//! - Equal (`==`)
//! - Not Equal (`!=`)
//! - Less Than (`<`)
//! - Less Than Or Equal (`<=`)
//! - Greater Than (`>`)
//! - Greater Than Or Equal (`>=`)
//! - Logical And (`&&`)
//! - Logical Or (`||`)
//! - Logical Not (`!`)
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert_eq(5 == 5, true);
//! assert_eq(5 != 5, false);
//! assert_eq(5 < 10, true);
//! assert_eq(5 <= 10, true);
//! assert_eq(5 > 10, false);
//! assert_eq(5 >= 10, false);
//! assert_eq(true && false, false);
//! assert_eq(false || true, true);
//! assert_eq(!true, false);
//! # "#);
//! ```
//!
//! ### Functions
//!
//! In `mechylang`, functions are first class citizens. This means that they can be passed as arguments to other functions, and returned from other functions.
//!
//! Functions can be declared in 2 ways:
//! - As a function declaration using the `fn <name>(<args>) { <body> }` syntax
//! - As an "anonymous" function bound to a variable with the `let <name> = fn(<args>) { <body> }` syntax
//!
//! A function declaration looks like this:
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! fn add(a, b) {
//!     a + b
//! }
//!
//! assert_eq(add(5, 10), 15);
//! # "#);
//! ```
//!
//! An anonymous function looks like this:
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let add = fn(a, b) {
//!    a + b
//! }
//!
//! assert_eq(add(5, 10), 15);
//! # "#);
//! ```
//!
//! The `return` keyword can be used to return early from a function.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let add = fn(a, b) {
//!   return a + b;
//!   // This line is never reached
//!   a - b
//! }
//!
//! assert_eq(add(5, 10), 15);
//! # "#);
//! ```
//!
//! Functions can be passed as arguments to other functions.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! fn apply(f, a, b) {
//!  f(a, b)
//! }
//!
//! let add = fn(a, b) {
//!  a + b
//! }
//!
//! assert_eq(apply(add, 5, 10), 15);
//! // You can also use an anonymous function without binding it to a variable
//! assert_eq(apply(fn(a, b) { a - b }, 5, 10), -5);
//! # "#);
//! ```
//!
//! Functions can be returned from other functions.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! fn make_adder(a) {
//!     return fn(b) {
//!         a + b
//!     }
//! }
//!
//! let add_five = make_adder(5);
//! let remove_five = make_adder(-5);
//!
//! assert_eq(add_five(10), 15);
//! assert_eq(remove_five(10), 5);
//! # "#);
//! ```
//!
//! # Function declarations are hoisted
//!
//! Functions declared with the `fn <name>(<args>) { <body> }` syntax are hoisted.
//!
//! This means that you can call a function before it is declared.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert_eq(add(5, 10), 15);
//! fn add(a, b) {
//!    a + b
//! }
//! # "#);
//! ```
//!
//! Functions declared with the `let <name> = fn(<args>) { <body> }` syntax are not hoisted.
//!
//! ```should_panic
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert_eq(add(5, 10), 15); // Results in `Identifier not found: add`
//! let add = fn(a, b) {
//!   a + b
//! }
//! # "#);
//! ```
//!
//!
//!
//!
//! ### Built in functions:
//!
//! `mechylang` has a few built in functions, to learn more about them, check out the [builtins module](crate::evaluator::builtins).
//!
//! Built in functions can be called like any other function. And even passed as arguments to other functions.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let apply = fn(f, x) {
//!     f(x)
//! }
//!
//! assert_eq(apply(len, "Hello World!"), 12);
//! # "#);
//! ```
//!
//! ### Blocks
//!
//! Blocks are a list of expressions that are evaluated sequentially.
//! Blocks themselves are expressions, and the last expression in a block is returned.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = {
//!    let a = 5;
//!    let b = 10;
//!    a + b
//! }
//!
//! assert_eq(x, 15);
//! # "#);
//! ```
//!
//! In blocks you can also use the `return` keyword to return early.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let x = {
//!   let a = 5;
//!   let b = 10;
//!   return a + b;
//!   // This line is never reached
//!   a - b
//! };
//!
//! assert_eq(x, 15);
//! # "#);
//! ```
//!
//! Returning from a bloct within a function will return from the function.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let add = fn(a, b) {
//!     {
//!         return a + b;
//!         // This line is never reached
//!         a - b
//!     }; // Since blocks are expressions and we want to evaluate this block as a statement
//!     // we need to add a semicolon at the end.
//!
//!     // This line is never reached
//!     2 * (a + b)
//! }
//!
//! assert_eq(add(5, 10), 15);
//! # "#);
//! ```
//!
//! ### Arrays
//!
//! Arrays are declared using the `[]` syntax.
//! Arrays can contain any type of object, including other arrays and functions.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let a = [1, 2, 3];
//! let b = [1, 2, 3, [4, 5, 6]];
//! let c = [1, 2, 3, fn(a, b) { a + b }];
//! # "#);
//! ```
//!
//! Arrays can be accessed using the `[]` operator.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! assert_eq([1, 2, 3][0], 1);
//! assert_eq([1, 2, 3][1], 2);
//! assert_eq([1, 2, 3][2], 3);
//! # "#);
//! ```
//!
//! Array values can be reassigned using the `[]` operator.
//! This will replace the value at the given index with the new value.
//! If the index is out of bounds, an error will be returned.
//! The index must be an integer.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let a = [1, 2, 3];
//! a[0] = 10;
//! assert_eq(a, [10, 2, 3]);
//! a[1] = 20;
//! assert_eq(a, [10, 20, 3]);
//! a[2] = 30;
//! assert_eq(a, [10, 20, 30]);
//! # "#);
//! ```
//!
//! To push an item to the end of an array, use the `push` method.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let a = [1, 2, 3];
//! a.push(4);
//! assert_eq(a, [1, 2, 3, 4]);
//! # "#);
//! ```
//!
//! Or to remove an item from the end of an array, use the `pop` method.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let a = [1, 2, 3];
//! assert_eq(a.pop(), 3);
//! assert_eq(a, [1, 2]);
//! assert_eq(a.pop(), 2);
//! assert_eq(a, [1]);
//! # "#);
//! ```
//!
//! To learn more about array methods, check out the [array
//! module](crate::evaluator::methods::array_methods)
//!
//!
//! ### Strings
//!
//! Strings are declared using the `""` syntax.
//! Strings can contain any unicode character, including emojis.
//!
//! ```rust
//! # mechylang::test_utils::test_eval_ok(r#"
//! let a = "Hello World!";
//! let b = "👋🌎";
//! let c = a + b;
//! assert_eq(c, "Hello World!👋🌎");
//! # "#);
//! ```
//!
//!
//! ### Examples
//!
//! To see some examples, check out [examples]
//!
//! [examples]: crate::docs::examples

pub mod docs;
mod errors;
pub mod evaluator;
mod lexer;
mod parser;
pub mod test_utils;
mod tracer;

pub use errors::Error;
pub use evaluator::environment::Environment;
pub use evaluator::eval::EvalConfig;
pub use evaluator::eval::Evaluator;
pub use evaluator::eval_file;
pub use evaluator::objects::Object;
pub use lexer::lexer::Lexer;
pub use lexer::tokens::Token;
pub use parser::parser::Parser;
