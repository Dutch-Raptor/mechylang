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
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! // This is a comment
//! // With the print function you can print to stdout
//! print("Hello World!");
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! #### Note about code examples:
//!
//! The code examples in this documentation are run as tests, so if you see a comment that says `// 5`, that means that the code should evaluate to `5`.
//! If you see a comment that says `// "Hello World!"`, that means that the code should evaluate to `"Hello World!"`.
//!
//! Behind the scenes every code example is run as a test like the following:
//! ```rust
//! use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! let result = Evaluator::eval(r#" // define the `mechylang` code to run
//! assert_eq(2 + 3, 5);
//! "#, &mut Environment::new(), EvalConfig::default());
//! assert_eq!(result, Ok(Object::Null)); // assert that the result is `Ok(Object::Null)`
//! ```
//!
//! Only the mechylang executed code is shown in the examples, not the rust code that runs the tests.
//! Mechylang automatically returns the last expression in a block, so the last expression in the code examples is the result.
//! _Since there is only one result for each code example, if multiple results need to be tested, they will be put in an array._
//!
//! ### Variables
//! Variables can be declared using the `let` keyword.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let x = 5;
//! let y = 10;
//! assert_eq(x, 5);
//! assert_eq(y, 10);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! Variables can be reassigned using the `=` operator.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let x = 5;
//! x = 10;
//! assert_eq(x, 10);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! Valid variable names are any combination of letters, numbers, and underscores, as long as they don't start with a number.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let x = 5;
//! let Y = 10;
//! let _ = 15;
//! let _z = 15;
//! let _1 = 20;
//! let _1z = 25;
//! let _z1 = 30;
//! let _Z_1 = 35;
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
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
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
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
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
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
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! assert_eq(5 == 5, true);
//! assert_eq(5 != 5, false);
//! assert_eq(5 < 10, true);
//! assert_eq(5 <= 10, true);
//! assert_eq(5 > 10, false);
//! assert_eq(5 >= 10, false);
//! assert_eq(true && false, false);
//! assert_eq(false || true, true);
//! assert_eq(!true, false);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! ### Functions
//!
//! In `mechylang`, functions are variables that can be called. Making them first class citizens.
//!
//! Functions can be declared using the `fn` keyword.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let add = fn(a, b) {
//!    a + b
//!    // The last expression in a block is returned
//!    // so there is no need to use the `return` keyword
//! }
//!
//! assert_eq(add(5, 10), 15);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! The `return` keyword can be used to return early from a function.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let add = fn(a, b) {
//!   return a + b;
//!   // This line is never reached
//!   a - b
//! }
//!
//! assert_eq(add(5, -10), -5);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! Functions can be passed as arguments to other functions.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let apply = fn(f, a, b) {
//!  f(a, b)
//! }
//!
//! let add = fn(a, b) {
//!  a + b
//! }
//!
//! assert_eq(apply(add, 5, 10), 15);
//! // Anonymous functions are also supported!
//! assert_eq(apply(fn(a, b) { a - b }, 5, 10), -5);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! Functions can be returned from other functions.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let make_adder = fn(a) {
//!     fn(b) {
//!         a + b
//!     }
//! }
//!
//! let add_five = make_adder(5);
//! let remove_five = make_adder(-5);
//!
//! assert_eq(add_five(10), 15);
//! assert_eq(remove_five(10), 5);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! ### Built in functions:
//!
//! `mechylang` has a few built in functions, to learn more about them, check out the [builtins module](crate::evaluator::builtins).
//!
//! Built in functions can be called like any other function. And even passed as arguments to other functions.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let apply = fn(f, x) {
//!     f(x)
//! }
//!
//! assert_eq(apply(len, "Hello World!"), 12);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! ### Arrays
//!
//! Arrays are declared using the `[]` syntax.
//! Arrays can contain any type of object, including other arrays and functions.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let a = [1, 2, 3];
//! let b = [1, 2, 3, [4, 5, 6]];
//! let c = [1, 2, 3, fn(a, b) { a + b }];
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! Arrays can be accessed using the `[]` operator.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! assert_eq([1, 2, 3][0], 1);
//! assert_eq([1, 2, 3][1], 2);
//! assert_eq([1, 2, 3][2], 3);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```
//!
//! To push an item to the end of an array, use the `push` method.
//!
//! ```rust
//! # use mechylang::{Evaluator, Environment, EvalConfig, Object};
//! # let result = Evaluator::eval(r#"
//! let a = [1, 2, 3];
//! a.push(4);
//! assert_eq(a, [1, 2, 3, 4]);
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Null));
//! ```

mod errors;
pub mod evaluator;
mod lexer;
mod parser;
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
