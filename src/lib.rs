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
//! use mechylang::evaluator::eval::Evaluator;
//! use mechylang::evaluator::environment::Environment;
//! use mechylang::evaluator::eval::EvalConfig;
//! use mechylang::evaluator::objects::Object;
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
//! # use mechylang::evaluator::{eval::Evaluator, environment::Environment, eval::EvalConfig, objects::Object};
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
//! use mechylang::evaluator::{
//!     eval::Evaluator,
//!     environment::Environment,
//!     eval::EvalConfig,
//!     objects::Object
//! };
//! let result = Evaluator::eval(r#" // define the `mechylang` code to run
//! 5
//! "#, &mut Environment::new(), EvalConfig::default());
//! assert_eq!(result, Ok(Object::Integer(5))); // specify the expected result
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
//! # use mechylang::evaluator::{eval::Evaluator, environment::Environment, eval::EvalConfig, objects::Object};
//! # let result = Evaluator::eval(r#"
//! let x = 5;
//! let y = 10;
//! x + y // 15
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Integer(15)));
//! ```
//!
//! Variables can be reassigned using the `=` operator.
//!
//! ```rust
//! # use mechylang::evaluator::{eval::Evaluator, environment::Environment, eval::EvalConfig, objects::Object};
//! # let result = Evaluator::eval(r#"
//! let x = 5;
//! x = 10;
//! x // 10
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Integer(10)));
//! ```
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
//! - Not (`!`)
//! - Bitwise Or (`|`)
//! - Bitwise And (`&`)
//! - Bitwise Xor (`^`)
//!
//! - Bitwise Left Shift (`<<`)
//! - Bitwise Right Shift (`>>`)
//!
//! ```rust
//! # use mechylang::evaluator::{eval::Evaluator, environment::Environment, eval::EvalConfig, objects::Object};
//! # let result = Evaluator::eval(r#"
//! let a = 5;
//! let b = 10;
//! [
//!     a + b, // 15
//!     a - b, // -5
//!     a * b, // 50
//!     a / b, // 0 (integer division)
//!     a % b, // 5
//!     -a, // -5
//!     !a, // false
//!     a | b, // 15
//!     a & b, // 0
//!     a ^ b, // 15
//!     a << b, // 5120
//!     a >> b, // 0
//!     0.5 + 0.5, // 1.0
//!     33.7 / 0.7, // 48.14285714285715
//! ]
//! # "#, &mut Environment::new(), EvalConfig::default());
//! # assert_eq!(result, Ok(Object::Array(vec![
//! #     Object::Integer(15),
//! #     Object::Integer(-5),
//! #     Object::Integer(50),
//! #     Object::Integer(0),
//! #     Object::Integer(5),
//! #     Object::Integer(-5),
//! #     Object::Boolean(false),
//! #     Object::Integer(15),
//! #     Object::Integer(0),
//! #     Object::Integer(15),
//! #     Object::Integer(5120),
//! #     Object::Integer(0),
//! #     Object::Float(1.0),
//! #     Object::Float(48.14285714285715),
//! # ])));
//! ```

pub mod errors;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod tracer;
