## What is `mechylang`?

`mechylang` is a programming language that was created for the purpose of learning how to create a programming language.

It is a dynamically typed, interpreted language that is heavily inspired by Rust.

Find the full documentation on [docs.rs](https://docs.rs/mechylang/latest/mechylang/).

It supports the following features:

- Variables
- Functions (with support for closures, recursion, and higher order functions)
- Control flow (if/else, while, for)
- Comments (single line and multi line)
- Arithmetic operations
- Comparison operations
- Logical operations
- String operations (concatenation)
- Printing to the console (or using a custom function passed to the interpreter)
- Iterators and some of their methods:
    - `map`
    - `filter`
    - `fold`
    - `step_by`
    - `sum`
    - `take`

Special thanks to [Thorsten Ball](https://thorstenball.com/) for his
book [Writing An Interpreter In Go](https://interpreterbook.com/), it was a great read and a lot of help.

## How do I use `mechylang`?

### Using as a binary

Install `mechylang` using `cargo install mechylang`.

You can then run a file using `mechylang_cli <file>`. Or just run the REPL using `mechylang_cli`.

### Using as a library

If you want to use `mechylang` as a library, you can add it to your `Cargo.toml` file like so:

 ```toml
 [dependencies]
mechylang = "0.1.0"
 ```

Or just run `cargo add mechylang`.

Then, you can use it in your code like so:

```rust
use mechylang::{Evaluator, Environment, EvalConfig, Object};

let mut env = Environment::new();

let code = "let x = 5; x + 5";

let result = Evaluator::eval(code, &mut env, EvalConfig::default());
assert_eq!(result, Ok(Object::Integer(10)));
```

## Mechylang Syntax

### Hello World

No programming language is complete without a hello world example.

 ```rust
 // This is a comment
 // With the println function you can println to stdout
 println("Hello World!")
 ```

### Variables

Variables can be declared using the `let` keyword.

 ```rust
 let x = 5
 let y = 10
 assert_eq(x, 5)
 assert_eq(y, 10)
 ```

Variables can be reassigned using the `=` operator.

 ```rust
 let x = 5
 x = 10
 assert_eq(x, 10)
 ```

Valid variable names are any combination of alphabetic characters, numbers, and underscores, as long as they don't start
with a number.

 ```rust
 let x = 5
 let Y = 10
 let _ = 15
 let _z = 15
 let _Z_1 = 35
 let Οι_χαρακτήρες_που_δεν_είναι_ASCII_λειτουργούν_επίσης =
 "non-ASCII characters also work"
 ```

### Types

`mechylang` has the following types:

- Integer
- Float
- Boolean
- String
- Array
- Function
- Unit
- Iterator

### Arithmetic Operations

`mechylang` supports the following arithmetic operations:

- Addition (`+`)
- Subtraction (`-`)
- Multiplication (`*`)
- Division (`/`)
- Remainder (`%`)
- Negation (`-`)
- Bitwise Or (`|`)
- Bitwise And (`&`)
- Bitwise Xor (`^`)
- Bitwise Not (`~`)
- Bitwise Left Shift (`<<`)
- Bitwise Right Shift (`>>`)

 ```rust
 let a = 5
 let b = 10
 assert_eq(a + b, 15)
 assert_eq(a - b, -5)
 assert_eq(a * b, 50)
 assert_eq(a / b, 0)
 assert_eq(a % b, 5)
 assert_eq(-a, -5)
 assert_eq(10 | 3, 11)
 assert_eq(10 & 3, 2)
 assert_eq(10 ^ 3, 9)
 assert_eq(~10, -11)
 assert_eq(10 << 1, 20)
 assert_eq(10 >> 1, 5)
 ```

### Comparison Operations

`mechylang` supports the following comparison operations:

- Equal (`==`)
- Not Equal (`!=`)
- Less Than (`<`)
- Less Than Or Equal (`<=`)
- Greater Than (`>`)
- Greater Than Or Equal (`>=`)
- Logical And (`&&`)
- Logical Or (`||`)
- Logical Not (`!`)

 ```rust
 assert_eq(5 == 5, true)
 assert_eq(5 != 5, false)
 assert_eq(5 < 10, true)
 assert_eq(5 <= 10, true)
 assert_eq(5 > 10, false)
 assert_eq(5 >= 10, false)
 assert_eq(true && false, false)
 assert_eq(false || true, true)
 assert_eq(!true, false)
 ```

#### Unit `()`

`mechylang` has a special type `()` called `Unit`. It is the bottom type, and is used to represent the absence of a
value.
It is similar to `void` in C, or `()` in Rust.

`Unit` is the return type of a function that doesn't return anything.

 ```rust
 fn foo() {
    // This function returns unit
 }

 assert_eq(foo(), ());
 ```

### Functions

In `mechylang`, functions are first class citizens. This means that they can be passed as arguments to other functions,
and returned from other functions.

Functions can be declared in 2 ways:

- As a function declaration using the `fn <name>(<args>) { <body> }` syntax
- As an anonymous function using the `fn(<args>) { <body> }` syntax, which can be assigned to a variable

A function declaration looks like this:

 ```rust
 fn add(a, b) {
     a + b
 }

 assert_eq(add(5, 10), 15)
 ```

An anonymous function looks like this:

 ```rust
 fn(a, b) {
    a + b
 }

 // You can also assign an anonymous function to a variable
 let add = fn(a, b) {
    a + b
 }

 assert_eq(add(5, 10), 15)
 ```

The `return` keyword can be used to return early from a function.

 ```rust
 let add = fn(a, b) {
   return a + b
   // This line is never reached
   a - b
 }

 assert_eq(add(5, 10), 15)
 ```

The last expression in a function is implicitly returned.

 ```rust
 fn is_even(x) {
   if x % 2 == 0 {
     return true
   }
   false
 }
 assert_eq(is_even(10), true)
 assert_eq(is_even(11), false)
 ```

Functions can be passed as arguments to other functions.

 ```rust
 fn apply(f, a, b) {
  f(a, b)
 }

 let add = fn(a, b) {
  a + b
 }

 assert_eq(apply(add, 5, 10), 15)
 // You can also use an anonymous function without binding it to a variable
 assert_eq(apply(fn(a, b) { a - b }, 5, 10), -5)
 ```

Functions can be returned from other functions.

 ```rust
 fn make_adder(a) {
     return fn(b) {
         a + b
     }
 }

 let add_five = make_adder(5)
 let remove_five = make_adder(-5)

 assert_eq(add_five(10), 15)
 assert_eq(remove_five(10), 5)
 ```

# Function declarations are hoisted

Functions declared with the `fn <name>(<args>) { <body> }` syntax are hoisted.

This means that you can call a function before it is declared.

 ```rust
 assert_eq(add(5, 10), 15)
 fn add(a, b) {
    a + b
 }
 ```

Functions declared with the `let <name> = fn(<args>) { <body> }` syntax are not hoisted.

 ```rust
 assert_eq(add(5, 10), 15) // Results in `Identifier not found: add`
 let add = fn(a, b) {
   a + b
 }
 ```

### Built-in functions:

`mechylang` has a few built-in functions, to learn more about them, check out
the [docs.rs](https://docs.rs/mechylang/latest/mechylang/evaluator/runtime/builtins/index.html).

Built-in functions can be called like any other function. And even passed as arguments to other functions.

 ```rust
 let apply = fn(f, x) {
     f(x)
 }

 assert_eq(apply(len, "Hello World!"), 12)
 ```

### Statements and Expressions

In mechylang, almost all statements are expressions. This means that they return a value.

#### Blocks

Blocks are expressions and return the value of the last expression in the block.

 ```rust
 let x = {
    let y = 5
    y * 2
 }

 assert_eq(x, 10)
 ```

#### If/Else

The `if` statement is an expression and returns a value.
The value of the `if` expression is the value of the last expression in the block that is executed.

 ```rust
 let x = if (true) { 5 } else { 10 }
 assert_eq(x, 5)
 ```

 ```rust
 let x = if (false) { 5 } else { 10 }
 assert_eq(x, 10)
 ```

If none of the branches are executed, the value of the `if` expression is `unit`.

 ```rust
 let x = if (false) { 5 } // A false condition without an else branch returns unit
 assert_eq(x, ())
 ```

#### Loops

The `while` and `for` loops are also expressions and return a value.
The value of the loop expression is the value of the last iteration.

 ```rust
 let x = 0
 let y = while (x < 5) {
     x = x + 1
     x * 2
 }
 assert_eq(y, 10)
 ```

 ```rust
 let x = for i in 0..5 {
    i * 4
    // loops return the value of the last iteration
 }

 assert_eq(x, 16)
 ```

##### Breaking with a value

You can break out of a loop with a value using the `break` keyword.

 ```rust
 let x = for i in 0..5 {
   if i == 3 {
     break i
   }
   i * 4
 }

 assert_eq(x, 3)
 ```

Statements (like `let`) that do not produce a value return `unit` which is represented by `()`.

 ```rust
 let x = {
   let y = 5
 }
 assert_eq(x, ())
 ```

### Blocks

Blocks are a list of expressions that are evaluated sequentially.
Blocks themselves are expressions, and the last expression in a block is returned.

 ```rust
 let x = {
    let a = 5
    let b = 10
    a + b
 }

 assert_eq(x, 15)
 ```

In blocks you can also use the `return` keyword to return early.

 ```rust
 let x = {
   let a = 5
   let b = 10
   return a + b
   // This line is never reached
   a - b
 };

 assert_eq(x, 15)
 ```

Returning from a bloct within a function will return from the function.

 ```rust
 let add = fn(a, b) {
     {
         return a + b
         // This line is never reached
         a - b
     } // Since blocks are expressions and we want to evaluate this block as a statement
     // we need to add a semicolon at the end.

     // This line is never reached
     2 * (a + b)
 }

 assert_eq(add(5, 10), 15)
 ```

### Arrays

Arrays are declared using the `[]` syntax.
Arrays can contain any type of object, including other arrays and functions.

 ```rust
 let a = [1, 2, 3]
 let b = [1, 2, 3, [4, 5, 6]]
 let c = [1, 2, 3, fn(a, b) { a + b }]
 ```

Arrays can be accessed using the `[]` operator.

 ```rust
 assert_eq([1, 2, 3][0], 1)
 assert_eq([1, 2, 3][1], 2)
 assert_eq([1, 2, 3][2], 3)
 ```

Array values can be reassigned using the `[]` operator.
This will replace the value at the given index with the new value.
If the index is out of bounds, an error will be returned.
The index must be an integer.

 ```rust
 let a = [1, 2, 3]
 a[0] = 10
 assert_eq(a, [10, 2, 3])
 a[1] = 20
 assert_eq(a, [10, 20, 3])
 a[2] = 30
 assert_eq(a, [10, 20, 30])
 ```

To push an item to the end of an array, use the `push` method.

 ```rust
 let a = [1, 2, 3]
 a.push(4)
 assert_eq(a, [1, 2, 3, 4])
 # "#)
 ```

Or to remove an item from the end of an array, use the `pop` method.

 ```rust
 let a = [1, 2, 3]
 assert_eq(a.pop(), 3)
 assert_eq(a, [1, 2])
 assert_eq(a.pop(), 2)
 assert_eq(a, [1])
 ```

### Strings

Strings are declared using the `""` syntax.
Strings can contain any unicode character, including emojis.

 ```rust
 let a = "Hello World!"
 let b = "👋🌎"
 let c = a + b
 assert_eq(c, "Hello World!👋🌎")
 ```