# Welcome to Mechylang!

Mechylang was created because I wanted to learn how human written code gets turned into machine understandable code. 
**This is a very simple language and is not meant to be used for anything other than learning.**

_This is a work in progress and is not ready for use yet._

Thanks to [Writing an Interpreter in Go](https://interpreterbook.com/) for helping me learn how to write an interpreter.

## Features

- [x] Basic arithmetic
- [x] Variables
- [x] Functions
- [x] Functions as first class citizens
- [x] Closures
- [x] If statements
- [x] For loops
- [ ] While loops
- [x] Arrays
- [ ] Objects
- [x] Strings
- [ ] Modules
- [ ] Standard library

## How to use Mechylang

First you'll need to install the interpreter.

```bash
cargo install --locked --git <TODO>
```

Then you can run a file with the interpreter.

```bash
mechylang file <file>
```

If you want to run a repl, you can do that too.

```bash
mechylang repl
```

You can also run a file and then go into the repl.

```bash
mechylang repl --file <file>
```

## Syntax

### Getting Started

Of course, every language needs a hello world program.

```rust
// This is a comment
// With the print function you can print to stdout
print("Hello World!");
```

### Variables

To create a variable, you use the `let` keyword.

```rust
let x = 5;
let y = 10;
let z = x + y;
```

Valid variable names are any combination of letters, numbers, and underscores, but they must start with a letter or underscore.

```rust
let _x = 5;
let x1 = 10;
let x_1 = 15;
let x = 20;
```

Variables are mutable, so you can change their value.

_Variable mutability being the default may change in the future._

```rust
let x = 5;
x = 10;
```

### Functions

Functions are created with the `fn` keyword.

```rust
let add = fn(x, y) {
  return x + y;
};

let result = add(5, 10); // result = 15
```

Functions are first class citizens, so you can pass them around like any other value.

```rust
let apply = fn(f, x, y) {
  return f(x, y);
};

let add = fn(x, y) {
  return x + y;
};

let result = apply(add, 5, 10); // result = 15
```

This goes for builtin functions too.

```rust
let apply = fn(f, x) {
  return f(x);
};

let result = apply(len, "Hello World!"); // result = 12
```

Functions can also be returned from other functions.

```rust
let adder = fn(x) {
  return fn(y) {
    return x + y;
  };
};

let add5 = adder(5);
let result = add5(10); // result = 15
```

### If Statements

If statements are pretty standard.

```rust
let x = 5;

if (x == 5) {
  print("x is 5");
} else {
  print("x is not 5");
}
```

The else statement is optional.


```rust
let x = 5;

if (x == 5) {
  print("x is 5");
}
```

_Support for else if statements may be added in the future._

### Loops

In Mechylang, there are two types of loops: for loops and while loops. These loops are expressions, so they return can return a value.

#### For Loops

To create a for loop, use the syntax `for <variable> in <iterable> { <body> }`.

```rust
for i in [1, 2, 3] {
  print(i);
}

// or use a range
for i in 0..3 {
  print(i);
}
```

More information on ranges can be found in the [Ranges](#ranges) section.
More information on iterables can be found in the [Iterables](#iterables) section.

#### While Loops

To create a while loop, use the syntax `while <condition> { <body> }`.

```rust
let x = 0;

while (x < 10) {
  print(x);
  x = x + 1;
}
```

#### Break and Continue

You can use the `break` and `continue` keywords to break out of a loop or skip to the next iteration.

```rust
for i in 0..10 {
  if (i == 5) {
    break;
  }
  print(i);
}
```

```rust
for i in 0..10 {
  if (i % 2 == 0) {
    continue;
  }
  print(i);
}
```

You can also break with a value. This value will be returned from the loop. This is where it becomes important that loops are expressions.

```rust
let x = for i in 0..10 {
  if (i == 5) {
    break i;
  }
  print(i); // prints 0, 1, 2, 3, 4
};

print(x); // x = 5, prints 5
```

### Arrays

Arrays are created with the syntax `[<value>, <value>, ...]`.

```rust
let x = [1, 2, 3];
```

Arrays are 0 indexed, so you can access the first element with `x[0]`.

```rust
let x = [1, 2, 3];
let y = x[0]; // y = 1
```

_Arrays are not mutable yet, but this may change in the future._

### Strings

Strings are created with the syntax `"<value>"`.

```rust
let x = "Hello World!";
```



### Iterables

Iterables are anything that can be iterated over. This includes arrays and ranges.

#### Ranges

Ranges are created with the syntax `[start]..[[=]end]`. Here anything in brackets is optional.

```rust
let x = 0..3; // 0 <= x < 3
let y = 0..=3; // 0 <= y <= 3
let z = ..3; // z < 3
let a = (6..); // a >= 6
let b = (..); // b is all integers
let c = ..=9; // c <= 9
```

_Note that ranges that are open ended on the right require parentheses._
_Note that ranges that are open ended on the left are not iterable as there is no starting point._

#### Iterating

To iterate over an iterable, you can use a for loop.

```rust
for i in 0..10 {
  print(i);
}
```

### Comments

```rust
// This is a comment
/* This is a multiline comment */
```

## Examples

### FizzBuzz

```rust
let fizzbuzz = fn(n) {
  for i in 1..=n {
    if (i % 3 == 0 && i % 5 == 0) {
      print("FizzBuzz");
      continue;
    }

    if (i % 3 == 0) {
      print("Fizz");
      continue;
    }

    if (i % 5 == 0) {
      print("Buzz");
      continue;
    }

    print(i);

  }
};

fizzbuzz(100);
```


### Fibonacci

Recursive

```rust
let fib = fn(n) {
  if (n == 0) {
    return 0;
  }

  if (n == 1) {
    return 1;
  }

  return fib(n - 1) + fib(n - 2);
};

fib(10); // 55
```

Dynamically Programmed

```rust
let fib = fn(n) {
  let a = 0;
  let b = 1;

  for i in 0..n {
    b = a + b;
    a = b - a;
  }

  return a;
};

fib(10); // 55
```

