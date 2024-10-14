#[cfg(test)]
mod tests {
    use crate::{Object};
    use crate::{test_utils::test_eval_ok};
    use crate::evaluator::Error;
    use crate::evaluator::objects::ObjectTy;
    use crate::evaluator::tests::test_eval;
    use crate::parser::expressions::{InfixOperator, PrefixOperator};


    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; }; return 1; }",
                Object::Integer(10),
            ),
            (
                "9; if (10 > 1) { if (10 > 1) { return 10; }; return 1; }",
                Object::Integer(10),
            ),
            (
                "if (10 > 1) { if (10 < 1) { return 10; }; return 1; }; return 9;",
                Object::Integer(1),
            ),
            (
                "if (10 < 1) { if (10 < 1) { return 10; }; return 1; }; return 9;",
                Object::Integer(9),
            ),
            (
                "let f = fn(x) { return x; x + 10; }; f(10);",
                Object::Integer(10),
            ),
            (
                "let f = fn(x) { let result = x + 10; return result; return 10; }; f(10);",
                Object::Integer(20),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_infix_type_error_handling() {
        let infix_err = test_eval(r#"
        5 + true
        "#).unwrap_err();

        assert!(matches!(infix_err.as_ref(), Error::UnsupportedInfixOperator {
            left_object: Object::Integer(5),
            right_object: Object::Boolean(true),
            operator: InfixOperator::Plus,
            ..
        }));

        let infix_err = test_eval(r#"
        true + false
        "#).unwrap_err();
        assert!(matches!(infix_err.as_ref(), Error::UnsupportedInfixOperator {
            left_object: Object::Boolean(true),
            right_object: Object::Boolean(false),
            operator: InfixOperator::Plus,
            ..
        }));
    }

    #[test]
    fn test_prefix_type_error_handling() {
        let prefix_err = test_eval(r#"
        -true
        "#).unwrap_err();

        assert!(matches!(prefix_err.as_ref(), Error::InvalidPrefixOperatorForType {
            operator: PrefixOperator::Minus,
            right: Object::Boolean(true),
            ..
        }));
    }

    #[test]
    fn test_identifier_not_found_error() {
        let ident_err = test_eval(r#"
        foobar
        "#).unwrap_err();

        match ident_err.as_ref() {
            Error::IdentifierNotFound { identifier, span: _, similar: _ } => {
                assert_eq!(identifier.as_ref(), "foobar");
            }
            _ => panic!("Expected IdentifierNotFoundError, got: {ident_err:?}", ),
        }
    }

    #[test]
    fn test_builtin_argument_type_error() {
        let builtin_arg_err = test_eval(r#"
        len(1)
        "#).unwrap_err();

        match builtin_arg_err.as_ref() {
            Error::TypeError { span: _, expected: _, found, context: _ } => {
                assert_eq!(found, &ObjectTy::Integer);
            }
            _ => panic!("Expected TypeError, got: {builtin_arg_err:?}"),
        }
    }

    fn test_function_wrong_number_arguments() {
        let err = test_eval(r#"
        len("one", "two")
        "#).unwrap_err();

        match err.as_ref() {
            Error::WrongNumberOfArguments {
                span: _, expected, found, unexpected_arg: _
            } => {
                assert_eq!(expected, &(1..=1));
                assert_eq!(*found, 2);
            }
            _ => panic!("Expected WrongNumberOfArguments, got: {err:?}"),
        }
    }

    fn dummy() {
        let tests = vec![
            (
                "len(\"one\", \"two\")",
                "Wrong number of arguments. Expected 1 argument(s), got 2",
                "len(\"one\", \"two\")",
                1,
            ),
            (
                "[1, 2, 3][\"hi\"]",
                "Index operator not supported for Array([Integer(1), Integer(2), Integer(3)])[String(\"hi\")]",
                "[1, 2, 3][\"hi\"]",
                1,
            ),
            (
                "[1, 2, 3][-1]",
                "Index out of bounds: -1, [1, 2, 3] has len(3)",
                "[1, 2, 3][-1]",
                1,
            ),
            (
                "fn(x) { x + 1; }(1, 2)",
                "Wrong number of arguments: expected 1, got 2",
                "fn(x) { x + 1; }(1, 2)",
                1,
            ),
        ];
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "(fn(x) { x + 2; });";
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::Function(function) => {
                assert_eq!(function.params.len(), 1);

                let params: Vec<String> = function
                    .params
                    .iter()
                    .map(|p| p.value.to_string())
                    .collect();

                assert_eq!(params, vec!["x".to_string()]);
                assert_eq!(
                    function
                        .body
                        .to_string()
                        .split_whitespace()
                        .collect::<Vec<&str>>()
                        .join(" "),
                    "{ (x + 2); }".to_string()
                );
            }
            _ => panic!("Object is not a function. Got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("(fn(x) { x; })(5)", Object::Integer(5)),
            (
                "
            let x = 5; 
            let factorial = fn(n) { if (n == 0) { return 1; }; return n * factorial(n - 1); }; factorial(5);",
                Object::Integer(120),
            ),
            (
                "
            let x = 5; 
            let factorial = fn(n) { if (n == 0) { return 1; }; return n * factorial(n - 1); }; factorial(5);",
                Object::Integer(120),
            ),
            (
                "let x = 5;
            let inc_x = fn() { x = x + 1; }; inc_x(); x;",
                Object::Integer(6),
            ),
            (
                r#"
                let adder = fn(x) {
                  return fn(y) {
                    return x + y;
                  };
                };

                let add5 = adder(5);
                let result = add5(10); // result = 15
                result;
                "#,
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            println!("Testing: {}", input);
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let tests = vec![
            (r#""Hello World!""#, Object::String("Hello World!".into())),
            (
                r#""Hello" + " " + "World!""#,
                Object::String("Hello World!".to_string().into()),
            ),
            // test escape sequences
            (r#""Hello\nWorld!""#, Object::String("Hello\nWorld!".into())),
            (r#""Hello\tWorld!""#, Object::String("Hello\tWorld!".into())),
            (r#""Hello\rWorld!""#, Object::String("Hello\rWorld!".into())),
            // test string comparison
            (r#""Hello" == "Hello""#, Object::Boolean(true)),
            (r#""Hello" == "World""#, Object::Boolean(false)),
            (r#""Hello" != "Hello""#, Object::Boolean(false)),
            (r#""Hello" != "World""#, Object::Boolean(true)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            ("len(\"this is a long string, where I can't quickly count the amount of characters.\")"
             , Object::Integer(76)),
            (r#"len([1, 2, 3])"#, Object::Integer(3)),

            // test passing a builtin to a function
            (r#"let apply_fn = fn(fun, x) { fun(x); }; apply_fn(len, "hello");"#, Object::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::Array(array) => {
                assert_eq!(array.len(), 3);
                assert_eq!(array[0], Object::Integer(1));
                assert_eq!(array[1], Object::Integer(4));
                assert_eq!(array[2], Object::Integer(6));
            }
            _ => panic!("Object is not an array. Got: {:?}", evaluated),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            (
                "let f = fn() { return [1, 2, 3]; }; f()[0]",
                Object::Integer(1),
            ),
            (
                "let f = fn() { return [1, 2, 3]; }; f()[1]",
                Object::Integer(2),
            ),
            (
                "let f = fn() { return [1, 2, 3]; }; f()[2]",
                Object::Integer(3),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_mutable_values() {
        let tests = vec![
            ("let one = 1; one = 2; one;", Object::Integer(2)),
            (
                "let one = 1; one = 2; let two = one; two;",
                Object::Integer(2),
            ),
            (
                "let one = 1; one = 2; let two = one; let three = one + two + 5; three;",
                Object::Integer(9),
            ),
            (
                r#"
            let one = 1;
            one += 2;
            one;
            "#,
                Object::Integer(3),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_range_expressions() {
        let tests = vec![(
            "1..5",
            Object::Range(Object::Integer(1).into(), Object::Integer(5).into()),
        )];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_for_expressions() {
        let tests = vec![
            (
                r#"let sum = 0;
            for i in [1, 2, 3, 4, 5] {
                sum += i;
            }
            sum;"#,
                Object::Integer(15),
            ),
            (
                r#"let sum = 0;
            for i in 1..5 {
                sum += i;
            }
            sum;"#,
                Object::Integer(10),
            ),
            (
                r#"let product = 1;
            for i in 1..5 {
                product *= i;
            }
            product;"#,
                Object::Integer(24),
            ),
            (
                r#"let fib = fn(n) {
                if (n == 0) {
                    return 0;
                }
                if (n == 1) {
                    return 1;
                }
                let a = 0;
                let b = 1;

                for i in 2..=n {
                    let c = a + b;
                    a = b;
                    b = c;
                }

                return b;
            };
            fib(10);"#,
                Object::Integer(55),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    // Deprecated in favor of method calls
    // #[test]
    // fn test_push_builtin() {
    //     let tests = vec![
    //         ("let a = [1, 2, 3]; push(a, 4); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //             Object::Integer(3),
    //             Object::Integer(4),
    //         ])),
    //         ("let a = []; push(a, 1); a;", Object::Array(vec![
    //             Object::Integer(1),
    //         ])),
    //         ("let a = []; push(a, 1); push(a, 2); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //         ])),
    //         ("let a = []; push(a, 1); push(a, 2); push(a, 3); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //             Object::Integer(3),
    //         ])),
    //         ("let a = []; push(a, 1); push(a, 2); push(a, 3); push(a, 4); a;", Object::Array(vec![
    //             Object::Integer(1),
    //             Object::Integer(2),
    //             Object::Integer(3),
    //             Object::Integer(4),
    //         ])),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let evaluated = test_eval(input);
    //         assert_eq!(evaluated, expected);
    //     }
    // }

    #[test]
    fn test_eval_while_expression() {
        let tests = vec![
            (
                "let sum = 0;
            let i = 0;
            while i < 5 {
                sum += i;
                i += 1;
            }
            sum;",
                Object::Integer(10),
            ),
            (
                "let n = 10;
            let answer = while true {
                if n == 0 {
                    break 42;
                }
                n -= 1;
            };
            answer;",
                Object::Integer(42),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_break_continue() {
        test_eval_ok(
            r#"
            let sum = 0
            for i in 1..5 {
                if (i == 3) {
                    break
                }
                sum += i
            }

            assert_eq(sum, 3)
        "#,
        );

        test_eval_ok(
            r#"
            let sum = 0
            for i in 1..5 {
                if (i == 3) {
                    continue
                }
                sum += i
            }

            assert_eq(sum, 7)
            "#,
        );

        test_eval_ok(
            r#"
            let sum = for i in 1..5 {
                if (i == 3) {
                    break 17
                }
            }
            assert_eq(sum, 17)
            "#,
        );

        test_eval_ok(
            r#"
            let running_sum = 0
            let sum = for i in 1..5 {
                if (i == 3) {
                    continue
                }
                running_sum += i
                i
            }
            assert_eq(sum, 4)
            assert_eq(running_sum, 7)
            "#,
        );
    }

    #[test]
    fn test_eval_basic_program() {
        let tests = vec![
            (
                r#"
                let fib = fn(n) {
                  if n == 0 {
                    return 0;
                  }

                  if n == 1 {
                    return 1;
                  }

                  return fib(n - 1) + fib(n - 2);
                };

                fib(10);
            "#,
                Object::Integer(55),
            ),
            (
                r#"
                // Iterative Fibonacci
                let fib = fn(n) {
                  let a = 0;
                  let b = 1;

                  for i in 0..n {
                    b = a + b;
                    a = b - a;
                  }

                  return a;
                };

                fib(10);
                "#,
                Object::Integer(55),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_method_call_expression() {
        let tests = vec![
            (
                r#"let a = [1, 2, 3]; a.push(4); a;"#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                ]),
            ),
            (
                r#"let a = [1, 2, 3]; let b = a.pop(); [b, a];"#,
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
                ]),
            ),
            (r#"let a = [1, 2, 3]; a.len()"#, Object::Integer(3)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_iter_methods() {
        let tests = vec![
            (
                r#"let a = [1, 2, 3]; a.iter().map(fn(x) { x * 2; }).collect()"#,
                Object::Array(vec![
                    Object::Integer(2),
                    Object::Integer(4),
                    Object::Integer(6),
                ]),
            ),
            (
                r#"let a = [1, 2, 3]; a.iter().filter(fn(x) { x % 2 == 0; }).collect()"#,
                Object::Array(vec![Object::Integer(2)]),
            ),
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).take(6).collect();
                a;
                "#,
                Object::Array(vec![
                    Object::Integer(9),
                    Object::Integer(18),
                    Object::Integer(27),
                    Object::Integer(36),
                    Object::Integer(45),
                    Object::Integer(54),
                ]),
            ),
            // sum of first 6 multiples of 9
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).take(6).sum();
                a;
                "#,
                Object::Integer(189),
            ),
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).take(6).fold("", fn(acc, item) { acc + item.to_string(); });
                a;
                "#,
                Object::String("91827364554".into()),
            ),
            (
                r#"
                let a = (1..).iter().filter(fn(x) { x % 9 == 0; }).step_by(2).skip(2).take(4).collect();
                a;
                "#,
                Object::Array(vec![
                    Object::Integer(45),
                    Object::Integer(63),
                    Object::Integer(81),
                    Object::Integer(99),
                ]),
            ),
            (
                r#"
                let a = (1..).iter();
                a.next();
                a.next();
                a.next();
                a.next();
                a.next();
                "#,
                Object::Integer(5),
            ),
            (
                r#"
                let iter = (1..).iter();
                let iter_skip = iter.skip(5);
                iter_skip.next();
                iter_skip.next();
                "#,
                Object::Integer(7),
            ),
            (
                r#"
                let iter = (1..).iter();
                let iter_take = iter.take(5);
                iter_take.next();
                iter_take.next();
                "#,
                Object::Integer(2),
            ),
            (
                r#"
                let iter = (1..).iter();
                let iter_stepped = iter.step_by(2);
                iter_stepped.next();
                iter_stepped.next();
                "#,
                Object::Integer(3),
            ),
            (
                r#"
                let n = 1000;

                let sum_primes = fn(n) {
                    if n < 2 {
                        return 0;
                    };
                    if n == 2 {
                        return 2;
                    };
                    if n <= 4 {
                        return 5;
                    };

                    (5..=n).iter()
                      .step_by(6)
                      .fold([2, 3], fn(primes, i) {
                          if primes.iter().filter(fn(p) { i % p == 0; }).count() == 0 {
                              primes.push(i);
                          }

                          if i + 2 <= n && primes.iter().filter(fn(p) { (i + 2) % p == 0; }).count() == 0 {
                              primes.push(i + 2);
                          }
                          primes;
                      })
                      .iter()
                      .sum();
                };
                sum_primes(n);

                "#,
                Object::Integer(76127),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            let err = format!("{evaluated:?}");
            assert_eq!(evaluated.unwrap(), expected);
        }
    }

    #[test]
    fn test_eval_array_assignment() {
        let tests = vec![
            (
                r#"
            let a = [1, 2, 3];
            a[0] = 4;
            a;
            "#,
                Object::Array(vec![
                    Object::Integer(4),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            (
                r#"
            let a = [1, 2, 3];
            a[1] = 4;
            a;
            "#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(4),
                    Object::Integer(3),
                ]),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_iife_functions() {
        let tests = vec![
            (
                r#"
            (fn() { 1 + 2; })();
            "#,
                Object::Integer(3),
            ),
            (
                r#"
            (fn(x) { x * 2; })(2);
            "#,
                Object::Integer(4),
            ),
            (
                r#"
            (fn(x) { x * 2; })(2) + (fn(x) { x * 3; })(3);
            "#,
                Object::Integer(13),
            ),
            (
                r#"
            let a = 1;
            (fn() { a = 2; })();
            a;
            "#,
                Object::Integer(2),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    /// Test that function declarations are hoisted properly
    fn test_function_declaration_hoisting() {
        test_eval_ok(
            r#"
                    main();
                    fn main() {
                        assert_eq(fib(6), 8);
                    }

                    fn fib(n) {
                        let a = 0;
                        let b = 1;

                        for i in 0..n {
                            b = add(a, b);
                            a = b - a;
                        }

                        return a;
                    }

                    fn add(a, b) {
                        a + b
                    }

            "#,
        );

        // test that functions within blocks are hoisted properly
        test_eval_ok(
            r#"
                    fn main() {
                        assert_eq(fib(4), 3);
                    }

                    let fib = {
                        fn fib(n) {
                            let a = 0;
                            let b = 1;

                            for i in 0..n {
                                b = add(a, b);
                                a = b - a;
                            }

                            return a;
                        }

                        fn add(a, b) {
                            a + b
                        }

                        fib
                    }

                    main();
            "#,
        );
    }

    #[test]
    fn test_eval_struct_literals() {
        test_eval_ok(
            r#"
                let a = struct {
                    a: 1,
                    b: 2,
                };
            "#,
        );
    }

    #[test]
    fn test_function_declaration_in_block() {
        test_eval_ok(
            r#"
                let a = {
                    fn add(a, b) {
                        a + b
                    }

                    add(1, 2)
                };

                assert_eq(a, 3);
            "#,
        );

        let err = test_eval(
            r#"
                let a = {
                    fn add(a, b) {
                        a + b
                    }
                };

                assert_eq(a(1, 2), 3);
            "#,
        ).unwrap_err();

        match err.as_ref() {
            Error::CannotCall {
                function,
                span: _,
            } => {
                assert_eq!(function, &Object::Unit);
            }
            _ => panic!("Expected CannotCall, got {:?}", err),
        }
    }

    #[test]
    fn test_passing_builtin_function_as_method_argument() {
        test_eval(
            r#"
                let array = [1, 2, 3];
                array.push(4);
                assert_eq(array, [1, 2, 3, 4]);
                
                array.iter().for_each(println);
                "#
        ).unwrap();
    }
}