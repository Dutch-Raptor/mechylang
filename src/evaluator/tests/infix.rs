use crate::evaluator::tests::test_eval;

#[test]
fn test_eval_integer_infix_expression() {
    let tests = vec![
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ("4 % 2", 0),
        ("5 % 2", 1),
        // bitwise
        ("1 | 2", 3),
        ("1 & 2", 0),
        ("1 ^ 2", 3),
        ("1 << 2", 4),
        ("4 >> 2", 1),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated.as_integer(), Some(expected),
                   "Failed for input: {}, expected: {}, got: {}", input, expected, evaluated);
    }
}

#[test]
fn test_eval_float_infix_expression() {
    let tests = vec![
        ("3.0 + 1.5", 4.5),
        ("3.0 - 1.5", 1.5),
        ("3.0 * 1.5", 4.5),
        ("3.0 / 1.5", 2.0),
        ("3.0 % 1.5", 0.0),
        ("3.0 % 1.6", 1.4),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated.as_float(), Some(expected),
                   "Failed for input: {}, expected: {}, got: {}", input, expected, evaluated);
    }
}
