use crate::evaluator::tests::test_eval;

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("500001 ", 500001),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated.as_integer(), Some(expected),
                   "Failed for input: {}, expected: {}, got: {}", input, expected, evaluated);
    }
}


#[test]
fn test_bitwise_operations() {
    let tests = vec![
        ("~0", -1),
        ("~1", -2),
        ("~-5", 4),
        ("~5", -6),
        ("~~5", 5),
        ("~~~5", -6),
        ("~~~~5", 5),
        ("~~~~~5", -6),
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
