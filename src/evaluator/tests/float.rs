use crate::evaluator::tests::test_eval;

#[test]
fn test_eval_float_expression() {
    let tests = vec![
        ("3.0", 3.0),
        ("3.341592", 3.341592),
        ("2168615253.0", 2168615253.0),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated.as_float(), Some(expected),
                   "Failed for input: {}, expected: {}, got: {}", input, evaluated, evaluated);
    }
}
