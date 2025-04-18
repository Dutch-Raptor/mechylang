use crate::evaluator::tests::test_eval;

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!!true", true),
        ("!!false", false),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated.as_boolean(), Some(expected),
                    "Failed for input: {}, expected: {}, got: {}", input, expected, evaluated);
    }
}
