use crate::evaluator::tests::test_eval;
use crate::Object;

#[test]
fn test_if_else_expressions() {
    let tests = vec![
        ("if (true) { 10; }", Object::Integer(10)),
        ("if (false) { 10; }", Object::Unit),
        ("if (1) { 10; }", Object::Integer(10)),
        ("if (1 < 2) { 10; }", Object::Integer(10)),
        ("if (1 > 2) { 10; }", Object::Unit),
        ("if (1 > 2) { 10; } else { 20; }", Object::Integer(20)),
        ("if (1 < 2) { 10; } else { 20; }", Object::Integer(10)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated, expected);
    }
}
