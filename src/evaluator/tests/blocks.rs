use crate::evaluator::tests::test_eval;
use crate::Object;

#[test]
fn test_block_expressions() {
    let tests = vec![
        ("{ 1; 2; 3; }", Object::Integer(3)),
        ("4 == { 4; }", Object::Boolean(true)),
        ("4 == { 5; }", Object::Boolean(false)),
        ("{ 4; } == 4", Object::Boolean(true)),
        ("{ 5; } == 4", Object::Boolean(false)),
        ("{ 4; } == { 4; }", Object::Boolean(true)),
        ("{ 4; } == { 5; }", Object::Boolean(false)),
        ("{ 4; 5; } == { 4; 5; }", Object::Boolean(true)),
        ("{ 4; 5; } == { 5; 4; }", Object::Boolean(false)),
        ("{ 4; 5; 6; } == { 4; 5; }", Object::Boolean(false)),
        ("{ 4; 5; } == { 4; 5; 6; }", Object::Boolean(false)),
        ("{ 4; 5; 6; } == { 4; 6; }", Object::Boolean(true)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        assert_eq!(evaluated, expected);
    }
}



