use crate::evaluator::tests::test_eval;
use crate::Object;

fn test_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(boolean) => assert_eq!(boolean, expected),
        _ => panic!("Object is not a Boolean. Got: {:?}", obj),
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
        ("2.6 > -2.9", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input).unwrap();
        test_boolean_object(evaluated, expected);
    }
}
