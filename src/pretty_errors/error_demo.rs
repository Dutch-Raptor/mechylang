use crate::pretty_errors::PrettyError;
use crate::{Environment, EvalConfig, Evaluator};
use ariadne::Source;

pub fn error_demo() {
    print_error(
        "Identifier not found with typo suggestions",
        r#"
        let items = ["a", "b", "c", "d"];
        let item_index = 3;
        
        // Accessing an undefined variable here
        let item = items[item_inedx];
    "#,
    );

    print_error(
        "Type error",
        r#"
        // Adding a string to an integer
        let num = "123" + 123;
    "#,
    );

    print_error(
        "Assertion failed",
        r#"
        let num = 123;
        assert(num == 124);
    "#,
    );

    print_error(
        "Assertion failed",
        r#"
        let num = 123;
        assert_eq(num, 124);
    "#,
    );

    print_error(
        "Attempting to iterate a non-iterable type",
        r#"
        let num = 123;
        for item in num {
            println(item);
        }
    "#,
    );

    print_error(
        "Iterator method on iterable",
        r#"
        let arr = [1, 2, 3];
        for item in arr.map(fn(x) {x * 2}) {
            println(item);
        }
    "#,
    );

    print_error(
        "Index out of bounds",
        r#"
        let arr = [1, 2, 3];
        let item = arr[3];
    "#,
    );

    print_error(
        "Indexing with a non indexing type",
        r#"
        let arr = [1, 2, 3];
        let item = arr["a"];
    "#,
    );

    print_error(
        "Indexing non-indexable type",
        r#"
        let num = 123;
        let item = num[3];
        "#,
    );
    
    print_error(
        "Invalid start of expression",
        r#"
        let fun = |x| x + 1;
        "#,
    );
    
    print_error(
        "Unexpected end of expression list (with related error)",
        r#"
        let doubled_ints = (0..).iter().map(|x| x * 2);
        "#,
    );
    
    print_error(
        "Invalid struct key",
        r#"
        let obj = struct { 1: 1, b: 2 };
        let item = obj["c"];
        "#,
    );
}

pub fn print_error(desc: &str, input: &str) {
    let mut env = Environment::new();
    let res = Evaluator::eval(input, &mut env, EvalConfig::default());

    assert!(res.is_err(), "Expected this to eval an error: {input}");

    if let Err(e) = res {
        e.as_pretty_errors(desc)
            .eprint((desc, Source::from(input)))
            .expect("Expected to be able to print error");
    }
}
