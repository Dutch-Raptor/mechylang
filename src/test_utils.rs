use crate::error::ErrorKind;
use crate::Lexer;

/// Runs the passed in `Mechylang` code and panics if it fails.
/// If code fails to run, the error is printed to stdout.
///
/// Works best in combination with `assert` statements in the `Mechylang` code.
///
/// # Examples
///
/// This will not panic because the assertion passes:
/// ```
/// use mechylang::test_utils::test_eval_ok;
/// test_eval_ok("assert(1 == 1);");
/// ```
///
/// This will panic because the assertion fails:
/// ```should_panic
/// use mechylang::test_utils::test_eval_ok;
/// test_eval_ok("assert(1 == 2);");
/// ```
///
/// # Panics
/// - if the code fails to run.
pub fn test_eval_ok(code: &str) {
    let result = crate::Evaluator::eval(code, &mut Default::default(), Default::default());

    if let Err(ref err) = result {
        for e in err.iter() {
            println!("{}", e);
        }
    }

    assert!(result.is_ok());
}

/// Parses the given Mechylang code and asserts that it is valid.
/// 
/// This function is intended to be used in tests to verify that the provided Mechylang code
/// can be successfully parsed without errors. If the code fails to parse, the function
/// prints the encountered errors to stdout and panics.
/// 
/// # Arguments
/// 
/// * `code` - A string slice containing the Mechylang code to be parsed.
/// 
/// # Panics
/// 
/// Panics if the code fails to parse. The parsing errors, if any, are printed to stdout.
/// 
/// # Examples
/// 
/// ```
/// # use mechylang::test_utils::test_parse_ok;
/// # test_parse_ok(r#"
/// let x = 5;
/// assert_eq(x, 5);
/// # "#);
/// ```
/// 
/// The above example verifies that the provided Mechylang code is syntactically valid.
pub fn test_parse_ok(code :&str) {
    let result = crate::Parser::new(Lexer::new(code)).parse();

    if let Err(ref err) = result {
        for e in err.iter() {
            println!("{}", e);
        }
    }

    assert!(result.is_ok());
}

/// Tests that evaluating a given piece of Mechylang code results in specific errors.
///
/// This function is used to verify that a piece of Mechylang code produces expected errors when evaluated.
/// It runs the code using the `Evaluator` and checks that the errors match the expected kinds.
///
/// # Arguments
///
/// * `code` - A string slice containing the Mechylang code to be evaluated.
/// * `error_kinds` - A slice of `ErrorKind` values representing the expected kinds of errors.
///
/// # Panics
///
/// The function will panic if:
/// - The code does not produce any errors (i.e., evaluation succeeds).
/// - The number of errors produced does not match the number of expected error kinds.
/// - The kinds of errors produced do not match the expected kinds.
pub fn test_eval_err(code: &str, error_kinds: &[ErrorKind]) {
    let result = crate::Evaluator::eval(code, &mut Default::default(), Default::default());

    if result.is_ok() {
        panic!("Expected error, got Ok");
    }

    if let Err(ref errors) = result {
        for e in errors.iter() {
            println!("{}", e);
        }
        assert_eq!(errors.len(), error_kinds.len());
        assert!(errors.iter().all(|e| error_kinds.contains(&e.kind)));
    }
}
