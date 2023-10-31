/// Runs the passed in `Mechylang` code and panics if it fails.
/// If code fails to run, the error is printed to stdout.
///
/// Works best in combination with `assert` statements in the `Mechylang` code.
///
/// # Examples
///
/// ```
/// use mechylang::test_utils::test_eval_ok;
/// test_eval_ok("assert(1 == 1);");
/// ```
///
/// ```should_panic
/// use mechylang::test_utils::test_eval_ok;
/// test_eval_ok("assert(1 == 2);");
/// ```
///
/// # Panics
/// Panics if the code fails to run.
pub fn test_eval_ok(code: &str) {
    let result = crate::Evaluator::eval(code, &mut Default::default(), Default::default());

    if let Err(ref err) = result {
        for e in err.iter() {
            println!("{}", e);
        }
    }

    assert!(result.is_ok());
}
