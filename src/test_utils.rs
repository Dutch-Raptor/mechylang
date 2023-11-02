use crate::errors::ErrorKind;

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

pub fn test_eval_err(code: &str, error_kinds: &[ErrorKind]) {
    let result = crate::Evaluator::eval(code, &mut Default::default(), Default::default());

    if let Ok(_) = result {
        panic!("Expected error, got Ok");
    }

    if let Err(ref errors) = result {
        println!("Errors: {:?}", errors);
        for e in errors.iter() {
            println!("{}", e);
        }
        assert_eq!(errors.len(), error_kinds.len());
        assert_eq!(errors.iter().all(|e| error_kinds.contains(&e.kind)), true);
    }
}
