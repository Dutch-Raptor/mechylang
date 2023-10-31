use crate::Object;

use super::MethodInner;

/// # Methods for String
///
/// ## `contains(needle: String) -> Boolean`
/// Returns true if the string contains the given substring.
/// ```
/// # use mechylang::test_utils::test_eval_ok;
/// # test_eval_ok(r#"
/// assert_eq("hello world".contains("hello"), true);
/// assert_eq("hello world".contains("goodbye"), false);
/// # "#);
/// ```
///
/// ## `replace(needle: String, replacement: String) -> String`
/// Returns a new string with all instances of the needle replaced with the replacement.
///
/// ```
/// # use mechylang::test_utils::test_eval_ok;
/// # test_eval_ok(r#"
/// assert_eq("hello world".replace("hello", "goodbye"), "goodbye world");
/// # "#);
pub const STRING_METHODS: [MethodInner; 2] = [
    MethodInner {
        name: "contains",
        args_len: 1..=1,
        function: |obj, _, args, _, _| {
            let haystack = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            let needle = match &args[0] {
                Object::String(s) => s.as_ref(),
                _ => return Err("Contains method called with non-string argument".to_string()),
            };

            Ok(Object::Boolean(haystack.contains(needle)))
        },
    },
    MethodInner {
        name: "replace",
        args_len: 2..=2,
        function: |obj, _, args, _, _| {
            let haystack = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            let needle = match &args[0] {
                Object::String(s) => s.as_ref(),
                _ => return Err("Replace method called with non-string argument".to_string()),
            };

            let replacement = match &args[1] {
                Object::String(s) => s.as_ref(),
                _ => return Err("Replace method called with non-string argument".to_string()),
            };

            Ok(Object::String(haystack.replace(needle, replacement).into()))
        },
    },
];

fn invalid_object_err() -> Result<Object, String> {
    Err("String method called on non-string object".to_string())
}
