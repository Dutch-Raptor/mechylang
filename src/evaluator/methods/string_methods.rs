use crate::evaluator::objects::iterators::IteratorObject;
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
/// ```
///
/// ## `len() -> Integer`
///
/// Returns the length of the string.
///
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("hello world".len(), 11);
/// # "#);
/// ```
///
/// ## `split(delimiter: String) -> Array`
/// Returns an array of strings split by the delimiter.
///
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("hello world".split(" "), ["hello", "world"]);
/// # "#);
/// ```
///
/// ## `trim() -> String`
/// Returns a new string with leading and trailing whitespace removed.
///
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("  hello world  ".trim(), "hello world");
/// # "#);
/// ```
///
/// ## `to_uppercase() -> String`
/// Returns a new string with all characters converted to uppercase.
///
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("hello world".to_uppercase(), "HELLO WORLD");
/// # "#);
/// ```
///
/// ## `to_lowercase() -> String`
/// Returns a new string with all characters converted to lowercase.
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("HELLO WORLD".to_lowercase(), "hello world");
/// # "#);
/// ```
///
/// ## `chars() -> Array
/// Returns an array of strings, each containing a single character from the original string.
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("hello world".chars(), ["h", "e", "l", "l", "o", " ", "w", "o", "r", "l", "d"]);
/// # "#);
/// ```
pub const STRING_METHODS: [MethodInner; 9] = [
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
    MethodInner {
        name: "len",
        args_len: 0..=0,
        function: |obj, _, _, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            Ok(Object::Integer(s.len() as i64))
        },
    },
    MethodInner {
        name: "split",
        args_len: 1..=1,
        function: |obj, _, args, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            let delimiter = match &args[0] {
                Object::String(s) => s.as_ref(),
                _ => return Err("Split method called with non-string argument".to_string()),
            };

            let split: Vec<_> = s.split(delimiter).map(|s| s.into()).collect();

            Ok(Object::Array(split))
        },
    },
    MethodInner {
        name: "trim",
        args_len: 0..=0,
        function: |obj, _, _, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            Ok(Object::String(s.trim().into()))
        },
    },
    MethodInner {
        name: "to_uppercase",
        args_len: 0..=0,
        function: |obj, _, _, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            Ok(Object::String(s.to_uppercase().into()))
        },
    },
    MethodInner {
        name: "to_lowercase",
        args_len: 0..=0,
        function: |obj, _, _, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            Ok(Object::String(s.to_lowercase().into()))
        },
    },
    MethodInner {
        name: "chars",
        args_len: 0..=0,
        function: |obj, _, _, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            Ok(Object::Array(
                s.chars()
                    .map(|c| Object::String(c.to_string().into()))
                    .collect(),
            ))
        },
    },
    MethodInner {
        name: "lines",
        args_len: 0..=0,
        function: |obj, _, _, _, _| {
            let s = match obj {
                Object::String(s) => s,
                _ => return invalid_object_err(),
            };

            Ok(Object::Iterator(IteratorObject {
                iterator: Box::new(
                    s.lines()
                        .map(Object::from)
                        .collect::<Vec<Object>>()
                        .into_iter(),
                ),
            }))
        },
    },
];

fn invalid_object_err() -> Result<Object, String> {
    Err("String method called on non-string object".to_string())
}
