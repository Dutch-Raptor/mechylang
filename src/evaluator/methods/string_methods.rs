use std::sync::Arc;
use lazy_static::lazy_static;
use crate::evaluator::Error;
use crate::evaluator::objects::iterators::IteratorObject;
use crate::evaluator::objects::{ArgumentList, ArgumentType, FunctionTy, MethodTy, ObjectTy};
use crate::Object;

use super::MethodInner;

lazy_static! {
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
/// Returns an iterator of strings, each containing a single character from the original string.
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq("hello world".chars().collect(), ["h", "e", "l", "l", "o", " ", "w", "o", "r", "l", "d"]);
/// # "#);
/// ```
pub static ref STRING_METHODS: Arc<[MethodInner]> = Arc::new([
    MethodInner {
        method_ty: MethodTy {
            method_name: "contains",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "needle".into(), ty: ObjectTy::String }]),
                expected_return_type: Some(Box::new(ObjectTy::Boolean)),
            },
        },
        function: |args| {
            let haystack = args.obj.as_string().expect("Expected string method to be called on a string");

            let needle = args.args[0].as_string().ok_or_else(|| Error::TypeError {
                span: args.method_span.clone(),
                expected: vec![ObjectTy::String],
                found: args.args[0].get_type(),
            })?;

            Ok(Object::Boolean(haystack.contains(needle)))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "replace",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly( vec![
                    ArgumentType { name: "needle".into(), ty: ObjectTy::String },
                    ArgumentType { name: "replacement".into(), ty: ObjectTy::String },
                ]),
                expected_return_type: Some(Box::new(ObjectTy::String)),
            },
        },
        function: |args| {
            let haystack = args.obj.as_string().expect("Expected string method to be called on a string");

            let needle_arg = &args.args[0];
            let needle = needle_arg.as_string().ok_or_else(|| Error::TypeError {
                span: needle_arg.span.clone().unwrap_or(args.method_span.clone()),
                expected: vec![ObjectTy::String],
                found: args.args[0].get_type(),
            })?;

            let replacement_arg = &args.args[1];
            let replacement = replacement_arg.as_string().ok_or_else(|| Error::TypeError {
                span: replacement_arg.span.clone().unwrap_or(args.method_span.clone()),
                expected: vec![ObjectTy::String],
                found: args.args[1].get_type(),
            })?;

            Ok(Object::String(haystack.replace(needle, replacement).into()))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "len",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Integer)),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");
            Ok(Object::Integer(s.len() as i64))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "split",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "delimiter".into(), ty: ObjectTy::String }]),
                expected_return_type: Some(Box::new(ObjectTy::Array { expected_item_types: None })),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");

            let delimiter = args.args[0].as_string().ok_or_else(|| Error::TypeError {
                span: args.args[0].span.clone().unwrap_or(args.method_span.clone()),
                expected: vec![ObjectTy::String],
                found: args.args[0].get_type(),
            })?;

            let split: Vec<_> = s.split(delimiter).map(|s| s.into()).collect();

            Ok(Object::Array(split))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "trim",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::String)),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");
            Ok(Object::String(s.trim().into()))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "to_uppercase",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::String)),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");

            Ok(Object::String(s.to_uppercase().into()))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "to_lowercase",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::String)),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");

            Ok(Object::String(s.to_lowercase().into()))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "chars",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::String) })),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");

            Ok(Object::Iterator(IteratorObject {
                iterator: Box::new(
                    s.chars()
                        .map(|c| Object::String(c.to_string().into()))
                        .collect::<Vec<Object>>()
                        .into_iter(),
                ),
            }))
        },
    },
    MethodInner {
        method_ty: MethodTy {
            method_name: "lines",
            self_ty: Box::new(ObjectTy::String),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::String) })),
            },
        },
        function: |args| {
            let s = args.obj.as_string().expect("Expected string method to be called on a string");

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
]);
}