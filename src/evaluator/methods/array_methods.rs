//! # Methods for the `Object::Array` type
//!
//! For info on the `Object::Array` type, see [the documentation for the `Object` enum](Object#variant.Array).

use std::sync::Arc;
use lazy_static::lazy_static;
use crate::evaluator::Error;
use crate::evaluator::objects::{ArgumentList, ArgumentType, FunctionTy, MethodTy, ObjectTy};
use crate::Object;

use super::{MethodInner};


lazy_static! {
/// ## `push(item: Any) -> Null`
/// Pushes an item to the end of the array
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// arr.push(4);
/// assert_eq(arr, [1, 2, 3, 4]);
/// # "#);
/// ```
/// ## `pop() -> Any`
/// Removes the last item from the array and returns it
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// assert_eq(arr.pop(), 3);
/// assert_eq(arr, [1, 2]);
/// # "#);
/// ```
/// ## `first() -> Any`
/// Returns the first item in the array
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// assert_eq(arr.first(), 1);
/// assert_eq(arr, [1, 2, 3]);
/// # "#);
/// ```
/// ## `last() -> Any`
///
/// Returns the last item in the array
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// assert_eq(arr.last(), 3);
/// assert_eq(arr, [1, 2, 3]);
/// # "#);
/// ```
/// ## `len() -> Integer`
///
/// Returns the length of the array
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// assert_eq(arr.len(), 3);
/// assert_eq(arr, [1, 2, 3]);
/// # "#);
/// ```
/// ## `insert(index: Integer, item: Any) -> Null`
///
/// Inserts an item at the given index, shifting all items after it to the right
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// arr.insert(1, 4);
/// assert_eq(arr, [1, 4, 2, 3]);
/// # "#);
/// ```
/// ## `remove(index: Integer) -> Any`
///
/// Removes an item at the given index, shifting all items after it to the left
/// and returns the removed item
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// let arr = [1, 2, 3];
/// assert_eq(arr.remove(1), 2);
/// assert_eq(arr, [1, 3]);
/// # "#);
/// ```
/// ## `contains(item: Any) -> Boolean`
/// Returns true if the array contains the given item
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq([1, 2, 3].contains(1), true);
/// assert_eq([1, 2, 3].contains(4), false);
/// # "#);
    pub static ref ARRAY_METHODS: Arc<[MethodInner]> = Arc::new([
    MethodInner {
        method_ty: MethodTy {
            method_name: "push",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                expected_return_type: Some(Box::new(ObjectTy::Unit)),
            },
        },

        function: |args| {
            let arg = args.args[0].clone();

            if let Some(ident) = args.obj_identifier {
                return args.env.mutate(ident.to_string(), move |arr| {
                    if let Object::Array(ref mut arr) = arr {
                        arr.push(arg.value.clone());
                        Ok(Object::Unit)
                    } else {
                        Err(Error::TypeError {
                            span: args.method_span.clone(),
                            expected: vec![ObjectTy::Array { expected_item_types: None }],
                            found: arr.get_type(),
                        }.into())
                    }
                });
            };

            Ok(Object::Unit)
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "contains",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                expected_return_type: Some(Box::new(ObjectTy::Boolean)),
            },
        },
        function: |args| {
            let value = args.args[0].clone();

            let arr = args.obj.as_array().expect("Expected array method to be called on an array");
            Ok(Object::Boolean(arr.contains(&value.value)))
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "remove",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "index".into(), ty: ObjectTy::Integer }]),
                expected_return_type: Some(Box::new(ObjectTy::Any)),
            },
        },
        function: |args| {
            let index = args.args.first().and_then(|arg| arg.as_integer()).ok_or_else(|| Error::TypeError {
                span: args.method_span.clone(),
                expected: vec![ObjectTy::Integer],
                found: args.args[0].get_type(),
            })?;

            if let Some(ident) = args.obj_identifier {
                args.env.mutate(ident.to_string(), move |arr| {
                    if let Object::Array(ref mut arr) = arr {
                        Ok(arr.remove(index as usize))
                    } else {
                        Err(Error::TypeError {
                            span: args.method_span.clone(),
                            expected: vec![ObjectTy::Array { expected_item_types: None }],
                            found: arr.get_type(),
                        }.into())
                    }
                })
            } else {
                match args.obj {
                    Object::Array(ref arr) => Ok(arr.get(index as usize).cloned().unwrap_or(Object::Unit)),
                    _ => Err(Error::TypeError {
                        span: args.method_span.clone(),
                        expected: vec![ObjectTy::Array { expected_item_types: None }],
                        found: args.obj.get_type(),
                    }.into()),
                }
            }
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "insert",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::new_bounded(2..=2, vec![
                    ArgumentType { name: "index".into(), ty: ObjectTy::Integer },
                    ArgumentType { name: "item".into(), ty: ObjectTy::Any },
                ]),
                expected_return_type: Some(Box::new(ObjectTy::Unit)),
            },
        },
        function: |args| {
            let ident = match args.obj_identifier {
                Some(ident) => ident,
                None => return Ok(Object::Unit), // No identifier, so no need to mutate the array as it will be dropped
            };

            let index = args.args.first().and_then(|arg| arg.as_integer()).ok_or_else(|| Error::TypeError {
                span: args.method_span.clone(),
                expected: vec![ObjectTy::Integer],
                found: args.args[0].get_type(),
            })?;

            let value = args.args[1].clone();

            args.env.mutate(ident.to_string(), move |arr| {
                if let Object::Array(ref mut arr) = arr {
                    arr.insert(index as usize, value.value.clone());
                    Ok(Object::Unit)
                } else {
                    Err(Error::TypeError {
                        span: args.method_span.clone(),
                        expected: vec![ObjectTy::Array { expected_item_types: None }],
                        found: arr.get_type(),
                    }.into())
                }
            })
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "len",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Integer)),
            },
        },
        function: |args| {
            let arr = args.obj.as_array().expect("Expected array method to be called on an array");
            Ok(Object::Integer(arr.len() as i64))
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "last",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Any)),
            },
        },
        function: |args| {
            let arr = args.obj.as_array().expect("Expected array method to be called on an array");
            Ok(arr.last().cloned().unwrap_or(Object::Unit))
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "first",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Any)),
            },
        },
        function: |args| {
            let arr = args.obj.as_array().expect("Expected array method to be called on an array");
            Ok(arr.first().cloned().unwrap_or(Object::Unit))
        },
    }, MethodInner {
        method_ty: MethodTy {
            method_name: "pop",
            self_ty: Box::new(ObjectTy::Array { expected_item_types: None }),
            function_ty: FunctionTy {
                arguments: ArgumentList::None,
                expected_return_type: Some(Box::new(ObjectTy::Any)),
            },
        },
        function: |args| {
            if let Some(ident) = args.obj_identifier {
                // If there is an identifier, mutate the array
                args.env.mutate(ident.to_string(), move |obj| {
                    if let Object::Array(ref mut arr) = obj {
                        Ok(arr.pop().unwrap_or(Object::Unit))
                    } else {
                        Err(Error::TypeError {
                            span: args.method_span.clone(),
                            expected: vec![ObjectTy::Array { expected_item_types: None }],
                            found: obj.get_type(),
                        }.into())
                    }
                })
            } else {
                // If there is no identifier, return the last item in the array
                match args.obj {
                    Object::Array(ref arr) => Ok(arr.last().cloned().unwrap_or(Object::Unit)),
                    _ => Err(Error::TypeError {
                        span: args.method_span.clone(),
                        expected: vec![ObjectTy::Array { expected_item_types: None }],
                        found: args.obj.get_type(),
                    }.into()),
                }
            }
        },
    }
]);
}

#[cfg(test)]
mod test {
    use crate::test_utils::test_eval_ok;

    #[test]
    fn test_array_contains() {
        test_eval_ok(
            r#"
            let arr = [1, 2, 3];
            assert_eq(arr.contains(2), true);
            assert_eq(arr.contains(4), false);
            assert_eq(arr, [1, 2, 3]);
            "#,
        );
    }
}
