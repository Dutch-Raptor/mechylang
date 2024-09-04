//! Methods for range type objects.
//!
//! `Mechylang` has the following range types:
//! - `RangeFull` - `..`
//! - `RangeInclusive` - `a..=b`
//! - `Range` - `a..b`
//! - `RangeToInclusive` - `..=b`
//! - `RangeTo` - `..b`
//! - `RangeFrom` - `a..`

use std::sync::Arc;
use lazy_static::lazy_static;
use crate::evaluator::objects::{ArgumentList, ArgumentType, FunctionTy, MethodTy, ObjectTy};
use crate::Object;
use crate::evaluator::{Error, };
use crate::evaluator::runtime::builtins::BuiltinError;
use super::MethodInner;

lazy_static! {
    /// Methods for `Range` objects.
    ///
    /// # Contains
    ///
    /// `contains` returns `true` if the range contains the given value.
    /// The value must be of the same type as the range.
    /// 
    /// # Examples
    ///
    ///
    /// ```
    /// use mechylang::test_utils::test_eval_ok;
    /// test_eval_ok(r#"
    /// let range = 1..3;
    /// assert_eq(range.contains(2), true);
    /// assert_eq(range.contains(4), false);
    /// assert_eq(range.contains(3), false);
    /// assert_eq(range, 1..3);
    /// "#);
    /// ```
    pub static ref RANGE_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "contains",
                self_ty: Box::new(ObjectTy::Range { from: Box::new(ObjectTy::Any), to: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "value".into(), ty: ObjectTy::Any }]),
                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                },
            },
            function: |args| {
                let (start, end) = args.obj.as_range().expect("Expected range method to be called on a range");

                let value = &args.args[0];
                match contains(&value.value, Some(&start), Some(&end), false) {
                    Some(result) => Ok(result.into()),
                    None => Err(Error::BuiltInError {
                        span: args.method_span.clone(),
                        message: "Cannot check if value is in range".into(),
                        error_type: BuiltinError::RangeError,
                    }.into()),
                }
            },
        }]);
}

lazy_static! {
    /// Methods for `RangeInclusive` objects.
    ///
    /// # Contains
    /// `contains` returns `true` if the range contains the given value.
    /// The value must be of the same type as the range.
    ///
    /// # Examples
    ///
    /// ```
    /// use mechylang::test_utils::test_eval_ok;
    /// test_eval_ok(r#"
    /// let range = 1..=3;
    /// assert_eq(range.contains(2), true);
    /// assert_eq(range.contains(4), false);
    /// assert_eq(range.contains(3), true);
    /// "#);
    /// ```
    pub static ref RANGE_INCLUSIVE_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                
                method_name: "contains",
                self_ty: Box::new(ObjectTy::RangeInclusive { from: Box::new(ObjectTy::Any), to: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "value".into(), ty: ObjectTy::Any }]),
                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                },
            },
            function: |args| {
                let (start, end) = args.obj.as_range_inclusive().expect("Expected range-inclusive method to be called on a range-inclusive");

                let value = &args.args[0];

                match contains(&value.value, Some(&start), Some(&end), true) {
                    Some(result) => Ok(result.into()),
                    None => Err(Error::BuiltInError {
                        span: args.method_span.clone(),
                        message: "Cannot check if value is in range".into(),
                        error_type: BuiltinError::RangeError,
                    }.into()),
                }
            },
        }
    ]);
}

lazy_static! {
    /// Methods for `RangeTo` objects.
    ///
    /// # Contains
    /// `contains` returns `true` if the range contains the given value.
    /// The value must be of the same type as the range.
    pub static ref RANGE_TO_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "contains",
                self_ty: Box::new(ObjectTy::RangeTo { to: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "value".into(), ty: ObjectTy::Any }]),
                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                },
            },
            function: |args| {
                let end = args.obj.as_range_to().expect("Expected range-to method to be called on a range-to");

                let value = &args.args[0];
                match contains(value, None, Some(&end), false) {
                    Some(result) => Ok(result.into()),
                    None => Err(Error::BuiltInError {
                        span: args.method_span.clone(),
                        message: "Cannot check if value is in range".into(),
                        error_type: BuiltinError::RangeError,
                    }.into()),
                }
            },
        }
    ]);
}

lazy_static! {
    /// Methods for `RangeToInclusive` objects.
    ///
    /// # Contains
    /// `contains` returns `true` if the range contains the given value.
    /// The value must be of the same type as the range.
    pub static ref RANGE_TO_INCLUSIVE_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "contains",
                self_ty: Box::new(ObjectTy::RangeToInclusive { to: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "value".into(), ty: ObjectTy::Any }]),
                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                },
            },
            function: |args| {
                let end = args.obj.as_range_to_inclusive().expect("Expected range-to-inclusive method to be called on a range-to-inclusive");
                
                let value = &args.args[0];
                match contains(value, None, Some(&end), true) {
                    Some(result) => Ok(result.into()),
                    None => Err(Error::BuiltInError {
                        span: args.method_span.clone(),
                        message: "Cannot check if value is in range".into(),
                        error_type: BuiltinError::RangeError,
                    }.into()),
                }
            },
        },
    ]);
}

lazy_static! {
    /// Methods for `RangeFrom` objects.
    pub static ref RANGE_FROM_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "contains",
                self_ty: Box::new(ObjectTy::RangeFrom { from: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "value".into(), ty: ObjectTy::Any }]),
                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                },
            },
            function: |args| {
                let start = args.obj.as_range_from().expect("Expected range-from method to be called on a range-from");
                
                let value = &args.args[0];
                match contains(value, Some(&start), None, false) {
                    Some(result) => Ok(result.into()),
                    None => Err(Error::BuiltInError {
                        span: args.method_span.clone(),
                        message: "Cannot check if value is in range".into(),
                        error_type: BuiltinError::RangeError,
                    }.into()),
                }
            },
        }
    ]);
}

lazy_static! {
    /// Methods for `RangeFull` objects.
    ///
    /// # Contains
    /// `contains` returns `true` if the range contains the given value.
    /// The value must be of the same type as the range.
    pub static ref RANGE_FULL_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "contains",
                self_ty: Box::new(ObjectTy::RangeFull),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                },
            },
            function: |args| {
                match args.obj {
                    Object::RangeFull => Ok(true.into()),
                    _ => panic!("RangeFull method called on non-range-full object: {}", args.obj),
                }
            },
        }
    ]);
}

fn contains(
    value: &Object,
    start: Option<&Object>,
    end: Option<&Object>,
    end_inclusive: bool,
) -> Option<bool> {
    if let Some(start) = start {
        let ord = match start.partial_cmp(value) {
            Some(ord) => ord,
            None => return None,
        };

        if ord == std::cmp::Ordering::Greater {
            return Some(false);
        }
    }

    if let Some(end) = end {
        let ord = match end.partial_cmp(value) {
            Some(ord) => ord,
            None => return None,
        };

        if ord == std::cmp::Ordering::Less {
            return Some(false);
        }

        if ord == std::cmp::Ordering::Equal && !end_inclusive {
            return Some(false);
        }
    }

    Some(true)
}
#[cfg(test)]
mod test {
    use crate::{test_utils::test_eval_ok, Object};

    use super::contains;

    #[test]
    fn test_contains() {
        // 1 not in 2..
        assert_eq!(
            contains(&Object::Integer(1), Some(&Object::Integer(2)), None, false),
            Some(false),
        );

        // 1 in 0..
        assert_eq!(
            contains(&Object::Integer(1), Some(&Object::Integer(0)), None, false),
            Some(true),
        );

        // 1  in 1..
        assert_eq!(
            contains(&Object::Integer(1), Some(&Object::Integer(1)), None, false),
            Some(true),
        );

        // 1 not in ..0
        assert_eq!(
            contains(&Object::Integer(1), None, Some(&Object::Integer(0)), false),
            Some(false),
        );

        // 1 not in ..1
        assert_eq!(
            contains(&Object::Integer(1), None, Some(&Object::Integer(1)), false),
            Some(false),
        );

        // 1 in ..2
        assert_eq!(
            contains(&Object::Integer(1), None, Some(&Object::Integer(2)), false),
            Some(true),
        );
    }

    #[test]
    fn test_range_contains() {
        test_eval_ok(
            r#"
            let range = 1..3;
            assert_eq(range.contains(2), true);
            assert_eq(range.contains(4), false);
            assert_eq(range.contains(3), false);
            assert_eq(range, 1..3);
            "#,
        );
    }
}
