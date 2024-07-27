//! Methods for range type objects.
//!
//! `Mechylang` has the following range types:
//! - `RangeFull` - `..`
//! - `RangeInclusive` - `a..=b`
//! - `Range` - `a..b`
//! - `RangeToInclusive` - `..=b`
//! - `RangeTo` - `..b`
//! - `RangeFrom` - `a..`

use crate::Object;

use super::MethodInner;

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
///
pub const RANGE_METHODS: [MethodInner; 1] = [MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, args, _, _| {
        if let Object::Range(start, end) = obj {
            let value = &args[0];
            contains(value, Some(&start), Some(&end), false)
        } else {
            Err(format!("Range method called on non-range object: {}", obj))
        }
    },
}];

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
pub const RANGE_INCLUSIVE_METHODS: [MethodInner; 1] = [MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, args, _, _| {
        if let Object::RangeInclusive(start, end) = obj {
            let value = &args[0];
            contains(value, Some(&start), Some(&end), true)
        } else {
            Err(format!(
                "RangeInclusive method called on non-range-inclusive object: {}",
                obj
            ))
        }
    },
}];

/// Methods for `RangeTo` objects.
///
/// # Contains
/// `contains` returns `true` if the range contains the given value.
/// The value must be of the same type as the range.
///
pub const RANGE_TO_METHODS: [MethodInner; 1] = [MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, args, _, _| {
        if let Object::RangeTo(end) = obj {
            let value = &args[0];
            contains(value, None, Some(&end), false)
        } else {
            Err(format!(
                "RangeTo method called on non-range-to object: {}",
                obj
            ))
        }
    },
}];

/// Methods for `RangeToInclusive` objects.
///
/// # Contains
/// `contains` returns `true` if the range contains the given value.
/// The value must be of the same type as the range.
pub const RANGE_TO_INCLUSIVE_METHODS: [MethodInner; 1] = [MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, args, _, _| {
        if let Object::RangeToInclusive(end) = obj {
            let value = &args[0];
            contains(value, None, Some(&end), true)
        } else {
            Err(format!(
                "RangeToInclusive method called on non-range-to-inclusive object: {}",
                obj
            ))
        }
    },
}];

/// Methods for `RangeFrom` objects.
///
pub const RANGE_FROM_METHODS: [MethodInner; 1] = [MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, args, _, _| {
        if let Object::RangeFrom(start) = obj {
            let value = &args[0];
            contains(value, Some(&start), None, false)
        } else {
            Err(format!(
                "RangeFrom method called on non-range-from object: {}",
                obj
            ))
        }
    },
}];

pub const RANGE_FULL_METHODS: [MethodInner; 1] = [MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, _, _, _| {
        if let Object::RangeFull = obj {
            // A RangeFull contains all values
            Ok(true.into())
        } else {
            Err(format!(
                "RangeFull method called on non-range-full object: {}",
                obj
            ))
        }
    },
}];

fn contains(
    value: &Object,
    start: Option<&Object>,
    end: Option<&Object>,
    end_inclusive: bool,
) -> Result<Object, String> {
    let type_err = Err(format!(
        "Contains method is not supported for value: {:?}, start: {:?}, end: {:?}",
        value, start, end
    ));
    if let Some(start) = start {
        let ord = match start.partial_cmp(value) {
            Some(ord) => ord,
            None => return type_err,
        };

        if ord == std::cmp::Ordering::Greater {
            return Ok(Object::Boolean(false));
        }
    }

    if let Some(end) = end {
        let ord = match end.partial_cmp(value) {
            Some(ord) => ord,
            None => return type_err,
        };

        if ord == std::cmp::Ordering::Less {
            return Ok(Object::Boolean(false));
        }

        if ord == std::cmp::Ordering::Equal && !end_inclusive {
            return Ok(Object::Boolean(false));
        }
    }

    Ok(Object::Boolean(true))
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
            Ok(Object::Boolean(false)),
        );

        // 1 in 0..
        assert_eq!(
            contains(&Object::Integer(1), Some(&Object::Integer(0)), None, false),
            Ok(Object::Boolean(true)),
        );

        // 1  in 1..
        assert_eq!(
            contains(&Object::Integer(1), Some(&Object::Integer(1)), None, false),
            Ok(Object::Boolean(true)),
        );

        // 1 not in ..0
        assert_eq!(
            contains(&Object::Integer(1), None, Some(&Object::Integer(0)), false),
            Ok(Object::Boolean(false)),
        );

        // 1 not in ..1
        assert_eq!(
            contains(&Object::Integer(1), None, Some(&Object::Integer(1)), false),
            Ok(Object::Boolean(false)),
        );

        // 1 in ..2
        assert_eq!(
            contains(&Object::Integer(1), None, Some(&Object::Integer(2)), false),
            Ok(Object::Boolean(true)),
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
