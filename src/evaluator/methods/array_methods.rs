//! # Methods for the `Object::Array` type
//!
//! For info on the `Object::Array` type, see [the documentation for the `Object` enum](crate::Object#variant.Array).
use crate::Object;

use super::{get_mutable_ident, MethodInner};

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
pub const ARRAY_PUSH: MethodInner = MethodInner {
    name: "push",
    args_len: 1..=1,
    function: |_, ident, args, env, _| {
        let value = args[0].clone();

        let ident = get_mutable_ident(ident)?;
        
        let value_id = env.store_object(value);

        env.mutate(ident.to_string(), move |arr| {
            if let Object::Array(ref mut arr) = arr {
                arr.push(value_id);
                Ok(Object::Unit)
            } else {
                Err(format!("Expected array, got {}", arr))
            }
        })
        .map_err(|e| e.to_string())?;
        Ok(Object::Unit)
    },
};

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
pub const ARRAY_POP: MethodInner = MethodInner {
    name: "pop",
    args_len: 0..=0,
    function: |_, ident, _, env, _| {
        let ident = get_mutable_ident(ident)?;

        let item = env
            .mutate(ident.to_string(), move |arr| {
                if let Object::Array(ref mut arr) = arr {
                    let obj_id = arr.pop();
                    
                    match obj_id {
                        Some(id) => Ok(Object::Reference(id)),
                        None => Ok(Object::Unit),
                    }
                } else {
                    Err(format!("Expected array, got {}", arr))
                }
            })
            .map_err(|e| e.to_string())?;
        
        if let Object::Reference(id) = item {
            return Ok(env.get_by_id(id).expect("objectId in array should be valid"));
        } 
        
        Ok(item)
    },
};

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
pub const ARRAY_FIRST: MethodInner = MethodInner {
    name: "first",
    args_len: 0..=0,
    function: |_, ident, _, env, _| {
        let ident = get_mutable_ident(ident)?;

        let item = env
            .mutate(ident.to_string(), move |arr| {
                if let Object::Array(ref mut arr) = arr {
                        match arr.first() {
                            Some(id) => Ok(Object::Reference(*id)),
                            None => Ok(Object::Unit),
                        }
                } else {
                    Err(format!("Expected array, got {}", arr))
                }
            })
            .map_err(|e| e.to_string())?;
        
        if let Object::Reference(id) = item {
            return Ok(env.get_by_id(id).expect("objectId in array should be valid"));
        }
        
        Ok(item)
    },
};

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
pub const ARRAY_LAST: MethodInner = MethodInner {
    name: "last",
    args_len: 0..=0,
    function: |_, ident, _, env, _| {
        let ident = get_mutable_ident(ident)?;

        let item = env
            .mutate(ident.to_string(), move |arr| {
                if let Object::Array(ref mut arr) = arr {
                        match arr.last() {
                            Some(id) => Ok(Object::Reference(*id)),
                            None => Ok(Object::Unit),
                        }
                } else {
                    Err(format!("Expected array, got {}", arr))
                }
            })
            .map_err(|e| e.to_string())?;
        
        if let Object::Reference(id) = item {
            return Ok(env.get_by_id(id).expect("objectId in array should be valid"));
        }
        
        Ok(item)
    },
};

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
pub const ARRAY_LEN: MethodInner = MethodInner {
    name: "len",
    args_len: 0..=0,
    function: |obj, _, _, _, _| match obj {
        Object::Array(ref arr) => Ok(Object::Integer(arr.len() as i64)),
        _ => Err("Argument to `len` not supported".to_string()),
    },
};

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
pub const ARRAY_INSERT: MethodInner = MethodInner {
    name: "insert",
    args_len: 2..=2,
    function: |obj, ident, args, env, _| {
        let index = match args[0] {
            Object::Integer(i) => i,
            _ => return Err("Expected integer for index".to_string()),
        };

        let value = args[1].clone();

        let ident = get_mutable_ident(ident)?;

        match obj {
            Object::Array(arr) => {
                if index < 0 || index as usize > arr.len() {
                    return Err(format!(
                        "Index {} out of bounds for array of length {}",
                        index,
                        arr.len()
                    ));
                }
            }
            _ => return Err("Expected array".to_string()),
        }
        
        let value_id = env.store_object(value);

        env.mutate(ident.to_string(), move |arr| {
            if let Object::Array(ref mut arr) = arr {
                arr.insert(index as usize, value_id);
                Ok(Object::Unit)
            } else {
                Err(format!("Expected array, got {}", arr))
            }
        })?;
        Ok(Object::Unit)
    },
};

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
pub const ARRAY_REMOVE: MethodInner = MethodInner {
    name: "remove",
    args_len: 1..=1,
    function: |obj, ident, args, env, _| {
        let index = match args[0] {
            Object::Integer(i) => i,
            _ => return Err("Expected integer for index".to_string()),
        };

        let ident = get_mutable_ident(ident)?;

        match obj {
            Object::Array(arr) => {
                if index < 0 || index as usize > arr.len() {
                    return Err(format!(
                        "Index {} out of bounds for array of length {}",
                        index,
                        arr.len()
                    ));
                }
            }
            _ => return Err("Expected array".to_string()),
        }

        let item = env
            .mutate(ident.to_string(), move |arr| {
                if let Object::Array(ref mut arr) = arr {
                    Ok(Object::Reference(arr.remove(index as usize)))
                } else {
                    Err(format!("Expected array, got {}", arr))
                }
            })
            .map_err(|e| e.to_string())?;
        
        if let Object::Reference(id) = item {
            return Ok(env.get_by_id(id).expect("objectId in array should be valid"));
        }
        
        Ok(item)
    },
};

/// ## `contains(item: Any) -> Boolean`
/// Returns true if the array contains the given item
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq([1, 2, 3].contains(1), true);
/// assert_eq([1, 2, 3].contains(4), false);
/// # "#);
pub const ARRAY_CONTAINS: MethodInner = MethodInner {
    name: "contains",
    args_len: 1..=1,
    function: |obj, _, args, env, _| {
        let value = args[0].clone();

        match obj {
            Object::Array(arr) => {
                for id in arr {
                    if env.get_by_id(id).expect("objectId in array should be valid") == value {
                        return Ok(Object::Boolean(true));
                    }
                }
                return Ok(Object::Boolean(false));
            }
            _ => Err("Argument to `contains` not supported".to_string()),
        }
    },
};

pub(crate) const ARRAY_METHODS: [MethodInner; 8] = [
    ARRAY_CONTAINS,
    ARRAY_REMOVE,
    ARRAY_PUSH,
    ARRAY_POP,
    ARRAY_FIRST,
    ARRAY_LAST,
    ARRAY_INSERT,
    ARRAY_LEN,
];

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
