use crate::Object;

/// Trait for unwrapping return values
///
/// If the object is a return value, it will unwrap it and return the inner value.
///
/// # Examples
/// ```ignore
/// use crate::evaluator::objects::{Object, UnwrapReturnValue};
///
/// let obj = Object::ReturnValue(Box::new(Object::Integer(5)));
/// assert_eq!(obj.unwrap_return_value(), Object::Integer(5));
///
/// let obj = Object::Integer(5);
/// assert_eq!(obj.unwrap_return_value(), Object::Integer(5));
/// ```
pub trait UnwrapReturnValue {
    fn unwrap_return_value(self) -> Object;
}

impl UnwrapReturnValue for Object {
    fn unwrap_return_value(self) -> Object {
        match self {
            Object::ReturnValue(val) => *val,
            _ => self,
        }
    }
}
