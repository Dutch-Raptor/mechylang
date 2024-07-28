pub(crate) mod function;
pub(crate) mod iterators;
pub(crate) mod reference;
pub(crate) mod traits;

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use itertools::Itertools;
use crate::evaluator::objects::function::Callable;
use self::{function::Function, iterators::IteratorObject, reference::Reference};

use super::{methods::Method, runtime::builtins::BuiltinFunction};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    /// # Integer
    ///
    /// An integer is a whole number. It can be positive or negative.
    ///
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// let x = 5;
    /// assert_eq(x, 5);
    /// # "#);
    /// ```
    /// ## Max and Min
    /// Integers have a maximum and minimum value. If you try to assign a value
    /// that is too large or too small, you will get an error.
    ///
    /// Max: 2<sup>63</sup> - 1 = 9,223,372,036,854,775,807
    /// Min: -2<sup>63</sup> = -9,223,372,036,854,775,808
    Integer(i64),
    /// # Float
    ///
    /// A float is a decimal number. It can be positive or negative.
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// let x = 5.5;
    /// assert_eq(x, 5.5);
    /// # "#);
    /// ```
    /// ## Max and Min
    /// Floats have a maximum and minimum value. If you try to assign a value
    /// that is too large or too small, you will get an error.
    ///
    /// Max: 1.7976931348623157<sup>308</sup>
    /// Min: 2.2250738585072014<sup>-308</sup>
    Float(f64),
    Boolean(bool),
    Unit,
    ReturnValue(Box<Object>),
    Function(Function),
    String(Rc<str>),
    BuiltinFunction(BuiltinFunction),
    /// # Array
    /// An array is a mutable, ordered list of items. It can contain any type of object.
    ///
    /// ## Usage
    ///
    /// To create an array, use the following syntax:
    ///
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// let arr = [1, 2, 3];
    /// assert_eq(arr, [1, 2, 3]);
    /// # "#);
    /// ```
    ///
    /// ## Indexing
    /// To get an item from an array, use the following syntax:
    ///
    /// ```rust
    /// # mechylang::test_utils::test_eval_ok(r#"
    /// let arr = [1, 2, 3];
    /// assert_eq(arr[0], 1);
    /// assert_eq(arr[1], 2);
    /// assert_eq(arr[2], 3);
    /// # "#);
    /// ```
    ///
    /// ## Methods
    ///
    /// Arrays have many methods, take a look at the [array methods module](crate::evaluator::methods::array_methods) for more information.
    Array(Vec<Object>),
    RangeFrom(Rc<Object>),
    RangeTo(Rc<Object>),
    RangeToInclusive(Rc<Object>),
    Range(Rc<Object>, Rc<Object>),
    RangeInclusive(Rc<Object>, Rc<Object>),
    RangeFull,

    Iterator(IteratorObject),

    Break(Option<Box<Object>>),
    Continue,
    Method(Method),

    Reference(Reference),

    Struct(HashMap<String, Object>),
}

impl Object {
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Object::Integer(i) => Some(*i),
            _ => None,
        }
    }
    
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Object::Float(f) => Some(*f),
            _ => None,
        }
    }
    
    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Object::Boolean(b) => Some(*b),
            _ => None,
        }
    }
    
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Object::String(s) => Some(s.as_ref()),
            _ => None,
        }
    }
    
    pub fn as_array(&self) -> Option<&Vec<Object>> {
        match self {
            Object::Array(a) => Some(a),
            _ => None,
        }
    }
    
    pub fn as_struct(&self) -> Option<&HashMap<String, Object>> {
        match self {
            Object::Struct(s) => Some(s),
            _ => None,
        }
    }
    
    pub fn as_range(&self) -> Option<(&Object, &Object)> {
        match self {
            Object::Range(from, to) => Some((from, to)),
            _ => None,
        }
    }
    
    pub fn as_range_inclusive(&self) -> Option<(&Object, &Object)> {
        match self {
            Object::RangeInclusive(from, to) => Some((from, to)),
            _ => None,
        }
    }
    
    pub fn as_range_from(&self) -> Option<&Object> {
        match self {
            Object::RangeFrom(from) => Some(from),
            _ => None,
        }
    }
    
    pub fn as_range_to(&self) -> Option<&Object> {
        match self {
            Object::RangeTo(to) => Some(to),
            _ => None,
        }
    }
    
    pub fn as_iterator(&self) -> Option<&IteratorObject> {
        match self {
            Object::Iterator(iterator) => Some(iterator),
            _ => None,
        }
    }
    
    /// Converts the object to an iterator if it is one
    /// 
    /// # Errors
    /// Returns an error containing the original object if it is not an iterator
    pub fn to_iterator(self) -> Result<IteratorObject, Object> {
        match self {
            Object::Iterator(iterator) => Ok(iterator),
            _ => Err(self),
        }
    }
    
    pub fn as_method(&self) -> Option<&Method> {
        match self {
            Object::Method(method) => Some(method),
            _ => None,
        }
    }
    
    pub fn as_reference(&self) -> Option<&Reference> {
        match self {
            Object::Reference(reference) => Some(reference),
            _ => None,
        }
    }
    
    pub fn as_function(&self) -> Option<&Function> {
        match self {
            Object::Function(function) => Some(function),
            _ => None,
        }
    }
    
    /// Converts the object to a function if it is one
    /// 
    /// # Errors
    /// Returns an error containing the original object if it is not a function
    pub fn to_function(self) -> Result<Function, Object> {
        match self {
            Object::Function(function) => Ok(function),
            _ => Err(self),
        }
    }
    
    pub fn as_builtin_function(&self) -> Option<&BuiltinFunction> {
        match self {
            Object::BuiltinFunction(builtin) => Some(builtin),
            _ => None,
        }
    }
    
    pub fn as_callable(&self) -> Option<&dyn Callable> {
        match self {
            Object::Function(function) => Some(function),
            Object::BuiltinFunction(builtin) => Some(builtin),
            _ => None,
        }
    }
    
    pub fn to_callable(self) -> Result<Rc<dyn Callable>, Object> {
        match self {
            Object::Function(function) => Ok(Rc::new(function)),
            Object::BuiltinFunction(builtin) => Ok(Rc::new(builtin)),
            _ => Err(self),
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a.partial_cmp(b),
            (Object::Float(a), Object::Float(b)) => a.partial_cmp(b),
            (Object::String(a), Object::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Float(float) => write!(f, "{}", float),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Unit => write!(f, "()"),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::String(string) => write!(f, "{}", string),
            Object::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({})", params)
            }
            Object::BuiltinFunction(builtin) => write!(f, "builtin function {}", builtin.name),
            Object::Array(array) => {
                let elements = array
                    .iter()
                    .map(|elem| elem.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "[{}]", elements)
            }
            Object::Struct(map) => {
                let items = map
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .join(", ");
                write!(f, "{{{}}}", items)
            }
            Object::RangeFrom(from) => write!(f, "{}..", from),
            Object::RangeTo(to) => write!(f, "..{}", to),
            Object::RangeToInclusive(to) => write!(f, "..={}", to),
            Object::Range(from, to) => write!(f, "{}..{}", from, to),
            Object::RangeInclusive(from, to) => write!(f, "{}..={}", from, to),
            Object::RangeFull => write!(f, ".."),
            Object::Iterator(iterator) => write!(f, "{}", iterator),
            Object::Break(Some(val)) => write!(f, "break {}", val),
            Object::Break(None) => write!(f, "break"),
            Object::Continue => write!(f, "continue"),
            Object::Method(method) => write!(f, "{}", method),
            Object::Reference(reference) => write!(f, "{}", reference),
        }
    }
}

impl From<bool> for Object {
    fn from(boolean: bool) -> Self {
        if boolean {
            TRUE
        } else {
            FALSE
        }
    }
}

impl From<&str> for Object {
    fn from(string: &str) -> Self {
        Object::String(string.into())
    }
}

impl From<String> for Object {
    fn from(string: String) -> Self {
        Object::String(string.into())
    }
}

impl From<Vec<Object>> for Object {
    fn from(array: Vec<Object>) -> Self {
        Object::Array(array)
    }
}

impl From<i64> for Object {
    fn from(int: i64) -> Self {
        Object::Integer(int)
    }
}

impl From<f64> for Object {
    fn from(float: f64) -> Self {
        Object::Float(float)
    }
}
