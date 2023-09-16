use std::{fmt::Display, rc::Rc};

use crate::parser::{expressions::Identifier, parser::BlockStatement};

use super::environment::Environment;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Let(Identifier, Box<Object>),
    Function(Function),
}

/// Trait for unwrapping return values
///
/// # Examples
/// ```
/// use interpreter::evaluator::objects::{Object, UnwrapReturnValue};
///
/// let obj = Object::ReturnValue(Box::new(Object::Integer(5)));
/// assert_eq!(obj.unwrap_return_value(), Object::Integer(5));
/// ```
pub trait UnwrapReturnValue {
    fn unwrap_return_value(self) -> Object;
}

impl UnwrapReturnValue for Object {
    /// Returns the inner value of a return value, otherwise returns the object
    ///
    /// # Examples
    /// ```
    /// use interpreter::evaluator::objects::{Object, UnwrapReturnValue};
    ///
    /// let obj = Object::ReturnValue(Box::new(Object::Integer(5)));
    /// assert_eq!(obj.unwrap_return_value(), Object::Integer(5));
    ///
    /// let obj = Object::Integer(5);
    /// assert_eq!(obj.unwrap_return_value(), Object::Integer(5));
    /// ```
    fn unwrap_return_value(self) -> Object {
        match self {
            Object::ReturnValue(val) => *val,
            _ => self,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub params: Rc<[Identifier]>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::Let(name, val) => write!(f, "{} = {}", name, val),
            Object::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({}) {{\n{}\n}}", params, function.body)
            }
        }
    }
}

impl Into<Object> for bool {
    fn into(self) -> Object {
        if self {
            TRUE
        } else {
            FALSE
        }
    }
}

impl Into<Object> for i64 {
    fn into(self) -> Object {
        Object::Integer(self)
    }
}
