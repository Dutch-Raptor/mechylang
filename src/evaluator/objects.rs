use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::parser::{expressions::Identifier, parser::BlockStatement};

use super::{
    builtins::BuiltinFunction,
    environment::Environment,
    eval::{EvalConfig, Evaluator},
    iterators::IteratorObject,
    methods::Method,
};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Function(Function),
    String(Rc<str>),
    BuiltinFunction(BuiltinFunction),
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
}

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

#[derive(Clone)]
pub struct Function {
    pub params: Rc<[Identifier]>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    pub fn call(
        &self,
        args: Vec<Object>,
        env: Option<Environment>,
        config: Rc<EvalConfig>,
    ) -> Result<Object, String> {
        Evaluator::eval_function(Object::Function(self.clone()), args, env, config)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.body == other.body
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .params
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .params
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Float(float) => write!(f, "{}", float),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::String(string) => write!(f, "{}", string),
            Object::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({}) {{\n{}\n}}", params, function.body)
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
