use std::{
    fmt::{Display, Formatter},
    ops::RangeInclusive,
    rc::Rc,
};

pub mod array_methods;
pub mod boolean_methods;
pub mod numeric_methods;
pub mod range_methods;
pub mod string_methods;

pub use self::{
    array_methods::ARRAY_METHODS,
    boolean_methods::BOOLEAN_METHODS,
    numeric_methods::FLOAT_METHODS,
    range_methods::{
        RANGE_FROM_METHODS, RANGE_FULL_METHODS, RANGE_INCLUSIVE_METHODS, RANGE_METHODS,
        RANGE_TO_INCLUSIVE_METHODS, RANGE_TO_METHODS,
    },
    string_methods::STRING_METHODS,
};

use super::{
    environment::Environment,
    eval::{EvalConfig, Evaluator},
    iterators::IteratorObject,
    objects::Object,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    #[cfg(not(debug))]
    pub method_name: &'static str,
    pub ident: Option<Rc<str>>,
    pub args_len: RangeInclusive<usize>,
    pub obj: Box<Object>,
    /// The function to call when the method is called
    ///
    /// # Arguments
    /// * `obj` - The object the method is being called on
    /// * `ident` - an identifier for the object in case it needs to be mutated
    /// * `args` - The arguments passed to the method
    /// * `env` - The environment the method is being called in
    pub function: fn(
        obj: Object,
        ident: Option<&str>,
        args: Vec<Object>,
        env: &mut Environment,
        config: Rc<EvalConfig>,
    ) -> Result<Object, String>,
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.method_name)
    }
}

pub trait ObjectMethods {
    fn get_method(
        &self,
        name: &str,
        obj_identifier: Option<Rc<str>>,
    ) -> Result<Method, MethodError>;
}

pub enum MethodError {
    NotFound,
    IterMethodOnIterable(String),
}

impl Display for MethodError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MethodError::NotFound => write!(f, "No such method for this type"),
            MethodError::IterMethodOnIterable(method) => {
                write!(f, "Called iterator method {} on an iterable object instead of on an iterator. Perhaps you meant to call `iter()` first?", method)
            }
        }
    }
}

impl ObjectMethods for Object {
    fn get_method(
        &self,
        method_name: &str,
        obj_identifier: Option<Rc<str>>,
    ) -> Result<Method, MethodError> {
        let get_method = |methods: &[MethodInner]| {
            methods
                .iter()
                .find(|m| m.name == method_name)
                .map(|m| Method {
                    method_name: m.name,
                    ident: obj_identifier.clone(),
                    args_len: m.args_len.clone(),
                    function: m.function,
                    obj: Box::new(self.clone()),
                })
        };
        // Type specific methods
        let method = match self {
            Object::Array(_) => get_method(&ARRAY_METHODS),
            Object::Integer(_) => get_method(&INTEGER_METHODS),
            Object::Range(_, _) => get_method(&RANGE_METHODS),
            Object::RangeInclusive(_, _) => get_method(&RANGE_INCLUSIVE_METHODS),
            Object::RangeFull => get_method(&RANGE_FULL_METHODS),
            Object::RangeFrom(_) => get_method(&RANGE_FROM_METHODS),
            Object::RangeTo(_) => get_method(&RANGE_TO_METHODS),
            Object::RangeToInclusive(_) => get_method(&RANGE_TO_INCLUSIVE_METHODS),
            Object::Iterator(_) => get_method(&ITERATOR_METHODS),
            Object::Float(_) => get_method(&FLOAT_METHODS),
            Object::String(_) => get_method(&STRING_METHODS),
            Object::Boolean(_) => get_method(&BOOLEAN_METHODS),

            // Explicitly return None for these types
            Object::Unit => None,
            Object::ReturnValue(_) => None,
            Object::Function(_) => None,
            Object::BuiltinFunction(_) => None,
            Object::Break(_) => None,
            Object::Continue => None,
            Object::Method(_) => None,
        }
        // Generic methods
        .or_else(|| match method_name {
            "iter" => Some(Method {
                method_name: "iter",
                ident: obj_identifier,
                args_len: 0..=0,
                function: |obj, _, _, _, _| {
                    Ok(Object::Iterator(IteratorObject::try_from(obj.clone())?))
                },
                obj: Box::new(self.clone()),
            }),
            "to_string" => Some(Method {
                method_name: "to_string",
                ident: obj_identifier,
                args_len: 0..=0,
                function: |obj, _, _, _, _| Ok(Object::String(obj.to_string().into())),
                obj: Box::new(self.clone()),
            }),
            _ => None,
        });
        // If no method matches, check if the object is iterable and if the method would match on the iterator
        // If so, warn the user that they should call the method on the iterator instead
        if method.is_none() {
            if let Ok(_) = IteratorObject::try_from(self.clone()) {
                if ITERATOR_METHODS
                    .iter()
                    .find(|m| m.name == method_name)
                    .is_some()
                {
                    return Err(MethodError::IterMethodOnIterable(method_name.to_string()));
                }
            }
        }

        method.ok_or(MethodError::NotFound)
    }
}

pub struct MethodInner {
    pub name: &'static str,
    pub args_len: RangeInclusive<usize>,
    function: fn(
        obj: Object,
        ident: Option<&str>,
        args: Vec<Object>,
        env: &mut Environment,
        config: Rc<EvalConfig>,
    ) -> Result<Object, String>,
}

/// Methods for the Iterator type
///
/// # Iterator Methods
///
/// ## Creating an iterator
///
/// To create an iteratos, use the `iter` method on an `array`, `range`, or `string`.
/// This will return an iterator that can be used to iterate over the object.
///
/// ```
/// # mechylang::test_utils::test_eval_ok(r#"
/// let array = [1, 2, 3];
/// let iterator = array.iter();
/// assert(iterator.next() == 1);
/// assert(iterator.next() == 2);
/// assert(iterator.next() == 3);
/// assert(iterator.next() == ());
/// # "#);
/// ```
pub const ITERATOR_METHODS: [MethodInner; 10] =
    [
        MethodInner {
            name: "next",
            args_len: 0..=0,
            function: |obj, ident, _, env, _| {
                if let Object::Iterator(mut iterator) = obj {
                    // mutate the object if it has an identifier
                    if let Some(ident) = ident {
                        return env.update(ident, |iter| {
                            Ok(if let Object::Iterator(iterator) = iter {
                                iterator.iterator.next().unwrap_or(Object::Unit)
                            } else {
                                Object::Unit
                            })
                        });
                    }

                    // otherwise just return the next item from the passed iterator
                    let item = iterator.next();
                    Ok(item.unwrap_or(Object::Unit))
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "collect",
            args_len: 0..=0,
            function: |obj, _, _, _, _| {
                if let Object::Iterator(iterator) = obj {
                    let vec: Vec<Object> = iterator.collect();
                    Ok(Object::Array(vec))
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "step_by",
            args_len: 1..=1,
            function: |obj, _, args, _, _| {
                if let Object::Iterator(iterator) = obj {
                    let step = match args[0] {
                        Object::Integer(i) => i,
                        _ => return Err("Expected integer for step".to_string()),
                    };

                    Ok(Object::Iterator(IteratorObject {
                        iterator: Box::new(iterator.step_by(step as usize)),
                    }))
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "skip",
            args_len: 1..=1,
            function: |obj, _, args, _, _| {
                if let Object::Iterator(iterator) = obj {
                    let skip = match args[0] {
                        Object::Integer(i) => i,
                        _ => return Err("Expected integer for skip".to_string()),
                    };

                    Ok(Object::Iterator(IteratorObject {
                        iterator: Box::new(iterator.skip(skip as usize)),
                    }))
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "take",
            args_len: 1..=1,
            function: |obj, _, args, _, _| {
                if let Object::Iterator(iterator) = obj {
                    let take = match args[0] {
                        Object::Integer(i) => i,
                        _ => return Err("Expected integer for take".to_string()),
                    };

                    Ok(Object::Iterator(IteratorObject {
                        iterator: Box::new(iterator.take(take as usize)),
                    }))
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "filter",
            args_len: 1..=1,
            function: |obj, _, args, _, config| {
                let iterator = match obj {
                    Object::Iterator(iterator) => iterator,
                    _ => return Err(format!("Expected Iterator, got {}", obj)),
                };

                let predicate = match &args[0] {
                    Object::Function(f) => f.clone(),
                    _ => return Err("Expected function for filter".to_string()),
                };

                match predicate.params.len() {
                    1 => {}
                    _ => return Err("Expected function with 1 parameter".to_string()),
                }

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.filter(move |item| {
                        match predicate.call(vec![item.clone()], None, config.clone()) {
                            Ok(obj) => Evaluator::is_truthy(&obj),
                            Err(e) => {
                                eprintln!("Error evaluating closure in filter {}", e);
                                false
                            }
                        }
                    })),
                }))
            },
        },
        MethodInner {
            name: "map",
            args_len: 1..=1,
            function: |obj, _, args, _, config| {
                let iterator = match obj {
                    Object::Iterator(iterator) => iterator,
                    _ => return Err(format!("Expected Iterator, got {}", obj)),
                };

                let function = match &args[0] {
                    Object::Function(f) => f.clone(),
                    _ => return Err("Expected function for map".to_string()),
                };

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.map(move |item| {
                        match function.call(vec![item], None, config.clone()) {
                            Ok(obj) => obj,
                            Err(e) => {
                                eprintln!("Error evaluating closure in map {}", e);
                                Object::Unit
                            }
                        }
                    })),
                }))
            },
        },
        MethodInner {
            name: "count",
            args_len: 0..=0,
            function: |obj, _, _, _, _| {
                if let Object::Iterator(iterator) = obj {
                    Ok(Object::Integer(iterator.count() as i64))
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "sum",
            args_len: 0..=0,
            function: |obj, _, _, _, _| {
                if let Object::Iterator(ref iterator) = obj {
                    Ok(iterator.clone().sum())
                } else {
                    Err(format!("Expected Iterator, got {}", obj))
                }
            },
        },
        MethodInner {
            name: "fold",
            args_len: 2..=2,
            function: |obj, _, args, _, config| {
                let iterator = match obj {
                    Object::Iterator(iterator) => iterator,
                    _ => return Err(format!("Expected Iterator, got {}", obj)),
                };

                let initial = args[0].clone();
                let function = match &args[1] {
                    Object::Function(f) => f.clone(),
                    _ => return Err("Expected function for fold".to_string()),
                };

                match function.params.len() {
                    2 => {}
                    _ => return Err(
                        "Expected function with 2 parameters: initial, fn(acc, v) -> updated acc"
                            .to_string(),
                    ),
                }

                Ok(iterator.clone().fold(initial, move |acc, obj| {
                    let res =
                        match function.call(vec![acc.clone(), obj.clone()], None, config.clone()) {
                            Ok(obj) => obj,
                            Err(e) => {
                                eprintln!("Error evaluating closure in fold {}", e);
                                Object::Unit
                            }
                        };

                    res
                }))
            },
        },
    ];

/// Methods for the `Object::Integer` type
///
/// # Integer Methods
///
/// ## Pow
/// `pow(Integer) -> Integer`
/// Raises the integer to the power of the given integer
///
/// ```rust
/// # mechylang::test_utils::test_eval_ok(r#"
/// assert_eq(2.pow(3), 8);
/// assert_eq(2.pow(0), 1);
/// # "#);
/// ```
pub const INTEGER_METHODS: [MethodInner; 1] = [MethodInner {
    name: "pow",
    args_len: 1..=1,
    function: |obj, _, args, _, _| {
        let exponent = match args[0] {
            Object::Integer(i) => i,
            _ => return Err("Expected integer for base".to_string()),
        };

        let base = match obj {
            Object::Integer(i) => i,
            _ => return Err("Expected integer".to_string()),
        };

        Ok(Object::Integer(base.pow(exponent as u32)))
    },
}];

pub fn get_mutable_ident(ident: Option<&str>) -> Result<&str, String> {
    match ident {
        Some(ident) => Ok(ident),
        None => {
            Err("Cannot mutate object in place without being called on an identifier".to_string())
        }
    }
}
