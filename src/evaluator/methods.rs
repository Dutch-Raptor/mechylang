use std::{
    fmt::{Display, Formatter},
    ops::RangeInclusive,
    rc::Rc,
};

use super::{
    environment::Environment, eval::Evaluator, iterators::IteratorObject, objects::Object,
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
        obj: &Object,
        ident: Option<&str>,
        args: Vec<Object>,
        env: &mut Environment,
    ) -> Result<Object, String>,
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.method_name)
    }
}

pub trait ObjectMethods {
    fn get_method(&self, name: &str, obj_identifier: Option<Rc<str>>) -> Option<Method>;
}

impl ObjectMethods for Object {
    fn get_method(&self, method_name: &str, obj_identifier: Option<Rc<str>>) -> Option<Method> {
        // Type specific methods
        match self {
            Object::Array(_) => ARRAY_METHODS
                .iter()
                .find(|m| m.name == method_name)
                .map(|m| Method {
                    method_name: m.name,
                    ident: obj_identifier.clone(),
                    args_len: m.args_len.clone(),
                    function: m.function,
                    obj: Box::new(self.clone()),
                }),
            Object::Integer(_) => INTEGER_METHODS
                .iter()
                .find(|m| m.name == method_name)
                .map(|m| Method {
                    method_name: m.name,
                    ident: obj_identifier.clone(),
                    args_len: m.args_len.clone(),
                    function: m.function,
                    obj: Box::new(self.clone()),
                }),
            Object::Range(_, _) => RANGE_METHODS
                .iter()
                .find(|m| m.name == method_name)
                .map(|m| Method {
                    method_name: m.name,
                    ident: obj_identifier.clone(),
                    args_len: m.args_len.clone(),
                    function: m.function,
                    obj: Box::new(self.clone()),
                }),
            Object::Iterator(_) => {
                ITERATOR_METHODS
                    .iter()
                    .find(|m| m.name == method_name)
                    .map(|m| Method {
                        method_name: m.name,
                        ident: obj_identifier.clone(),
                        args_len: m.args_len.clone(),
                        function: m.function,
                        obj: Box::new(self.clone()),
                    })
            }
            _ => None,
        }
        // Generic methods
        .or_else(|| match method_name {
            "iter" => Some(Method {
                method_name: "iter",
                ident: obj_identifier,
                args_len: 0..=0,
                function: |obj, _, _, _| {
                    Ok(Object::Iterator(IteratorObject::try_from(obj.clone())?))
                },
                obj: Box::new(self.clone()),
            }),
            "to_string" => Some(Method {
                method_name: "to_string",
                ident: obj_identifier,
                args_len: 0..=0,
                function: |obj, _, _, _| Ok(Object::String(obj.to_string().into())),
                obj: Box::new(self.clone()),
            }),
            _ => None,
        })
    }
}

struct MethodInner {
    name: &'static str,
    args_len: RangeInclusive<usize>,
    function: fn(
        obj: &Object,
        ident: Option<&str>,
        args: Vec<Object>,
        env: &mut Environment,
    ) -> Result<Object, String>,
}

const RANGE_METHODS: [MethodInner; 0] = [];

const ITERATOR_METHODS: [MethodInner; 10] = [
    MethodInner {
        name: "next",
        args_len: 0..=0,
        function: |obj, _, _, _| {
            if let Object::Iterator(ref iterator) = obj {
                iterator
                    .clone()
                    .next()
                    .ok_or("Iterator is empty".to_string())
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "collect",
        args_len: 0..=0,
        function: |obj, _, _, _| {
            if let Object::Iterator(ref iterator) = obj {
                let vec: Vec<Object> = iterator.clone().collect();
                Ok(Object::Array(vec))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "step_by",
        args_len: 1..=1,
        function: |obj, _, args, _| {
            if let Object::Iterator(ref iterator) = obj {
                let step = match args[0] {
                    Object::Integer(i) => i,
                    _ => return Err("Expected integer for step".to_string()),
                };

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.clone().step_by(step as usize)),
                }))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "skip",
        args_len: 1..=1,
        function: |obj, _, args, _| {
            if let Object::Iterator(ref iterator) = obj {
                let skip = match args[0] {
                    Object::Integer(i) => i,
                    _ => return Err("Expected integer for skip".to_string()),
                };

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.clone().skip(skip as usize)),
                }))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "take",
        args_len: 1..=1,
        function: |obj, _, args, _| {
            if let Object::Iterator(ref iterator) = obj {
                let take = match args[0] {
                    Object::Integer(i) => i,
                    _ => return Err("Expected integer for take".to_string()),
                };

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.clone().take(take as usize)),
                }))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "filter",
        args_len: 1..=1,
        function: |obj, _, args, _| {
            if let Object::Iterator(ref iterator) = obj {
                let predicate = match &args[0] {
                    Object::Function(f) => f.clone(),
                    _ => return Err("Expected function for filter".to_string()),
                };

                match predicate.params.len() {
                    1 => {}
                    _ => return Err("Expected function with 1 parameter".to_string()),
                }

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.clone().filter(move |obj| {
                        match predicate.call(vec![obj.clone()], None) {
                            Ok(obj) => Evaluator::is_truthy(&obj),
                            Err(e) => {
                                eprintln!("Error evaluating closure in filter {}", e);
                                false
                            }
                        }
                    })),
                }))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "map",
        args_len: 1..=1,
        function: |obj, _, args, _| {
            if let Object::Iterator(ref iterator) = obj {
                let predicate = match &args[0] {
                    Object::Function(f) => f.clone(),
                    _ => return Err("Expected function for map".to_string()),
                };

                match predicate.params.len() {
                    1 => {}
                    _ => return Err("Expected function with 1 parameter".to_string()),
                }

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.clone().map(move |obj| {
                        match predicate.call(vec![obj.clone()], None) {
                            Ok(obj) => obj,
                            Err(e) => {
                                eprintln!("Error evaluating closure in map {}", e);
                                Object::Null
                            }
                        }
                    })),
                }))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "count",
        args_len: 0..=0,
        function: |obj, _, _, _| {
            if let Object::Iterator(ref iterator) = obj {
                Ok(Object::Integer(iterator.clone().count() as i64))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
    MethodInner {
        name: "sum",
        args_len: 0..=0,
        function: |obj, _, _, _| {
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
        function: |obj, _, args, _| {
            let initial = args[0].clone();
            let function = match &args[1] {
                Object::Function(f) => f.clone(),
                _ => return Err("Expected function for fold".to_string()),
            };

            if let Object::Iterator(ref iterator) = obj {
                match function.params.len() {
                    2 => {}
                    _ => return Err(
                        "Expected function with 2 parameters: initial, fn(acc, v) -> updated acc"
                            .to_string(),
                    ),
                }

                Ok(iterator.clone().fold(initial, move |acc, obj| {
                    let res = match function.call(vec![acc.clone(), obj.clone()], None) {
                        Ok(obj) => obj,
                        Err(e) => {
                            eprintln!("Error evaluating closure in fold {}", e);
                            Object::Null
                        }
                    };

                    res
                }))
            } else {
                Err(format!("Expected Iterator, got {}", obj))
            }
        },
    },
];

const INTEGER_METHODS: [MethodInner; 1] = [MethodInner {
    name: "pow",
    args_len: 1..=1,
    function: |obj, _, args, _| {
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

const ARRAY_METHODS: [MethodInner; 5] = [
    MethodInner {
        name: "push",
        args_len: 1..=1,
        function: |_, ident, args, env| {
            let value = args[0].clone();

            let ident = get_mutable_ident(ident)?;

            env.update(ident.to_string(), move |arr| {
                if let Object::Array(ref mut arr) = arr {
                    arr.push(value.clone());
                    Ok(Object::Null)
                } else {
                    Err(format!("Expected array, got {}", arr))
                }
            })
            .map_err(|e| e.to_string())?;
            Ok(Object::Null)
        },
    },
    MethodInner {
        name: "pop",
        args_len: 0..=0,
        function: |_, ident, _, env| {
            let ident = get_mutable_ident(ident)?;

            let item = env
                .update(ident.to_string(), move |arr| {
                    if let Object::Array(ref mut arr) = arr {
                        arr.pop()
                            .map(|v| Ok(v))
                            .unwrap_or(Err("Array is empty".to_string()))
                    } else {
                        Err(format!("Expected array, got {}", arr))
                    }
                })
                .map_err(|e| e.to_string())?;
            Ok(item)
        },
    },
    MethodInner {
        name: "first",
        args_len: 0..=0,
        function: |_, ident, _, env| {
            let ident = get_mutable_ident(ident)?;

            let item = env
                .update(ident.to_string(), move |arr| {
                    if let Object::Array(ref mut arr) = arr {
                        arr.first()
                            .map(|v| Ok(v.clone()))
                            .unwrap_or(Err("Array is empty".to_string()))
                    } else {
                        Err(format!("Expected array, got {}", arr))
                    }
                })
                .map_err(|e| e.to_string())?;
            Ok(item)
        },
    },
    MethodInner {
        name: "last",
        args_len: 0..=0,
        function: |_, ident, _, env| {
            let ident = get_mutable_ident(ident)?;

            let item = env
                .update(ident.to_string(), move |arr| {
                    if let Object::Array(ref mut arr) = arr {
                        arr.last()
                            .map(|v| Ok(v.clone()))
                            .unwrap_or(Err("Array is empty".to_string()))
                    } else {
                        Err(format!("Expected array, got {}", arr))
                    }
                })
                .map_err(|e| e.to_string())?;
            Ok(item)
        },
    },
    MethodInner {
        name: "len",
        args_len: 0..=0,
        function: |obj, _, _, _| match obj {
            Object::Array(ref arr) => Ok(Object::Integer(arr.len() as i64)),
            _ => Err("Argument to `len` not supported".to_string()),
        },
    },
];

fn get_mutable_ident(ident: Option<&str>) -> Result<&str, String> {
    match ident {
        Some(ident) => Ok(ident),
        None => {
            Err("Cannot mutate object in place without being called on an identifier".to_string())
        }
    }
}
