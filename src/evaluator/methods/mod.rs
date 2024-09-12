use std::{
    fmt::{Display, Formatter},
    ops::RangeInclusive,
    rc::Rc,
};
use std::collections::HashMap;
use std::sync::Arc;
use lazy_static::lazy_static;

pub mod array_methods;
pub mod boolean_methods;
pub mod numeric_methods;
pub mod range_methods;
pub mod string_methods;
pub mod struct_methods;

use self::struct_methods::STRUCT_METHODS;
pub(crate) use self::{
    array_methods::ARRAY_METHODS,
    boolean_methods::BOOLEAN_METHODS,
    numeric_methods::FLOAT_METHODS,
    range_methods::{
        RANGE_FROM_METHODS, RANGE_FULL_METHODS, RANGE_INCLUSIVE_METHODS, RANGE_METHODS,
        RANGE_TO_INCLUSIVE_METHODS, RANGE_TO_METHODS,
    },
    string_methods::STRING_METHODS,
};

use crate::{Environment, EvalConfig, Evaluator, Object, Span};
use crate::evaluator::objects::function::{Callable};
use crate::evaluator::objects::iterators::IteratorObject;
use crate::evaluator::{Result, Error};
use crate::evaluator::methods::numeric_methods::INTEGER_METHODS;
use crate::evaluator::objects::{Argument, ArgumentList, ArgumentType, FunctionTy, MethodTy, ObjectTy};

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub ident: Option<Rc<str>>,
    pub method_ty: MethodTy,
    /// The function to call when the method is called
    ///
    /// # Arguments
    /// * `obj` - The object the method is being called on
    /// * `ident` - an identifier for the object in case it needs to be mutated
    /// * `args` - The arguments passed to the method
    /// * `env` - The environment the method is being called in
    pub function: MethodFunction,
    pub obj: Box<Object>,
    pub obj_span: Span,
    pub method_span: Span,
}

impl Method {
    pub fn name(&self) -> &str {
        self.method_ty.method_name
    }
}

impl Callable for Method {
    fn call(&self, _obj: Option<Object>, args: Vec<Argument>, env: &mut Environment, config: Rc<EvalConfig>) -> Result<Object> {
        let mut evaluator = Evaluator {
            globals: HashMap::new(),
            current_span: self.method_span.clone(),
            lines: vec![].into(),
            eval_config: config,
        };

        evaluator.apply_method(self.clone(), args, env)
    }

    fn args_len(&self) -> RangeInclusive<usize> {
        self.method_ty.function_ty.arguments.args_len()
    }

    fn argument_list(&self) -> Option<ArgumentList> {
        Some(self.method_ty.function_ty.arguments.clone())
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.method_ty)
    }
}

pub trait ObjectMethods {
    fn get_method(
        &self,
        name: &str,
        obj_span: Span,
        method_span: Span,
        obj_identifier: Option<Rc<str>>,
    ) -> std::result::Result<Method, MethodError>;
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
        obj_span: Span,
        method_span: Span,
        obj_identifier: Option<Rc<str>>,
    ) -> std::result::Result<Method, MethodError> {
        let get_method = |methods: &[MethodInner]| {
            methods
                .iter()
                .find(|m| m.name() == method_name)
                .map(|m| Method {
                    ident: obj_identifier.clone(),
                    function: m.function,
                    method_ty: m.method_ty.clone(),
                    obj: Box::new(self.clone()),
                    method_span: method_span.clone(),
                    obj_span: obj_span.clone(),
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
            Object::Struct(_) => get_method(&STRUCT_METHODS),

            // Explicitly return None for these types
            Object::Unit => None,
            Object::ReturnValue(_) => None,
            Object::Function(_) => None,
            Object::BuiltinFunction(_) => None,
            Object::Break(_) => None,
            Object::Continue => None,
            Object::Method(_) => None,
            Object::Reference(_) => None,
        }
            // Generic methods
            .or_else(|| match method_name {
                "iter" => Some(Method {
                    ident: obj_identifier,
                    method_ty: MethodTy {
                        method_name: "iter",
                        self_ty: Box::new(ObjectTy::Any),
                        function_ty: FunctionTy {
                            arguments: ArgumentList::None,
                            expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) })),
                        },
                    },
                    function: |args| {
                        Ok(Object::Iterator(IteratorObject::try_from(args.obj.clone()).map_err(|e| Error::IterCallOnNonIterable {
                            obj_span: args.obj_span.clone(),
                            iter_call_span: args.method_span.clone(),
                            reason: e,
                        })?))
                    },
                    obj: Box::new(self.clone()),
                    method_span: method_span.clone(),
                    obj_span: obj_span.clone(),
                }),
                "to_string" => Some(Method {
                    ident: obj_identifier,
                    method_ty: MethodTy {
                        method_name: "to_string",
                        self_ty: Box::new(ObjectTy::Any),
                        function_ty: FunctionTy {
                            arguments: ArgumentList::None,
                            expected_return_type: Some(Box::new(ObjectTy::String)),
                        },
                    },
                    function: |args| Ok(Object::String(args.obj.to_string().into())),
                    obj: Box::new(self.clone()),
                    method_span: method_span.clone(),
                    obj_span: obj_span.clone(),
                }),
                _ => None,
            });
        // If no method matches, check if the object is iterable and if the method would match on the iterator
        // If so, warn the user that they should call the method on the iterator instead
        if method.is_none()
            && IteratorObject::try_from(self.clone()).is_ok()
            && ITERATOR_METHODS
            .iter()
            .any(|m| m.name() == method_name)
        {
            return Err(MethodError::IterMethodOnIterable(method_name.to_string()));
        }

        method.ok_or(MethodError::NotFound)
    }
}

#[derive(Debug)]
pub struct MethodArgs<'a> {
    pub obj_span: Span,
    pub method_span: Span,
    pub obj: Object,
    pub obj_identifier: Option<Rc<str>>,
    pub args: Vec<Argument>,
    pub env: &'a mut Environment,
    pub config: Rc<EvalConfig>,
}

type MethodFunction = fn(
    args: MethodArgs,
) -> Result<Object>;

pub struct MethodInner {
    pub method_ty: MethodTy,
    function: MethodFunction,
}

impl MethodInner {
    pub fn name(&self) -> &str {
        self.method_ty.method_name
    }
}

lazy_static! {
/// Methods for the Iterator type
///
/// # Iterator Methods
///
/// ## Creating an iterator
///
/// To create an iterators, use the `iter` method on an `array`, `range`, or `string`.
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
    static ref ITERATOR_METHODS: Arc<[MethodInner]> = Arc::new([
        MethodInner {
            method_ty: MethodTy {
                method_name: "next",
                self_ty: Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Any)),
                },
            },
            function: |args| {
                if let Object::Iterator(mut iterator) = args.obj {
                    // mutate the object if it has an identifier
                    if let Some(ident) = args.obj_identifier {
                        return args.env.mutate(ident.clone(), |iter| {
                            Ok(if let Object::Iterator(iterator) = iter {
                                iterator.iterator.next().unwrap_or(Object::Unit)
                            } else {
                                Object::Unit
                            }.into())
                        }).map_err(|_| Error::MutateError {
                            span: args.obj_span.clone(),
                            name: ident.clone(),
                        }.into());
                    }

                    // otherwise just return the next item from the passed iterator
                    let item = iterator.next();
                    Ok(item.unwrap_or(Object::Unit))
                } else {
                    panic!("Expected Iterator, got {:?}", args.obj)
                }
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "collect",
                self_ty: Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Array { expected_item_types: Some(Box::new(ObjectTy::Any)) })),
                },
            },
            function: |args| {
                if let Object::Iterator(iterator) = args.obj {
                    let vec: Vec<Object> = iterator.collect();
                    Ok(Object::Array(vec))
                } else {
                    panic!("Expected Iterator, got {:?}", args.obj)
                }
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "step_by",
                self_ty: Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(
                        vec![ArgumentType { name: "count".into(), ty: ObjectTy::Integer }]),
                    expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) })),
                },
            },
            function: |args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator method to be called on an iterator");
                let step = args.args[0].as_integer().ok_or_else(|| Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Integer],
                    found: args.args[0].get_type(),
                })?;

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.step_by(step as usize)),
                }))
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "skip",
                self_ty: Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(
                        vec![ArgumentType { name: "count".into(), ty: ObjectTy::Integer }]),
                    expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) })),
                },
            },
            function: |args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator method to be called on an iterator");
                let skip = args.args[0].as_integer().ok_or_else(|| Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Integer],
                    found: args.args[0].get_type(),
                })?;

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.skip(skip as usize)),
                }))
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "take",
                self_ty: Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(
                        vec![ArgumentType { name: "count".into(), ty: ObjectTy::Integer }]),
                    expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) })),
                },
            },
            function: |args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator method to be called on an iterator");
                let take = args.args[0].as_integer().ok_or_else(|| Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Integer],
                    found: args.args[0].get_type(),
                })?;
                
                if let Some(ident) = args.obj_identifier {
                    let mutated = args.env.mutate(ident.clone(), |iter| {
                        if let Object::Iterator(iterator_ref) = iter {
                            Ok(Object::Iterator(IteratorObject {
                                iterator: Box::new(iterator_ref.take(take as usize).collect::<Vec<_>>().into_iter()),
                            }))
                        } else {
                            Err(Error::MutateError {
                                span: args.obj_span, name: ident,
                            }.into())
                        }
                    });
                    
                    if let Ok(iterator) = mutated {
                        return Ok(iterator);
                    }
                }

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.take(take as usize)),
                }))
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "filter",
                self_ty: Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) }),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(
                        vec![ArgumentType {
                            name: "predicate".into(),
                            ty: ObjectTy::Function {
                                function_ty: FunctionTy {
                                    arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                                    expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                                },
                            },
                        }]),
                    expected_return_type: Some(Box::new(ObjectTy::Iterator { item: Box::new(ObjectTy::Any) })),
                },
            },
            function: |mut args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator, got non-iterator");

                let expected_type = ObjectTy::Function {
                    function_ty: FunctionTy {
                        arguments: ArgumentList::new_exactly(
                            vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                        expected_return_type: Some(Box::new(ObjectTy::Boolean)),
                    },
                };

                let predicate = args.args.remove(0);

                let invalid_predicate_type = || Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![expected_type.clone()],
                    found: predicate.get_type(),
                };

                let predicate = predicate.value.clone().to_callable()
                    .map_err(|e| invalid_predicate_type())?;

                let args_len = predicate.args_len();

                if !args_len.contains(&1usize) {
                    return Err(Error::TypeError {
                        span: args.method_span.clone(),
                        expected: vec![expected_type],
                        found: ObjectTy::Function {
                            function_ty: FunctionTy {
                                arguments: predicate.argument_list().unwrap_or(ArgumentList::Unknown),
                                expected_return_type: Some(Box::new(ObjectTy::Unknown)),
                            },
                        },
                    }.into());
                }
                
                // Create an enclosed environment for the closure
                let enclosed_env = Environment::new_enclosed(args.env);

                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.filter(move |item| {
                        let mut env = Environment::new_enclosed(&enclosed_env);
                        match predicate.call(None, vec![Argument::from_obj(item.clone())], &mut env, args.config.clone()) {
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
            method_ty: MethodTy {
                method_name: "map",
                self_ty: Box::new(ObjectTy::Unknown),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(
                        vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                    expected_return_type: Some(Box::new(ObjectTy::Any)),
                },
            },
            function: |mut args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator, got non-iterator");
                

                let transform = args.args.remove(0);

                let invalid_transform_type = || Box::new(Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Function {
                        function_ty: FunctionTy {
                            arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                            expected_return_type: Some(Box::new(ObjectTy::Any)),
                        },
                    }],
                    found: transform.get_type(),
                });

                let function = transform.value.clone().to_callable().map_err(|e| invalid_transform_type())?;
                let args_len = function.args_len();

                if !args_len.contains(&1usize) {
                    return Err(invalid_transform_type());
                }

                let enclosed_env = Environment::new_enclosed(args.env);
                Ok(Object::Iterator(IteratorObject {
                    iterator: Box::new(iterator.map(move |item| {
                        let mut env = Environment::new_enclosed(&enclosed_env);
                        let function_span = transform.span.clone();
                        function.call(None, vec![Argument {span: function_span ,value: item}], &mut env, args.config.clone()).unwrap_or_else(|e| {
                            eprintln!("Error evaluating closure in map: {}", e);
                            Object::Unit
                        })
                    })),
                }))
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "count",
                self_ty: Box::new(ObjectTy::Unknown),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Integer)),
                },
            },
            function: |args| {
                if let Object::Iterator(iterator) = args.obj {
                    Ok(Object::Integer(iterator.count() as i64))
                } else {
                    panic!("Expected Iterator, got {:?}", args.obj)
                }
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "sum",
                self_ty: Box::new(ObjectTy::Unknown),
                function_ty: FunctionTy {
                    arguments: ArgumentList::None,
                    expected_return_type: Some(Box::new(ObjectTy::Integer)),
                },
            },
            function: |args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator, got non-iterator");
                Ok(iterator.sum())
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "for_each",
                self_ty: Box::new(ObjectTy::Unknown),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_exactly(
                        vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                    expected_return_type: None,
                },
            },
            function: |mut args| {
                if let Object::Iterator(iterator) = args.obj {
                    let expected_type = ObjectTy::Function {
                        function_ty: FunctionTy {
                            arguments: ArgumentList::new_exactly(vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }]),
                            expected_return_type: None,
                        },
                    };

                    
                    let function = args.args[0]
                        .value
                        .as_callable()
                        .ok_or_else(|| Error::TypeError {
                            span: args.method_span.clone(),
                            expected: vec![expected_type.clone()],
                            found: args.args[0].get_type(),
                        })?;
                    
                    let args_len = function.args_len();
                    
                    if !args_len.contains(&1usize) {
                        return Err(Error::TypeError {
                            span: args.method_span.clone(),
                            expected: vec![expected_type.clone()],
                            found: args.args[0].get_type(),
                        }.into());
                    }

                    iterator.for_each(|item| {
                        let mut env = Environment::new_enclosed(args.env);
                        function.call(None, vec![Argument::from_obj(item)], &mut env, Rc::new(EvalConfig::default())).unwrap_or_else(|e| {
                            eprintln!("Error evaluating closure in for_each {}", e);
                            Object::Unit
                        });
                    });
                    Ok(Object::Unit)
                } else {
                    panic!("Expected Iterator, got {:?}", args.obj)
                }
            },
        },
        MethodInner {
            method_ty: MethodTy {
                method_name: "fold",
                self_ty: Box::new(ObjectTy::Unknown),
                function_ty: FunctionTy {
                    arguments: ArgumentList::new_bounded(1..=2, vec![
                        ArgumentType { name: "initial".into(), ty: ObjectTy::Any },
                        ArgumentType {
                            name: "accumulator".into(),
                            ty: ObjectTy::Function {
                                function_ty: FunctionTy {
                                    arguments: ArgumentList::new_exactly(vec![
                                        ArgumentType { name: "acc".into(), ty: ObjectTy::Any },
                                        ArgumentType { name: "item".into(), ty: ObjectTy::Any },
                                    ]),
                                    expected_return_type: Some(Box::new(ObjectTy::Any)),
                                },
                            },
                        },
                    ]),
                    expected_return_type: Some(Box::new(ObjectTy::Any)),
                },
            },
            function: |mut args| {
                let iterator = args.obj.to_iterator().expect("Expected Iterator, got non-iterator");

                let initial = args.args.remove(0).value;

                let invalid_fold_function_type = || Box::new(Error::TypeError {
                    span: args.method_span.clone(),
                    expected: vec![ObjectTy::Function {
                        function_ty: FunctionTy {
                            arguments: ArgumentList::new_exactly(vec![
                                ArgumentType { name: "acc".into(), ty: ObjectTy::Any },
                                ArgumentType { name: "item".into(), ty: ObjectTy::Any },
                            ]),
                            expected_return_type: Some(Box::new(ObjectTy::Any)),
                        },
                    }],
                    found: args.args[0].get_type(),
                });

                let function = args.args[0].value
                    .as_callable()
                    .ok_or_else(invalid_fold_function_type)?;
                
                if !function.args_len().contains(&2usize) {
                    return Err(invalid_fold_function_type())
                }

                Ok(iterator.clone().fold(initial, move |acc, obj| {
                    let mut env = Environment::new_enclosed(args.env);
                    function.call(None, vec![Argument::from_obj(acc.clone()), Argument::from_obj(obj.clone())], &mut env, args.config.clone()).unwrap_or_else(|e| {
                        eprintln!("Error evaluating closure in fold {}", e);
                        Object::Unit
                    })
                }))
            },
        },
    ]
        );
    }

