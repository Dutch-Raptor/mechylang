pub(crate) mod function;
pub(crate) mod iterators;
pub(crate) mod reference;
pub(crate) mod traits;

use itertools::Itertools;

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};
use std::ops::{Deref, RangeInclusive};
use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct ArgumentType {
    pub name: Arc<str>,
    pub ty: ObjectTy,
}

impl Display for ArgumentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_ref();
        let ty = self.ty.to_string();
        write!(f, "{name}: {ty}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArgumentList {
    /// The function takes no arguments
    None,
    /// It is unknown how many or what types of arguments the function takes
    Unknown,
    Exactly {
        args: Vec<ArgumentType>,
    },
    Bounded {
        bound: RangeInclusive<usize>,
        /// If the length of the args is less than the upper bound, 
        /// it is assumed that the last given argument type can be repeated
        args: Vec<ArgumentType>,
    },
}

impl Display for ArgumentList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgumentList::None => Ok(()),
            ArgumentList::Exactly { args } => write!(f, "{}", args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", ")),
            ArgumentList::Bounded { bound, args } => {
                if bound.start() == &0 && bound.end() == &0 {
                    return Ok(());
                }
                if args.is_empty() {
                    let should_trail = bound.end() > bound.start();
                    write!(f, "{}", (0..*bound.start()).map(|_| ObjectTy::Unknown.to_string()).collect::<Vec<String>>().join(", "))?;

                    if should_trail {
                        write!(f, ", ...")?;
                    }
                    return Ok(());
                }

                write!(f, "{}", args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", "))?;
                if args.len() != *bound.end() {
                    write!(f, "...")?;
                }

                Ok(())
            }
            ArgumentList::Unknown => write!(f, "Unknown"),
        }
    }
}

impl ArgumentList {
    pub fn new_exactly(args: Vec<ArgumentType>) -> Self {
        ArgumentList::Exactly { args }
    }

    pub fn new_bounded(bound: RangeInclusive<usize>, args: Vec<ArgumentType>) -> Self {
        ArgumentList::Bounded { bound, args }
    }

    pub fn args_len(&self) -> RangeInclusive<usize> {
        match self {
            ArgumentList::None => 0..=0,
            ArgumentList::Exactly { args } => args.len()..=args.len(),
            ArgumentList::Bounded { bound, .. } => bound.clone(),
            ArgumentList::Unknown => 0..=usize::MAX,
        }
    }

    pub fn from_args_len(args_len: RangeInclusive<usize>) -> Self {
        let start = args_len.start();
        let end = args_len.end();

        if start == &0 && end == &0 {
            ArgumentList::None
        } else if start == &0 && end == &usize::MAX {
            ArgumentList::Unknown
        } else {
            ArgumentList::Bounded {
                args: vec![ArgumentType { name: "item".into(), ty: ObjectTy::Any }],
                bound: args_len,
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectTy {
    Any,
    AnyOf { types: Vec<ObjectTy> },
    Unit,
    Integer,
    Float,
    Boolean,
    String,
    Array { expected_item_types: Option<Box<ObjectTy>> },
    Range { from: Box<ObjectTy>, to: Box<ObjectTy> },
    RangeInclusive { from: Box<ObjectTy>, to: Box<ObjectTy> },
    RangeFrom { from: Box<ObjectTy> },
    RangeTo { to: Box<ObjectTy> },
    RangeToInclusive { to: Box<ObjectTy> },
    RangeFull,
    Iterator { item: Box<ObjectTy> },
    Function {
        function_ty: FunctionTy,
    },
    Method { method_ty: MethodTy },
    Struct { expected_field_type: Option<Box<ObjectTy>> },
    Reference,
    Unknown,
}

impl Display for ObjectTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectTy::Any => write!(f, "any"),
            ObjectTy::AnyOf { types } => {
                types.iter().map(ToString::to_string).intersperse(" | ".to_string()).map(|s| write!(f, "{s}")).collect()
            }
            ObjectTy::Unit => write!(f, "()"),
            ObjectTy::Integer => write!(f, "int"),
            ObjectTy::Float => write!(f, "float"),
            ObjectTy::Boolean => write!(f, "bool"),
            ObjectTy::String => write!(f, "string"),
            ObjectTy::Array { expected_item_types } => write!(f, "[{}]", expected_item_types.as_ref().map(|ty| ty.to_string()).unwrap_or_else(|| "any".to_string())),
            ObjectTy::Range { from, to } => write!(f, "{}..{}", from, to),
            ObjectTy::RangeInclusive { from, to } => write!(f, "{}..={}", from, to),
            ObjectTy::RangeFrom { from } => write!(f, "{}..", from),
            ObjectTy::RangeTo { to } => write!(f, "..{}", to),
            ObjectTy::RangeToInclusive { to } => write!(f, "..={}", to),
            ObjectTy::RangeFull => write!(f, ".."),
            ObjectTy::Iterator { item } => write!(f, "Iter(item: {item})"),
            ObjectTy::Function { function_ty } => write!(f, "{function_ty}"),
            ObjectTy::Method { method_ty } => write!(f, "{method_ty}"),
            ObjectTy::Struct { expected_field_type } => write!(f, "{{{}}}", expected_field_type.as_deref().unwrap_or(&ObjectTy::Unknown).to_string()),
            ObjectTy::Reference => write!(f, "&any"),
            ObjectTy::Unknown => write!(f, "unknown"),
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct MethodTy {
    pub method_name: &'static str,
    pub self_ty: Box<ObjectTy>,
    pub function_ty: FunctionTy,
}

impl Display for MethodTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let method_name = self.method_name;
        let self_ty = self.self_ty.to_string();
        let arguments = self.function_ty.arguments.to_string();
        let expected_return_type = self.function_ty.expected_return_type.as_deref().unwrap_or(&ObjectTy::Unknown).to_string();
        write!(f, "{self_ty}::{method_name}({arguments}) -> {expected_return_type}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionTy {
    pub arguments: ArgumentList,
    /// The type of the return value of the function
    pub expected_return_type: Option<Box<ObjectTy>>,
}

impl Display for FunctionTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arguments = self.arguments.to_string();
        let expected_return_type = self.expected_return_type.as_deref().unwrap_or(&ObjectTy::Unknown).to_string();
        write!(f, "fn({arguments}) -> {expected_return_type}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Argument {
    pub span: Option<Span>,
    pub value: Object,
}

impl Deref for Argument {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl Argument {
    pub(crate) fn from_obj(obj: Object) -> Argument {
        Argument { span: None, value: obj }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinFunctionTy {
    pub name: &'static str,
    pub function_ty: FunctionTy,
}


use crate::evaluator::objects::function::{Callable};
use crate::Span;
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
    pub(crate) fn get_type(&self) -> ObjectTy {
        match self {
            Object::Integer(_) => ObjectTy::Integer,
            Object::Float(_) => ObjectTy::Float,
            Object::Boolean(_) => ObjectTy::Boolean,
            Object::String(_) => ObjectTy::String,
            Object::Array(_) => ObjectTy::Array { expected_item_types: None },
            Object::Range(from, to) => ObjectTy::Range { from: Box::new(from.get_type()), to: Box::new(to.get_type()) },
            Object::RangeInclusive(_, _) => ObjectTy::RangeInclusive { from: Box::new(ObjectTy::Any), to: Box::new(ObjectTy::Any) },
            Object::RangeFrom(from) => ObjectTy::RangeFrom { from: Box::new(from.get_type()) },
            Object::RangeTo(to) => ObjectTy::RangeTo { to: Box::new(to.get_type()) },
            Object::RangeToInclusive(to) => ObjectTy::RangeToInclusive { to: Box::new(to.get_type()) },
            Object::RangeFull => ObjectTy::RangeFull,
            Object::Iterator(_) => ObjectTy::Iterator { item: Box::new(ObjectTy::Any) },
            Object::Function(function) => ObjectTy::Function {
                function_ty: FunctionTy {
                    arguments: function.argument_list().unwrap_or(ArgumentList::Unknown),
                    expected_return_type: None,
                },
            },
            Object::Break(_) => ObjectTy::Any,
            Object::Continue => ObjectTy::Any,
            Object::Method(method) => ObjectTy::Method { method_ty: method.method_ty.clone() },
            Object::Reference(_) => ObjectTy::Reference,
            Object::Struct(_) => ObjectTy::Struct { expected_field_type: None },
            Object::Unit => ObjectTy::Unit,
            Object::ReturnValue(_) => ObjectTy::Any,
            Object::BuiltinFunction(builtin) => ObjectTy::Function {
                function_ty: builtin.ty.function_ty.clone(),
            },
        }
    }
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
    
    pub fn as_range_to_inclusive(&self) -> Option<&Object> {
        match self {
            Object::RangeToInclusive(to) => Some(to),
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
            (Object::Float(a), Object::Integer(b)) => a.partial_cmp(&(*b as f64)),
            (Object::Integer(a), Object::Float(b)) => a.partial_cmp(&(*b as i64)),
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
            Object::BuiltinFunction(builtin) => write!(f, "builtin function {}", builtin.name()),
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
                    .collect::<Vec<String>>()
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
