use std::ops::RangeInclusive;
use std::rc::Rc;
use crate::{Expression, Object, Span};
use crate::evaluator::objects::iterators::IntoIteratorError;
use crate::evaluator::objects::{Argument, ObjectTy};
use crate::evaluator::runtime::builtins::BuiltinError;
use crate::parser::expressions::{InfixOperator, PrefixOperator};

pub type Result<T> = std::result::Result<T, Box<Error>>;


#[derive(Debug, PartialEq)]
pub enum Error {
    LexerError(crate::lexer::Error),
    ParserError(crate::parser::Error),
    /// Error when trying to use an invalid prefix operator for the given type
    InvalidPrefixOperatorForType { span: Span, operator: PrefixOperator, right: Object, operator_span: Span, right_span: Span },
    /// Error when trying to find an identifier that does not exist
    IdentifierNotFound { span: Span, identifier: Rc<str>, similar: Option<Rc<str>> },
    /// Error when trying to assign to an indexed expression
    InvalidIndexedAssignmentExpression { span: Span, left: Rc<Expression> },
    /// Error when trying to mutate a value that is not mutable
    MutateError { span: Span, name: Rc<str> },
    /// Error when trying to use an unsupported infix operator for the given types (left and right)
    UnsupportedInfixOperator {
        left_span: Span,
        right_span: Span,
        left_object: Object,
        right_object: Object,
        operator: InfixOperator,
        operator_span: Span
    },
    /// Error when trying to iterate over a non-iterable
    IteratingOverNonIterable { 
        obj: Object, 
        obj_span: Span, 
        for_span: Span,
        reason: IntoIteratorError },
    /// Error when trying to call a non-function
    CannotCall { function: Object, span: Span },
    WrongNumberOfArguments { 
        span: Span, 
        unexpected_arg: Option<(usize, Argument)>,
        expected: RangeInclusive<usize>, found: usize },
    BuiltInError { span: Span, message: String, error_type: BuiltinError },
    IterCallOnNonIterable { 
        obj: Object,
        obj_span: Span, 
        iter_call_span: Span, reason: IntoIteratorError },
    TypeError { span: Span, expected: Vec<ObjectTy>, found: ObjectTy, context: Option<Span> },
    AssertionFailed { span: Span, assert_span: Span, value: Object },
    AssertionEqualFailed { assert_span: Span, first_span: Span, second_span: Span, first_value: Object, second_value: Object },
    IOError { error: IOError, span: Option<Span> },
    MutateNonExistentVariable { name: Rc<str> },
    IndexOutOfBounds { array_span: Span, index_span: Span, index: usize, length: usize },
    IteratorMethodOnIterable { method_span: Span, method_name: Rc<str>, object_type: ObjectTy, object_span: Span },
    PropertyNotFound { span: Span, property: Rc<str>, object_type: ObjectTy },
    IndexingNonIndexableType { indexed_span: Span, indexed_obj: Object },
}

#[derive(Debug)]
pub struct IOError {
    #[allow(unused)]
    cause: std::io::Error,
}

impl PartialEq for IOError {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<std::io::Error> for IOError {
    fn from(value: std::io::Error) -> Self {
        IOError { cause: value }
    }
}


impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
