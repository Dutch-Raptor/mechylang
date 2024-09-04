use crate::{Evaluator, Object};

mod integers;
mod booleans;
mod infix;
mod prefix;
mod float;
mod if_else;
mod blocks;
mod scoping;

pub(super) fn test_eval(input: &str) -> crate::evaluator::Result<Object> {
    Evaluator::eval(input, &mut Default::default(), Default::default())
        .inspect_err(|error| {
            println!("error: {:?}", error);
        })
        .map_err(|error| {
            match error {
                crate::Error::EvaluatorError(error) => error,
                _ => panic!("Expected an evaluator error, got {:?}", error),
            }
        })
}
