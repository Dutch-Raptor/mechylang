use std::fmt::Debug;
use ariadne::Source;
use crate::{Evaluator, Object};
use crate::pretty_errors::PrettyError;

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
            println!("error: {}", error);
            let stdout = std::io::stdout();
            let mut handle = stdout.lock();
            error.as_pretty_errors("test_eval").write(("test_eval", Source::from(input)), &mut handle).unwrap();
        })
        .map_err(|error| {
            match error {
                crate::Error::EvaluatorError(error) => error,
                _ => panic!("Expected an evaluator error, got {:?}", error),
            }
        })
}
