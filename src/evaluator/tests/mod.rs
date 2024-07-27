use crate::Evaluator;
use crate::evaluator::EvalResult;

mod integers;
mod booleans;
mod infix;
mod prefix;
mod float;
mod if_else;
mod blocks;
mod scoping;

pub(super) fn test_eval(input: &str) -> EvalResult {
    Evaluator::eval(input, &mut Default::default(), Default::default())
        .inspect_err(|errors| {
            for error in errors.iter() {
                println!("{}", error);
            }
        })
}
