use crate::{EvalConfig, Evaluator};

use self::runtime::environment::Environment;

pub mod eval;
pub mod methods;
pub mod objects;
/// This module contains the evaluator for the language.
/// It takes the AST and evaluates it.
pub mod properties;
pub mod runtime;

pub fn eval_file(file: &str) -> Result<(), Vec<String>> {
    let input = std::fs::read_to_string(file).unwrap();
    let mut env = Environment::new();
    Evaluator::eval(input, &mut env, EvalConfig::default())
        .map(|_| ())
        .map_err(|e| e.iter().map(|e| color_print::cformat!("{}", e)).collect())
}
