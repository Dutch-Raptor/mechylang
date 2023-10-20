use self::eval::{EvalConfig, Evaluator};

pub mod builtins;
pub mod environment;
pub mod eval;
pub mod iterators;
pub mod methods;
/// This module contains the evaluator for the language.
/// It takes the AST and evaluates it.
pub mod objects;

pub fn eval_file(file: &str) -> Result<(), Vec<String>> {
    let input = std::fs::read_to_string(file).unwrap();
    let mut env = environment::Environment::new();
    Evaluator::eval(input, &mut env, EvalConfig::default())
        .map(|_| ())
        .map_err(|e| e.iter().map(|e| color_print::cformat!("{}", e)).collect())
}
