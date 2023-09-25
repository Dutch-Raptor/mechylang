use self::eval::Evaluator;

pub mod builtins;
pub mod environment;
pub mod eval;
/// This module contains the evaluator for the language.
/// It takes the AST and evaluates it.
pub mod objects;

pub fn eval_file(file: &str) {
    let input = std::fs::read_to_string(file).unwrap();
    let mut env = environment::Environment::new();
    match Evaluator::eval(input, &mut env) {
        Ok(_) => {}
        Err(errors) => {
            for error in errors.iter() {
                color_print::cprintln!("{}", error);
            }
        }
    }
}
