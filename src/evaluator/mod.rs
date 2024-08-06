use std::collections::HashMap;
use std::rc::Rc;
use crate::{Error, Lexer, Object, Parser, Token, trace};
use crate::errors::InterpreterErrors;
use crate::evaluator::objects::function::Function;
use crate::parser::program::Program;
use crate::parser::statements::Statement;
use self::runtime::environment::Environment;

pub mod eval_tests;
pub mod methods;
pub mod objects;
pub mod runtime;
mod config;
mod statements;
mod expressions;
mod errors;
#[cfg(test)]
mod tests;

pub use self::config::EvalConfig;

pub fn eval_file(file: &str) -> Result<(), Vec<String>> {
    let input = std::fs::read_to_string(file).unwrap();
    let mut env = Environment::new();
    Evaluator::eval(input, &mut env, EvalConfig::default())
        .map(|_| ())
        .map_err(|e| e.iter().map(|e| color_print::cformat!("{}", e)).collect())
}

pub struct Evaluator {
    lines: Rc<[String]>,
    current_token: Option<Token>,
    globals: HashMap<Rc<str>, Object>,
    eval_config: Rc<EvalConfig>,
}

pub type EvalResult = Result<Object, InterpreterErrors>;

impl Evaluator {
    pub fn eval(
        input: impl Into<Rc<str>>,
        env: &mut Environment,
        config: EvalConfig,
    ) -> EvalResult {
        let input: Rc<str> = input.into();
        let lexer = Lexer::new(input);
        let lines = lexer.lines();
        let mut parser = Parser::new(lexer);

        let Program { statements } = parser.parse()?;

        let evaluator = Evaluator {
            lines,
            current_token: None,
            globals: HashMap::new(),
            eval_config: config.into(),
        };

        let a = evaluator
            .eval_program(statements, env)
            .map_err(|err| InterpreterErrors(vec![err]));
        
        println!("{:?}", a);
        
        a
    }


    pub fn eval_program(
        mut self,
        program: Vec<Statement>,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!("eval_program");
        let mut result = Object::Unit;

        self.eval_initial_pass(&program, env);

        for statement in program.into_iter() {
            result = self.eval_statement(&statement, env)?;

            if let Object::ReturnValue(val) = result {
                return Ok(*val);
            }
        }

        Ok(result)
    }
   
    /// Performs an initial pass over the program to collect function definitions.
    ///
    /// This method iterates through the provided program's statements and looks for
    /// function definitions (`Statement::Function`). When a function definition is found,
    /// it creates a `Function` object and stores it in the given environment (`env`) with
    /// the function's name as the key. This allows functions to be registered in the environment
    /// before any other code is executed, enabling forward references to functions.
    ///
    /// # Arguments
    ///
    /// * `program` - A slice of `Statement` representing the program to evaluate.
    /// * `env` - A mutable reference to the `Environment` where functions will be registered.
    ///
    /// # Panics
    ///
    /// This method should not panic under normal circumstances. It relies on the assumption
    /// that the program and environment provided are valid and that the function definitions
    /// encountered have unique names that can be stored in the environment without conflicts.
    fn eval_initial_pass(
        &mut self,
        program: &[Statement],
        env: &mut Environment,
    ) {
        for statement in program.iter() {
            if let Statement::Function(function) = statement {
                let func = Function {
                    params: function.parameters.clone(),
                    body: function.body.clone(),
                    env: env.clone(),
                };
                env.set(function.name.value.clone(), Object::Function(func));
            }
        }
    }
}
