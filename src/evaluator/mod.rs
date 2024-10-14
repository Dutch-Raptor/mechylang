use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluator::objects::function::Function;

mod eval_tests;
mod methods;
mod objects;
pub mod runtime;
mod config;
mod statements;
mod expressions;
mod error;
#[cfg(test)]
mod tests;

pub use config::EvalConfig;
pub use objects::{Object, iterators::IntoIteratorError};
pub use runtime::{Environment};
pub use error::{Error, Result};
use crate::{Lexer, Parser, Program, Span, Statement, trace};

pub struct Evaluator {
    current_span: Span,
    globals: HashMap<Rc<str>, Object>,
    eval_config: Rc<EvalConfig>,
}

impl Evaluator {
    pub fn eval(
        input: &str,
        env: &mut Environment,
        config: EvalConfig,
    ) -> crate::Result<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let Program { statements } = parser.parse()?;

        let evaluator = Evaluator {
            current_span: Span::default(),
            globals: HashMap::new(),
            eval_config: config.into(),
        };

        Ok(evaluator.eval_program(statements, env)?)
    }


    pub fn eval_program(
        mut self,
        program: Vec<Statement>,
        env: &mut Environment,
    ) -> Result<Object> {
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
                    span: function.span.clone(),
                    params: function.parameters.clone(),
                    body: function.body.clone(),
                    env: env.clone(),
                };
                env.set(function.name.value.clone(), Object::Function(func));
            }
        }
    }
}
