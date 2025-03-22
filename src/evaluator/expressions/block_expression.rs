use crate::{Environment, Evaluator, Object};
use crate::parser::expressions::BlockExpression;
use crate::evaluator::{Result};

impl Evaluator {
    pub(super) fn eval_scoped_block_expression(
        &mut self,
        block: &BlockExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        // new scope, so create a new environment with the current environment as its parent
        let env = &mut Environment::new_enclosed(env);

        // evaluate the initial pass of the block
        self.eval_initial_pass(&block.statements, env);

        self.eval_block_statements(block, env)
    }

    pub(super) fn eval_block_expression(
        &mut self,
        block: &BlockExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        // evaluate the initial pass of the block
        self.eval_initial_pass(&block.statements, env);

        self.eval_block_statements(block, env)
    }

    pub(super) fn eval_block_statements(&mut self, block: &BlockExpression, env: &mut Environment) -> Result<Object> {
        let mut result = Object::Unit;

        for statement in block.statements.iter() {
            result = self.eval_statement(statement, env)?;
            
            // If the result is a return, break or continue, 
            // bubble it up and stop evaluating the block
            match result {
                Object::ReturnValue(_) |
                Object::Break(_) |
                Object::Continue => return Ok(result),
                _ => {}
            };
        }

        Ok(result)
    }
}