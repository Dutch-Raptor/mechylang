use crate::{Environment, Error, Evaluator, Object};
use crate::parser::expressions::BlockExpression;

impl Evaluator {
    pub(super) fn eval_scoped_block_expression(
        &mut self,
        block: &BlockExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
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
    ) -> Result<Object, Error> {
        // evaluate the initial pass of the block
        self.eval_initial_pass(&block.statements, env);

        self.eval_block_statements(block, env)
    }

    pub(super) fn eval_block_statements(&mut self, block: &BlockExpression, env: &mut Environment) ->  Result<Object, Error> {
        let mut result = Object::Unit;

        for statement in block.statements.iter() {
            result = self.eval_statement(statement, env)?;

            // if the result is a return value, bubble it up and stop evaluating the block
            if let Object::ReturnValue(_) = result {
                return Ok(result);
            }

            // if the result is a break, bubble it up and stop evaluating the block
            if let Object::Break(_) = result {
                return Ok(result);
            }

            // if the result is a continue, bubble it up and stop evaluating the block
            if let Object::Continue = result {
                return Ok(result);
            }
        }

        Ok(result)
    }
}