use crate::{Environment,Evaluator, Object, trace};
use crate::evaluator::{Result, Error};
use crate::evaluator::objects::iterators::IteratorObject;
use crate::parser::expressions::{ExpressionSpanExt, ForExpression, IfExpression, WhileExpression};

impl Evaluator {
    pub(super) fn eval_if_expression(
        &mut self,
        if_expr: &IfExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let condition = self.eval_expression(&if_expr.condition, env)?;

        if Self::is_truthy(&condition) {
            self.eval_scoped_block_expression(&if_expr.consequence, env)
        } else if let Some(alternative) = &if_expr.alternative {
            self.eval_scoped_block_expression(alternative, env)
        } else {
            Ok(Object::Unit)
        }
    }


    pub(super) fn eval_for_expression(
        &mut self,
        for_expr: &ForExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_for_expression: {}", for_expr));

        let mut result = Object::Unit;

        let iterable = self.eval_expression(&for_expr.iterable, env)?;

        let iterator = IteratorObject::try_from(iterable).map_err(|err| {
            Error::IteratingOverNonIterable {
                span: for_expr.iterable.span().clone(),
                reason: err,
            }
        })?;

        for (index, item) in iterator.enumerate() {
            let mut new_env = Environment::new_enclosed(env);

            new_env.set(for_expr.iterator.value.clone(), item.clone());

            if let Some(ref index_ident) = for_expr.index {
                new_env.set(index_ident.value.clone(), Object::Integer(index as i64));
            }

            result = self.eval_block_expression(&for_expr.body, &mut new_env)?;

            // Return early if the result is a return or break
            match result {
                Object::ReturnValue(_) => return Ok(result),
                Object::Break(Some(value)) => return Ok(*value),
                Object::Break(None) => return Ok(Object::Unit),
                // continue is handled in eval_block_statement
                _ => {}
            }
        }

        if let Some(ref else_block) = for_expr.else_block {
            result = self.eval_block_expression(else_block, env)?;
        }

        Ok(result)
    }


    pub(super) fn eval_while_expression(
        &mut self,
        while_expr: &WhileExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_while_expression: {}", while_expr));

        let mut result = Object::Unit;

        while Self::is_truthy(&self.eval_expression(&while_expr.condition, env)?) {
            let mut new_env = Environment::new_enclosed(env);

            result = self.eval_block_expression(&while_expr.body, &mut new_env)?;

            // Return early if the result is a return or break
            match result {
                Object::ReturnValue(_) => return Ok(result),
                Object::Break(Some(value)) => return Ok(*value),
                Object::Break(None) => return Ok(Object::Unit),
                // continue is handled in eval_block_statement
                _ => {}
            }
        }

        if let Some(ref else_block) = while_expr.else_block {
            result = self.eval_block_expression(else_block, env)?;
        }

        Ok(result)
    }
}