use crate::{Environment, Evaluator, Object};
use crate::parser::expressions::{ArrayLiteral, ExpressionSpanExt, IndexExpression};
use crate::evaluator::{Result, Error};
use crate::evaluator::objects::ObjectTy;

impl Evaluator {
    pub(super) fn eval_array_expression(
        &mut self,
        array: &ArrayLiteral,
        env: &mut Environment,
    ) -> Result<Object> {
        let elements = array.elements
            .iter()
            .map(|element| self.eval_expression(element, env))
            .collect::<Result<Vec<Object>>>()?;

        Ok(Object::Array(elements))
    }


    pub(super) fn eval_index_expression(
        &mut self,
        index: &IndexExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let left = self.eval_expression(&index.left, env)?;
        let array = left.as_array().ok_or_else(|| Error::TypeError {
            span: index.span.clone(),
            expected: vec![ObjectTy::Array { expected_item_types: None }],
            found: left.get_type(),
        })?;

        let evaluated_index = self.eval_expression(&index.index, env)?;
        let evaluated_index = evaluated_index.as_integer().ok_or_else(|| Error::TypeError {
            span: index.span.clone(),
            expected: vec![ObjectTy::Integer],
            found: evaluated_index.get_type(),
        })? as usize;
        
        match array.get(evaluated_index) {
            Some(item) => Ok(item.clone()),
            None => Err(Error::IndexOutOfBounds {
                array_span: index.left.span().clone(),
                index_span: index.index.span().clone(),
                index: evaluated_index,
                length: array.len(),
            }.into()),
        }
    }
}