use crate::{Environment, Error, Evaluator, Object};
use crate::error::ErrorKind;
use crate::parser::expressions::{ArrayLiteral, ExpressionSpanExt, IndexExpression};

impl Evaluator {
    pub(super) fn eval_array_expression(
        &mut self,
        array: &ArrayLiteral,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let elements = array.elements
            .iter()
            .map(|element| self.eval_expression(element, env))
            .collect::<Result<Vec<Object>, Error>>()?;

        Ok(Object::Array(elements))
    }


    pub(super) fn eval_index_expression(
        &mut self,
        index: &IndexExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let left = self.eval_expression(&index.left, env)?;

        let array = match left {
            Object::Array(ref arr) => arr,
            _ => {
                return Err(self.error(
                    index.left.span().clone(),
                    &format!("Expected an array. Got {:?}", left).to_string(),
                    ErrorKind::TypeError,
                ))
            }
        };

        let evaluated_index = self.eval_expression(&index.index, env)?;

        match evaluated_index {
            Object::Integer(i) => match array.get(i as usize) {
                Some(item) => Ok(item.clone()),
                None => Err(self.error(
                    index.index.span().clone(),
                    &format!(
                        "Index out of bounds: {}, {} has len({})",
                        i,
                        left,
                        array.len()
                    )
                        .to_string(),
                    ErrorKind::IndexOutOfBounds,
                )),
            },
            _ => {
                return Err(self.error(
                    index.index.span().clone(),
                    &format!(
                        "Index operator not supported for {:?}[{:?}]",
                        left, evaluated_index
                    )
                        .to_string(),
                    ErrorKind::IndexOperatorNotSupported,
                ))
            }
        }
    }
}