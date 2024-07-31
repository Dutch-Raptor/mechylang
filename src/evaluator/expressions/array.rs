use crate::{Environment, Error, Evaluator, Object};
use crate::errors::ErrorKind;
use crate::evaluator::runtime::environment::ObjectId;
use crate::parser::expressions::{ArrayLiteral, ExpressionToken, IndexExpression};

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
        
        // store all the elements in the environment
        let element_ids = elements
            .iter()
            .map(|element| env.store_object(element.clone()))
            .collect::<Vec<_>>();
        
        Ok(Object::Array(element_ids))
    }
    
    pub(in crate::evaluator) fn eval_index_expression_object_id(
        &mut self,
        index: &IndexExpression,
        env: &mut Environment,
    ) -> Result<ObjectId, Error> {
        let left = self.eval_expression(&index.left, env)?;

        match &left {
            Object::Array(_) => self.eval_array_index_expression(index, env, &left),
            _ => Err(self.error(Some(index.left.token()), "Expected an array", ErrorKind::TypeError)),
        }
    }

    fn eval_array_index_expression(&mut self, index: &IndexExpression, env: &mut Environment, left: &Object) -> Result<ObjectId, Error> {
        let array = left.as_array().expect("Left should be an array in eval_array_index_expression");

        let evaluated_index = self.eval_expression(&index.index, env)?;

        match evaluated_index {
            Object::Integer(i) => match array.get(i as usize) {
                Some(id) => Ok(*id),
                None => Err(self.error(
                    Some(index.index.token()),
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
                    Some(index.index.token()),
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

    pub(super) fn eval_index_expression(
        &mut self,
        index: &IndexExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let object_id = self.eval_index_expression_object_id(index, env)?;
        
        Ok(env.get_by_id(object_id).expect("objectId in array should be valid"))
    }
}