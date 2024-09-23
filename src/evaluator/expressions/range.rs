use crate::{Environment,  Evaluator, Object, trace};
use crate::parser::expressions::{Expression};
use crate::evaluator::{Result};

impl Evaluator {
    pub(super) fn eval_range_expression(
        &mut self,
        expression: &Expression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_range_expression: {}", expression));

        Ok(match expression {
            Expression::Range(range) => {
                if range.inclusive {
                    Object::RangeInclusive(
                        self.eval_expression(&range.left, env)?.into(),
                        self.eval_expression(&range.right, env)?.into(),
                    )
                } else {
                    Object::Range(
                        self.eval_expression(&range.left, env)?.into(),
                        self.eval_expression(&range.right, env)?.into(),
                    )
                }
            }
            Expression::RangeTo(range) => {
                if range.inclusive {
                    Object::RangeToInclusive(self.eval_expression(&range.right, env)?.into())
                } else {
                    Object::RangeTo(self.eval_expression(&range.right, env)?.into())
                }
            }
            Expression::RangeFrom(range) => {
                Object::RangeFrom(self.eval_expression(&range.left, env)?.into())
            }
            Expression::RangeFull(_) => Object::RangeFull,
            _ => {
                panic!("Expected range expression, got {:?}", expression);
            }
        })
    }
}