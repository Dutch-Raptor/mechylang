use std::rc::Rc;
use crate::{Environment, Evaluator, Object, trace};
use crate::parser::expressions::{Expression, ExpressionSpanExt, PrefixExpression, PrefixOperator};
use crate::evaluator::{Result, Error};

impl Evaluator {

    pub(super) fn eval_prefix_expression(&mut self, env: &mut Environment, prefix: &PrefixExpression) -> Result<Object> {
        let right = self.eval_expression(&prefix.right, env)?;

        match prefix.operator {
            PrefixOperator::Bang => self.eval_bang_operator_expression(&right),
            PrefixOperator::Minus => self.eval_minus_prefix_operator_expression(&right),
            PrefixOperator::BitwiseNot => {
                self.eval_bitwise_not_operator_expression(&right)
            }
            PrefixOperator::Ampersand => {
                self.eval_reference_operator_expression(prefix.right.clone(), env)
            }
            PrefixOperator::Asterisk => {
                self.eval_dereference_operator_expression(&right)
            }
        }.ok_or_else(|| Box::new(Error::InvalidPrefixOperatorForType {
            span: prefix.span.clone(),
            operator_span: prefix.operator_span.clone(),
            right_span: prefix.right.span().clone(),
            operator: prefix.operator,
            right,
        }))
    }

    fn eval_bang_operator_expression(&self, right: &Object) -> Option<Object> {
        let _trace = trace!(&format!("eval_bang_operator_expression({})", right).to_string());
        match right {
            Object::Boolean(boolean) => Some((!boolean).into()),
            _ => None,
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: &Object) -> Option<Object> {
        let _trace = trace!(&format!("eval_minus_prefix_operator_expression({})", right));
        match right {
            Object::Integer(integer) => Some((-integer).into()),
            Object::Float(float) => Some((-float).into()),
            _ => None,
        }
    }
    

    fn eval_bitwise_not_operator_expression(&self, right: &Object) -> Option<Object> {
        match right {
            Object::Integer(integer) => Some(Object::Integer(!integer)),
            _ => None,
        }
    }

    fn eval_reference_operator_expression(
        &self,
        right: Rc<Expression>,
        _env: &mut Environment,
    ) -> Option<Object> {
        let _trace = trace!(&format!("eval_reference_operator_expression: {}", right));
        todo!()
    }

    fn eval_dereference_operator_expression(&self, right: &Object) -> Option<Object> {
        let _trace = trace!(&format!(
            "eval_dereference_operator_expression: {:?}",
            right
        ));
        todo!()
    }
}