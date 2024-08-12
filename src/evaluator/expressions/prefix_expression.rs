use std::rc::Rc;
use crate::{Environment, Error, Evaluator, Object, trace};
use crate::error::ErrorKind;
use crate::parser::expressions::{Expression, PrefixExpression, PrefixOperator};

impl Evaluator {

    pub(super) fn eval_prefix_expression(&mut self, env: &mut Environment, prefix: &PrefixExpression) -> Result<Object, Error> {
        let right = self.eval_expression(&prefix.right, env)?;
        self.current_span = prefix.span.clone();

        Ok(match prefix.operator {
            PrefixOperator::Bang => self.eval_bang_operator_expression(right),
            PrefixOperator::Minus => self.eval_minus_prefix_operator_expression(right)?,
            PrefixOperator::BitwiseNot => {
                self.eval_bitwise_not_operator_expression(right)?
            }
            PrefixOperator::Ampersand => {
                self.eval_reference_operator_expression(prefix.right.clone(), env)?
            }
            PrefixOperator::Asterisk => {
                self.eval_dereference_operator_expression(right)?
            }
        })
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        let _trace = trace!(&format!("eval_bang_operator_expression({})", right).to_string());
        match right {
            Object::Boolean(boolean) => (!boolean).into(),
            Object::Unit => true.into(),
            _ => false.into(),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_minus_prefix_operator_expression({})", right));
        match right {
            Object::Integer(integer) => Ok((-integer).into()),
            Object::Float(float) => Ok((-float).into()),
            _ => Err(self.error(
                self.current_span.clone(),
                format!("Unknown operator: -{:?}", right).as_str(),
                ErrorKind::UnknownOperator,
            )),
        }
    }

    fn eval_bitwise_not_operator_expression(&self, right: Object) -> Result<Object, Error> {
        match right {
            Object::Integer(integer) => Ok(Object::Integer(!integer)),
            _ => Err(self.error(
                self.current_span.clone(),
                &format!("Invalid operator: ~{:?}", right).to_string(),
                ErrorKind::InvalidOperator,
            )),
        }
    }

    fn eval_reference_operator_expression(
        &self,
        right: Rc<Expression>,
        _env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_reference_operator_expression: {}", right));
        todo!()
    }

    fn eval_dereference_operator_expression(&self, right: Object) -> Result<Object, Error> {
        let _trace = trace!(&format!(
            "eval_dereference_operator_expression: {:?}",
            right
        ));
        todo!()
    }
}