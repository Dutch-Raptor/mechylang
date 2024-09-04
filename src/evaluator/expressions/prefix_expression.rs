use std::rc::Rc;
use crate::{Environment, Evaluator, Object, Span, trace};
use crate::parser::expressions::{Expression, PrefixExpression, PrefixOperator};
use crate::evaluator::{Result, Error};

impl Evaluator {

    pub(super) fn eval_prefix_expression(&mut self, env: &mut Environment, prefix: &PrefixExpression) -> Result<Object> {
        let right = self.eval_expression(&prefix.right, env)?;
        self.current_span = prefix.span.clone();
        let span = prefix.span.clone();

        Ok(match prefix.operator {
            PrefixOperator::Bang => self.eval_bang_operator_expression(right, span),
            PrefixOperator::Minus => self.eval_minus_prefix_operator_expression(right, span)?,
            PrefixOperator::BitwiseNot => {
                self.eval_bitwise_not_operator_expression(right, span)?
            }
            PrefixOperator::Ampersand => {
                self.eval_reference_operator_expression(prefix.right.clone(), env, span)?
            }
            PrefixOperator::Asterisk => {
                self.eval_dereference_operator_expression(right, span)?
            }
        })
    }

    fn invalid_prefix_operator_for_type(operator: PrefixOperator, right: Object, span: Span) -> Box<Error> {
        Error::InvalidPrefixOperatorForType {
            span,
            operator,
            right,
        }.into()
    }

    fn eval_bang_operator_expression(&self, right: Object, _span: Span) -> Object {
        let _trace = trace!(&format!("eval_bang_operator_expression({})", right).to_string());
        match right {
            Object::Boolean(boolean) => (!boolean).into(),
            Object::Unit => true.into(),
            _ => false.into(),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object, span: Span) -> Result<Object> {
        let _trace = trace!(&format!("eval_minus_prefix_operator_expression({})", right));
        match right {
            Object::Integer(integer) => Ok((-integer).into()),
            Object::Float(float) => Ok((-float).into()),
            _ => Err(Self::invalid_prefix_operator_for_type(PrefixOperator::Minus, right, span)),
        }
    }
    

    fn eval_bitwise_not_operator_expression(&self, right: Object, span: Span) -> Result<Object> {
        match right {
            Object::Integer(integer) => Ok(Object::Integer(!integer)),
            _ => Err(Self::invalid_prefix_operator_for_type(PrefixOperator::BitwiseNot, right, span)),
        }
    }

    fn eval_reference_operator_expression(
        &self,
        right: Rc<Expression>,
        _env: &mut Environment,
        span: Span,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_reference_operator_expression: {}", right));
        todo!()
    }

    fn eval_dereference_operator_expression(&self, right: Object, span: Span) -> Result<Object> {
        let _trace = trace!(&format!(
            "eval_dereference_operator_expression: {:?}",
            right
        ));
        todo!()
    }
}