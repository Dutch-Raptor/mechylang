use std::rc::Rc;
use crate::{Environment, Evaluator, Object,  trace};
use crate::parser::expressions::{ExpressionSpanExt, InfixExpression, InfixOperator};
use crate::evaluator::{Result, Error};

impl Evaluator {
    pub(super) fn eval_infix_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("Evaluating infix expression: {}", infix).to_string());
        // check if the infix operator is an assignment operator
        if infix.operator.is_assignment() {
            return self.eval_assignment_expression(infix, env);
        }

        let left = self.eval_expression(&infix.left, env)?;
        let right = self.eval_expression(&infix.right, env)?;

        let left_span = infix.left.span().clone();
        let right_span = infix.right.span().clone();
        let operator_span = infix.operator_span.clone();

        self.current_span = infix.span.clone();

        match (&left, &right) {
            (Object::Unit, _) | (_, Object::Unit) => {
                self.eval_unit_infix_expression(&infix.operator, &left, &right)
            }
            (Object::Integer(left), Object::Integer(right)) => {
                self.eval_integer_infix_expression(&infix.operator, *left, *right)
            }
            (Object::Float(left), Object::Float(right)) => {
                self.eval_float_infix_expression(&infix.operator, *left, *right)
            }
            (Object::Float(left), Object::Integer(right)) => {
                self.eval_float_infix_expression(&infix.operator, *left, *right as f64)
            }
            (Object::Integer(left), Object::Float(right)) => {
                self.eval_float_infix_expression(&infix.operator, *left as f64, *right)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.eval_boolean_infix_expression(&infix.operator, *left, *right)
            }
            (Object::String(left), Object::String(right)) => {
                self.eval_string_infix_expression(&infix.operator, left.clone(), right.clone())
            }
            _ => None
        }.ok_or_else(|| Error::UnsupportedInfixOperator {
            left_span,
            right_span,
            left_object: left.clone(),
            right_object: right.clone(),
            operator: infix.operator,
            operator_span,
        }.into())
    }


    fn eval_unit_infix_expression(
        &self,
        operator: &InfixOperator,
        left: &Object,
        right: &Object,
    ) -> Option<Object> {
        match operator {
            InfixOperator::CompareNotEqual => Some(Object::Boolean(left != right)),
            InfixOperator::CompareEqual => Some(Object::Boolean(left == right)),
            _ => None,
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &InfixOperator,
        left: i64,
        right: i64,
    ) -> Option<Object> {
        match operator {
            InfixOperator::Plus => Some((left + right).into()),
            InfixOperator::Minus => Some((left - right).into()),
            InfixOperator::Asterisk => Some((left * right).into()),
            InfixOperator::Slash => Some((left / right).into()),
            InfixOperator::Percent => Some((left % right).into()),

            InfixOperator::CompareEqual => Some((left == right).into()),
            InfixOperator::CompareNotEqual => Some((left != right).into()),
            InfixOperator::CompareGreater => Some((left > right).into()),
            InfixOperator::CompareLess => Some((left < right).into()),
            InfixOperator::CompareGreaterEqual => Some((left >= right).into()),
            InfixOperator::CompareLessEqual => Some((left <= right).into()),

            InfixOperator::BitwiseOr => Some((left | right).into()),
            InfixOperator::BitwiseAnd => Some((left & right).into()),
            InfixOperator::BitwiseXor => Some((left ^ right).into()),
            InfixOperator::BitwiseLeftShift => Some((left << right).into()),
            InfixOperator::BitwiseRightShift => Some((left >> right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::LogicalAnd | InfixOperator::LogicalOr | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus | InfixOperator::AssignMinus | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd | InfixOperator::AssignBitwiseXor
            => None,
        }
    }


    fn eval_float_infix_expression(
        &self,
        operator: &InfixOperator,
        left: f64,
        right: f64,
    ) -> Option<Object> {
        match operator {
            InfixOperator::Plus => Some((left + right).into()),
            InfixOperator::Minus => Some((left - right).into()),
            InfixOperator::Asterisk => Some((left * right).into()),
            InfixOperator::Slash => Some((left / right).into()),
            InfixOperator::Percent => Some((left % right).into()),
            InfixOperator::CompareEqual => Some((left == right).into()),
            InfixOperator::CompareNotEqual => Some((left != right).into()),
            InfixOperator::CompareGreater => Some((left > right).into()),
            InfixOperator::CompareLess => Some((left < right).into()),
            InfixOperator::CompareGreaterEqual => Some((left >= right).into()),
            InfixOperator::CompareLessEqual => Some((left <= right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::LogicalAnd | InfixOperator::LogicalOr | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseAnd | InfixOperator::BitwiseXor | InfixOperator::BitwiseLeftShift
            | InfixOperator::BitwiseRightShift | InfixOperator::AssignEqual | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus | InfixOperator::AssignAsterisk | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseOr | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => None,
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &InfixOperator,
        left: bool,
        right: bool,
    ) -> Option<Object> {
        match operator {
            InfixOperator::CompareEqual => Some((left == right).into()),
            InfixOperator::CompareNotEqual => Some((left != right).into()),
            InfixOperator::LogicalAnd => Some((left && right).into()),
            InfixOperator::LogicalOr => Some((left || right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::Plus | InfixOperator::Minus | InfixOperator::Asterisk
            | InfixOperator::Slash | InfixOperator::Percent | InfixOperator::CompareGreater
            | InfixOperator::CompareLess | InfixOperator::CompareGreaterEqual | InfixOperator::CompareLessEqual
            | InfixOperator::BitwiseAnd | InfixOperator::BitwiseOr | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseLeftShift | InfixOperator::BitwiseRightShift | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus | InfixOperator::AssignMinus | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd | InfixOperator::AssignBitwiseXor
            => None,
        }
    }

    fn eval_string_infix_expression(
        &self,
        operator: &InfixOperator,
        left: Rc<str>,
        right: Rc<str>,
    ) -> Option<Object> {
        let _trace = trace!(&format!(
            "eval_string_infix_expression({:?}, {}, {})",
            operator, left, right
        ));
        match operator {
            InfixOperator::Plus => Some((left.to_string() + &right).into()),
            InfixOperator::CompareEqual => Some((left == right).into()),
            InfixOperator::CompareNotEqual => Some((left != right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::Minus | InfixOperator::Asterisk | InfixOperator::Slash
            | InfixOperator::Percent | InfixOperator::CompareGreater | InfixOperator::CompareLess
            | InfixOperator::CompareGreaterEqual | InfixOperator::CompareLessEqual | InfixOperator::LogicalAnd
            | InfixOperator::LogicalOr | InfixOperator::BitwiseOr | InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseXor | InfixOperator::BitwiseLeftShift | InfixOperator::BitwiseRightShift
            | InfixOperator::AssignEqual | InfixOperator::AssignPlus | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk | InfixOperator::AssignSlash | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr | InfixOperator::AssignBitwiseAnd | InfixOperator::AssignBitwiseXor
            => None,
        }
    }
}