use std::rc::Rc;
use crate::{Environment, Error, Evaluator, Object, trace};
use crate::errors::ErrorKind;
use crate::parser::expressions::{InfixExpression, InfixOperator};

impl Evaluator {
    pub(super) fn eval_infix_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("Evaluating infix expression: {}", infix).to_string());
        // check if the infix operator is an assignment operator
        if infix.operator.is_assignment() {
            return self.eval_assignment_expression(infix, env);
        }

        let left = self.eval_expression(&infix.left, env)?;
        let right = self.eval_expression(&infix.right, env)?;

        self.current_token = Some(infix.token.clone());

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
            _ => Err(self.error(
                self.current_token.as_ref(),
                format!("Type mismatch: {:?} {} {:?}", left, infix.operator, right).as_str(),
                ErrorKind::TypeMismatch,
            )),
        }
    }


    fn eval_unit_infix_expression(
        &self,
        operator: &InfixOperator,
        left: &Object,
        right: &Object,
    ) -> Result<Object, Error> {
        match operator {
            InfixOperator::CompareNotEqual => Ok(Object::Boolean(left != right)),
            InfixOperator::CompareEqual => Ok(Object::Boolean(left == right)),
            _ => Err(self.error(
                self.current_token.as_ref(),
                &format!("Invalid operator: {:?} {:?} {:?}", left, operator, right).to_string(),
                ErrorKind::InvalidOperator,
            )),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &InfixOperator,
        left: i64,
        right: i64,
    ) -> Result<Object, Error> {
        let invalid = || {
            self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: Integer({:?}) {} Integer({:?})",
                    left, operator, right
                )
                    .as_str(),
                ErrorKind::InvalidOperator,
            )
        };

        match operator {
            InfixOperator::Plus => Ok((left + right).into()),
            InfixOperator::Minus => Ok((left - right).into()),
            InfixOperator::Asterisk => Ok((left * right).into()),
            InfixOperator::Slash => Ok((left / right).into()),
            InfixOperator::Percent => Ok((left % right).into()),
            
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),
            InfixOperator::CompareGreater => Ok((left > right).into()),
            InfixOperator::CompareLess => Ok((left < right).into()),
            InfixOperator::CompareGreaterEqual => Ok((left >= right).into()),
            InfixOperator::CompareLessEqual => Ok((left <= right).into()),

            InfixOperator::BitwiseOr => Ok((left | right).into()),
            InfixOperator::BitwiseAnd => Ok((left & right).into()),
            InfixOperator::BitwiseXor => Ok((left ^ right).into()),
            InfixOperator::BitwiseLeftShift => Ok((left << right).into()),
            InfixOperator::BitwiseRightShift => Ok((left >> right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::LogicalAnd | InfixOperator::LogicalOr | InfixOperator::AssignEqual 
            | InfixOperator::AssignPlus | InfixOperator::AssignMinus | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd | InfixOperator::AssignBitwiseXor 
            => Err(invalid()),
        }
    }


    fn eval_float_infix_expression(
        &self,
        operator: &InfixOperator,
        left: f64,
        right: f64,
    ) -> Result<Object, Error> {
        let invalid = || {
            self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: Float({:?}) {} Float({:?})",
                    left, operator, right
                )
                    .as_str(),
                ErrorKind::InvalidOperator,
            )
        };

        match operator {
            InfixOperator::Plus => Ok((left + right).into()),
            InfixOperator::Minus => Ok((left - right).into()),
            InfixOperator::Asterisk => Ok((left * right).into()),
            InfixOperator::Slash => Ok((left / right).into()),
            InfixOperator::Percent => Ok((left % right).into()),
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),
            InfixOperator::CompareGreater => Ok((left > right).into()),
            InfixOperator::CompareLess => Ok((left < right).into()),
            InfixOperator::CompareGreaterEqual => Ok((left >= right).into()),
            InfixOperator::CompareLessEqual => Ok((left <= right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::LogicalAnd | InfixOperator::LogicalOr | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseAnd | InfixOperator::BitwiseXor | InfixOperator::BitwiseLeftShift
            | InfixOperator::BitwiseRightShift | InfixOperator::AssignEqual | InfixOperator::AssignPlus
            | InfixOperator::AssignMinus | InfixOperator::AssignAsterisk | InfixOperator::AssignSlash
            | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseOr | InfixOperator::AssignBitwiseAnd
            | InfixOperator::AssignBitwiseXor => Err(invalid()),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &InfixOperator,
        left: bool,
        right: bool,
    ) -> Result<Object, Error> {
        let invalid = || {
            self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: Boolean({:?}) {} Boolean({:?})",
                    left, operator, right
                )
                    .as_str(),
                ErrorKind::InvalidOperator,
            )
        };

        match operator {
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),
            InfixOperator::LogicalAnd => Ok((left && right).into()),
            InfixOperator::LogicalOr => Ok((left || right).into()),
            
            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::Plus | InfixOperator::Minus | InfixOperator::Asterisk
            | InfixOperator::Slash | InfixOperator::Percent | InfixOperator::CompareGreater
            | InfixOperator::CompareLess | InfixOperator::CompareGreaterEqual | InfixOperator::CompareLessEqual
            | InfixOperator::BitwiseAnd | InfixOperator::BitwiseOr | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseLeftShift | InfixOperator::BitwiseRightShift | InfixOperator::AssignEqual
            | InfixOperator::AssignPlus | InfixOperator::AssignMinus | InfixOperator::AssignAsterisk
            | InfixOperator::AssignSlash | InfixOperator::AssignPercent | InfixOperator::AssignBitwiseOr
            | InfixOperator::AssignBitwiseAnd | InfixOperator::AssignBitwiseXor 
            => Err(invalid()),
        }
    }

    fn eval_string_infix_expression(
        &self,
        operator: &InfixOperator,
        left: Rc<str>,
        right: Rc<str>,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!(
            "eval_string_infix_expression({:?}, {}, {})",
            operator, left, right
        ));
        match operator {
            InfixOperator::Plus => Ok((left.to_string() + &right).into()),
            InfixOperator::CompareEqual => Ok((left == right).into()),
            InfixOperator::CompareNotEqual => Ok((left != right).into()),

            // Explicitly not supported. This ensures that we always handle all possible operators
            InfixOperator::Minus | InfixOperator::Asterisk | InfixOperator::Slash
            | InfixOperator::Percent | InfixOperator::CompareGreater | InfixOperator::CompareLess
            | InfixOperator::CompareGreaterEqual | InfixOperator::CompareLessEqual | InfixOperator::LogicalAnd
            | InfixOperator::LogicalOr | InfixOperator::BitwiseOr | InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseXor | InfixOperator::BitwiseLeftShift | InfixOperator::BitwiseRightShift
            | InfixOperator::AssignEqual | InfixOperator::AssignPlus | InfixOperator::AssignMinus
            | InfixOperator::AssignAsterisk | InfixOperator::AssignSlash | InfixOperator::AssignPercent
            | InfixOperator::AssignBitwiseOr | InfixOperator::AssignBitwiseAnd | InfixOperator::AssignBitwiseXor
            => Err(self.error(
                self.current_token.as_ref(),
                format!(
                    "Invalid operator: String({:?}) {} String({:?})",
                    left, operator, right
                )
                    .as_str(),
                ErrorKind::InvalidOperator,
            )),
        }
    }
}