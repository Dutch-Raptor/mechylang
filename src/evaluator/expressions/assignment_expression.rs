use crate::{Environment, Error, Evaluator, Object, trace};
use crate::errors::ErrorKind;
use crate::parser::expressions::{Expression, ExpressionToken, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator};

impl Evaluator {
    pub(super) fn eval_assignment_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_assignment_expression: {}", infix));
        // set current token to the identifier token
        self.current_token = Some(infix.left.token().clone());

        let new_value = {
            if infix.operator == InfixOperator::AssignEqual {
                self.eval_expression(&infix.right, env)?
            } else {
                self.eval_infix_expression(
                    &InfixExpression {
                        left: infix.left.clone(),
                        operator: infix.operator
                            .assignment_related_operator()
                            .expect("eval_assignment_expression to only be called for assignment operators"),
                        right: infix.right.clone(),
                        token: infix.token.clone(),
                    },
                    env,
                )?
            }
        };

        let obj_id = match infix.left.as_ref() {
            Expression::Identifier(ident) => {
                env.get_id(ident.value.clone()).ok_or_else(|| {
                    self.error(
                        Some(infix.left.token()),
                        &format!("Identifier {} not found", ident.value).to_string(),
                        ErrorKind::IdentifierNotFound,
                    )
                })?
            }
            
            Expression::Index(index_expr) =>
                self.eval_index_expression_object_id(index_expr, env)?,
            
            Expression::Prefix(
                PrefixExpression {
                    token: _,
                    operator: PrefixOperator::Asterisk,
                    right,
                }) => {
                *self.eval_expression(right, env)?.as_reference().ok_or_else(|| {
                    self.error(
                        Some(&infix.token),
                        &format!("Cannot dereference non-reference: {:?}", right).to_string(),
                        ErrorKind::InvalidDereference,
                    )
                })?
            }

            _ => return Err(self.error(Some(&infix.token), "Cannot index non-identifier", ErrorKind::InvalidIndex)),
        };
        
        env.set_by_id(obj_id, new_value);

        Ok(Object::Unit)
    }
}