use crate::{Environment, Evaluator, Object, trace};
use crate::parser::expressions::{Expression, ExpressionSpanExt, Identifier, IndexExpression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator};
use crate::evaluator::{Result, Error};
use crate::evaluator::objects::ObjectTy;

impl Evaluator {
    pub(super) fn eval_assignment_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_assignment_expression: {}", infix));
        // set current token to the identifier token

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
                        span: infix.span.clone(),
                        operator_span: infix.operator_span.clone(),
                    },
                    env,
                )?
            }
        };

        self.current_span = infix.left.span().clone();

        match infix.left.as_ref() {
            Expression::Identifier(ident) => Self::update_identifier(env, new_value, ident)
                .map_err(|_| Self::identifier_not_found_error(ident, env))?,
            Expression::Index(index_expr) => self.assign_index_expression(infix, env, new_value, &index_expr)?,
            Expression::Prefix(PrefixExpression {
                                   span: _,
                                   right_span: _right_span, operator_span: _operator_span, operator: PrefixOperator::Asterisk,
                                   right: _,
                               }) => {
                todo!("Dereference operator not implemented yet");
            }

            _ => {}
        }

        Ok(Object::Unit)
    }

    fn assign_index_expression(&mut self, infix: &InfixExpression, env: &mut Environment, new_value: Object, index_expr: &&IndexExpression) -> Result<()> {
        let index = self.eval_expression(&index_expr.index, env)?;

        match index_expr.left.as_ref() {
            Expression::Identifier(ident) => {
                env.mutate(ident.clone(), |obj: &mut Object| {
                    let array = match obj {
                        Object::Array(ref mut arr) => arr,
                        _ => return Err(Error::IndexingNonIndexableType {
                            indexed_span: index_expr.left.span().clone(),
                            indexed_obj: obj.clone(),
                        }.into()),
                    };

                    let index = index.as_integer()
                        .ok_or_else(|| Error::TypeError {
                            expected: vec![ObjectTy::Integer],
                            found: index.get_type(),
                            span: index_expr.index.span().clone(),
                            context: Some(index_expr.span.clone()),
                        })? as usize;

                    if let Some(item) = array.get_mut(index) {
                        *item = new_value.clone();
                        Ok(Object::Unit)
                    } else {
                        Err(Error::IndexOutOfBounds {
                            array_span: index_expr.left.span().clone(),
                            index_span: index_expr.index.span().clone(),
                            index,
                            length: array.len(),
                        }.into())
                    }
                })?;
            }
            _ => {
                return Err(Error::InvalidIndexedAssignmentExpression {
                    span: infix.span.clone(),
                    left: index_expr.left.clone(),
                }.into())
            }
        };

        Ok(())
    }

    fn update_identifier(env: &mut Environment, new_value: Object, ident: &Identifier) -> Result<()> {
        env.update(ident.value.clone(), new_value).map_err(|_| {
            Evaluator::identifier_not_found_error(ident, env)
        })?;
        Ok(())
    }
}