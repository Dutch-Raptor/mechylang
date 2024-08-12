use crate::{Environment, Error, Evaluator, Object, trace};
use crate::error::ErrorKind;
use crate::parser::expressions::{Expression, ExpressionSpanExt, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator};

impl Evaluator {
    pub(super) fn eval_assignment_expression(
        &mut self,
        infix: &InfixExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_assignment_expression: {}", infix));
        // set current token to the identifier token
        self.current_span = infix.left.span().clone();

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
                    },
                    env,
                )?
            }
        };

        match infix.left.as_ref() {
            Expression::Identifier(ident) => {
                env.update(ident.value.clone(), new_value).map_err(|_| {
                    self.error(
                        infix.left.span().clone(),
                        &format!("Identifier {} not found", ident.value).to_string(),
                        ErrorKind::IdentifierNotFound,
                    )
                })?;
            }
            Expression::Index(index_expr) => {
                let index = self.eval_expression(&index_expr.index, env)?;

                let mutate_fn = |ident: String| {
                    move |obj: &mut Object| {
                        let array = match obj {
                            Object::Array(arr) => arr,
                            _ => {
                                return Err(format!("Cannot index non-array: {:?}", obj).to_string())
                            }
                        };

                        let index = match index {
                            Object::Integer(i) => i as usize,
                            _ => {
                                return Err(
                                    format!("Cannot index array with {:?}", index).to_string()
                                )
                            }
                        };

                        array
                            .get_mut(index)
                            .map(|item| {
                                *item = new_value.clone();
                                Ok(Object::Unit)
                            })
                            .unwrap_or_else(|| {
                                Err(format!(
                                    "Index out of bounds: {}, {} has len({})",
                                    index,
                                    ident,
                                    array.len()
                                ))
                            })
                    }
                };

                match index_expr.left.as_ref() {
                    Expression::Identifier(ident) => {
                        env.mutate(ident.clone(), mutate_fn(ident.value.to_string()))
                    }
                    _ => {
                        return Err(self.error(
                            infix.span.clone(),
                            &format!("Cannot index non-identifier: {:?}", index_expr.left)
                                .to_string(),
                            ErrorKind::MutateError,
                        ))
                    }
                }
                    .map_err(|err| {
                        self.error(
                            infix.span.clone(),
                            &format!("Error mutating variable: {}", err).to_string(),
                            ErrorKind::MutateError,
                        )
                    })?;
            }

            Expression::Prefix(PrefixExpression {
                                   span: _,
                                   operator: PrefixOperator::Asterisk,
                                   right: _,
                               }) => {
                todo!("Dereference operator not implemented yet");
                // let mut reference = match self.eval_expression(right, env)? {
                //     Object::Reference(reference) => reference,
                //     _ => {
                //         return Err(self.error(
                //             Some(token),
                //             &format!("Cannot dereference non-reference: {:?}", right).to_string(),
                //             ErrorKind::InvalidDereference,
                //         ))
                //     }
                // };
                //
                // reference.update(new_value).map_err(|err| {
                //     self.error(
                //         Some(&infix.token),
                //         &format!("Error mutating variable: {}", err).to_string(),
                //         ErrorKind::MutateError,
                //     )
                // })?;
            }

            _ => {}
        }

        Ok(Object::Unit)
    }
}